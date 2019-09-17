defmodule Thrift.Binary.Framed.Client do
  @moduledoc """
  A client implementation of Thrift's Binary Framed protocol.

  This client is meant to be used with a generated Thrift client. This module
  implements framing on top of the `Connection` behaviour.

  This module adds `oneway/4` for making "oneway" RPC calls that won't receive
  a response.

  See `start_link/3` for the various connection options.

      {:ok, client} = Client.start_link(
          "localhost", 2345,
          tcp_opts: [],
          ssl_opts: [enabled: true, cacertfile: "cacerts.pem", certfile: "cert.pem", keyfile: "key.pem"],
          gen_server_opts: [timeout: 10_000])
  """

  alias Thrift.Protocol.Binary
  alias Thrift.TApplicationException
  alias Thrift.Transport.SSL

  @immutable_tcp_opts [active: true, packet: 4, mode: :binary]

  @type error :: {:error, atom} | {:error, {:exception, struct}}
  @type success :: {:ok, binary}

  @type protocol_response :: success | error

  @type tcp_option ::
          {:timeout, pos_integer}
          | {:send_timeout, integer}

  @type tcp_opts :: [tcp_option]

  @type genserver_call_option :: {:timeout, timeout}

  @type genserver_call_options :: [genserver_call_option]

  @type option ::
          {:tcp_opts, [tcp_option]}
          | {:ssl_opts, [SSL.option()]}
          | {:gen_server_opts, [genserver_call_option]}
          | {:reconnect, boolean}

  @type options :: [option]

  defmodule State do
    @moduledoc false

    @type t :: %State{
            host: String.t(),
            port: 1..65_535,
            tcp_opts: [Thrift.Binary.Framed.Client.tcp_option()],
            ssl_opts: [SSL.option()],
            timeout: integer,
            sock: {:gen_tcp, :gen_tcp.socket()} | {:ssl, :ssl.sslsocket()},
            seq_id: integer,
            reconnect: boolean
          }
    defstruct host: nil,
              port: nil,
              tcp_opts: nil,
              ssl_enabled: false,
              ssl_opts: nil,
              timeout: 5000,
              sock: nil,
              seq_id: 0,
              reconnect: false
  end

  require Logger
  use Connection

  @impl Connection
  def init({host, port, opts}) do
    tcp_opts = Keyword.get(opts, :tcp_opts, [])
    ssl_opts = Keyword.get(opts, :ssl_opts, [])
    reconnect = Keyword.get(opts, :reconnect, false)

    {timeout, tcp_opts} = Keyword.pop(tcp_opts, :timeout, 5000)

    s = %State{
      host: to_host(host),
      port: port,
      tcp_opts: tcp_opts,
      ssl_opts: ssl_opts,
      timeout: timeout,
      reconnect: reconnect
    }

    {:connect, :init, s}
  end

  @doc """
  Starts and connects the client.

  When called, the client connects to the specified host and establishes a
  TCP connection. The following options can be given:

  `tcp_opts`: A keyword list that controls how the underlying connection is
  handled. All options not handled below are sent to the underlying gen_tcp
  (with the exception of the following options, which, if overridden, would
  break the framed client: [`active`, `packet`, `mode`]).

    - `:timeout`: A positive integer (milliseconds) that governs how long the
      gen_tcp connection waits for operations to complete. This timeout is
      used when connecting or receiving data.
    - `:send_timeout`: A positive integer (milliseconds) that governs how long
      our connection waits when sending data.

  `ssl_opts`: A keyword list of SSL/TLS options:

    - `:enabled`: A boolean indicating whether to upgrade the connection to
      the SSL protocol
    - `:optional`: A boolean indicating whether to accept both SSL and plain
      connections
    - `:configure`: A 0-arity function to provide additional SSL options at
      runtime
    - Additional `:ssl.ssl_option/0` values specifying other `:ssl` options

  `gen_server_opts`: A keyword list of options for the GenServer:

    - `:timeout`: A positive integer (milliseconds) specifying the amount of
      time the client's GenServer should wait for a reply. After this time,
      the GenServer will exit with `{:error, :timeout}`.

  Additionally, the options `:name`, `:debug`, and `:spawn_opt`, if specified,
  will be passed to the underlying `GenServer`. See `GenServer.start_link/3`
  for details on these options.
  """
  @spec start_link(String.t(), 0..65_535, options) :: GenServer.on_start()
  def start_link(host, port, opts) do
    {gen_server_opts, client_opts} = Keyword.split(opts, [:debug, :name, :spawn_opt])
    Connection.start_link(__MODULE__, {host, port, client_opts}, gen_server_opts)
  end

  @doc """
  Closes the client's underlying connection.
  """
  def close(conn), do: Connection.call(conn, :close)

  @impl Connection
  def connect(info, %{sock: nil, host: host, port: port, tcp_opts: opts, timeout: timeout} = s) do
    opts =
      opts
      |> Keyword.merge(@immutable_tcp_opts)
      |> Keyword.put_new(:send_timeout, 1000)

    # reset sequence id for newly created connection
    s = %{s | seq_id: 0}

    case :gen_tcp.connect(host, port, opts, timeout) do
      {:ok, sock} ->
        sock
        |> maybe_ssl_handshake(host, port, s)
        |> maybe_resend_data(info)

      {:error, :timeout} = error ->
        Logger.error("Failed to connect to #{host}:#{port} due to timeout after #{timeout}ms")
        {:stop, error, s}

      {:error, reason} = error ->
        Logger.error("Failed to connect to #{host}:#{port} due to #{:inet.format_error(reason)}")
        {:stop, error, s}
    end
  end

  @impl Connection
  def disconnect(info, %{sock: {transport, sock}} = s) do
    :ok = transport.close(sock)

    case info do
      {:reconnect, _} ->
        {:connect, info, %{s | sock: nil}}

      {:close, from} ->
        Connection.reply(from, :ok)
        {:stop, :normal, nil}

      {:error, :closed} = error ->
        Logger.error("Connection closed")
        {:stop, error, nil}

      {:error, :timeout, timeout} ->
        Logger.error("Connection timed out after #{timeout}ms")
        {:stop, {:error, :timeout}, nil}

      {:error, reason} = error ->
        # :ssl formats ssl and posix errors
        reason = :ssl.format_error(reason)
        Logger.error("Connection error: #{reason}")
        {:stop, error, nil}
    end
  end

  @spec oneway(pid, String.t(), iodata, options) :: :ok
  @doc """
  Execute a one way RPC. One way RPC calls do not generate a response,
  and as such, this implementation uses `GenServer.cast`.
  The data argument must be a properly formatted Thrift message.
  """
  def oneway(conn, rpc_name, serialized_args, _opts) do
    :ok = Connection.cast(conn, {:oneway, rpc_name, serialized_args})
  end

  @spec call(pid, String.t(), iodata, module, options) :: protocol_response
  @doc """
  Executes a Thrift RPC. The data argument must be a correctly formatted
  Thrift message.

  The `opts` argument takes the same type of keyword list that `start_link` takes.
  """
  def call(conn, rpc_name, serialized_args, deserialize_module, opts) do
    tcp_opts = Keyword.get(opts, :tcp_opts, [])
    gen_server_opts = Keyword.get(opts, :gen_server_opts, [])
    gen_server_timeout = Keyword.get(gen_server_opts, :timeout, 5000)

    case Connection.call(conn, {:call, rpc_name, serialized_args, tcp_opts}, gen_server_timeout) do
      {:ok, data} ->
        data
        |> deserialize_module.deserialize
        |> unpack_response

      {:error, _} = err ->
        err
    end
  end

  # Unpack a "complex" response that contains a `nil` success value and one or
  # more exception fields. We can't differentiate between a "void" result and
  # a potential exception without checking each exception field to see if it
  # contains a non-`nil` value.
  #
  # As a small optimization, we only use this function if the response struct
  # has at least one exception field (hence the `map_size/1` guard check).
  defp unpack_response({%{success: nil} = response, ""}) when map_size(response) > 2 do
    exception =
      response
      |> Map.from_struct()
      |> Enum.find_value(fn {_, value} -> value end)

    if exception do
      {:error, {:exception, exception}}
    else
      # "void" function result
      {:ok, nil}
    end
  end

  defp unpack_response({%{success: result}, ""}), do: {:ok, result}
  defp unpack_response({:error, _} = error), do: error

  def handle_call(_, _, %{sock: nil} = s) do
    {:reply, {:error, :closed}, s}
  end

  def handle_call(
        {:call, rpc_name, serialized_args, tcp_opts} = msg,
        from,
        %{
          sock: {transport, sock},
          seq_id: seq_id,
          timeout: default_timeout,
          reconnect: reconnect
        } = s
      ) do
    s = %{s | seq_id: seq_id + 1}
    message = Binary.serialize(:message_begin, {:call, seq_id, rpc_name})
    timeout = Keyword.get(tcp_opts, :timeout, default_timeout)

    with :ok <- transport.send(sock, [message | serialized_args]),
         {:ok, message} <- receive_message(sock, timeout) do
      reply = deserialize_message_reply(message, rpc_name, seq_id)
      {:reply, reply, s}
    else
      {:error, :closed} = error ->
        if reconnect do
          {:disconnect, {:reconnect, {:call, msg, from}}, s}
        else
          {:disconnect, error, error, s}
        end

      {:error, :timeout} = error ->
        {:disconnect, {:error, :timeout, timeout}, error, s}

      {:error, _} = error ->
        {:disconnect, error, error, s}
    end
  end

  @impl Connection
  def handle_call(:close, from, s) do
    {:disconnect, {:close, from}, s}
  end

  @impl Connection
  def handle_cast(_, %{sock: nil} = s) do
    {:noreply, s}
  end

  def handle_cast(
        {:oneway, rpc_name, serialized_args} = msg,
        %{sock: {transport, sock}, seq_id: seq_id, reconnect: reconnect} = s
      ) do
    s = %{s | seq_id: seq_id + 1}
    message = Binary.serialize(:message_begin, {:oneway, seq_id, rpc_name})

    case transport.send(sock, [message | serialized_args]) do
      :ok ->
        {:noreply, s}

      {:error, :closed} = error ->
        if reconnect do
          {:disconnect, {:reconnect, {:cast, msg}}, s}
        else
          {:disconnect, error, s}
        end

      {:error, _} = error ->
        {:disconnect, error, s}
    end
  end

  @impl Connection
  def handle_info({:tcp_closed, sock}, %{reconnect: true, sock: {_transport, sock}} = s) do
    {:disconnect, {:reconnect, nil}, s}
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  def deserialize_message_reply(message, rpc_name, seq_id) do
    handle_message(Binary.deserialize(:message_begin, message), seq_id, rpc_name)
  end

  defp receive_message(sock, timeout) do
    receive do
      {:tcp, ^sock, data} -> {:ok, data}
      {:tcp_closed, ^sock} -> {:error, :closed}
      {:tcp_error, ^sock, error} -> {:error, error}
      {:ssl, ^sock, data} -> {:ok, data}
      {:ssl_closed, ^sock} -> {:error, :closed}
      {:ssl_error, ^sock, error} -> {:error, error}
    after
      timeout -> {:error, :timeout}
    end
  end

  defp handle_message({:ok, {:reply, seq_id, rpc_name, serialized_response}}, seq_id, rpc_name) do
    {:ok, serialized_response}
  end

  defp handle_message(
         {:ok, {:exception, seq_id, rpc_name, serialized_response}},
         seq_id,
         rpc_name
       ) do
    exception = Binary.deserialize(:application_exception, serialized_response)
    {:error, {:exception, exception}}
  end

  defp handle_message({:ok, {message_type, seq_id, rpc_name, _}}, seq_id, rpc_name) do
    exception =
      TApplicationException.exception(
        type: :invalid_message_type,
        message: "The server replied with invalid message type (#{message_type})"
      )

    {:error, {:exception, exception}}
  end

  defp handle_message({:ok, {_, seq_id, mismatched_rpc_name, _}}, seq_id, rpc_name) do
    exception =
      TApplicationException.exception(
        type: :wrong_method_name,
        message: "The server replied to #{mismatched_rpc_name}, but we sent #{rpc_name}"
      )

    {:error, {:exception, exception}}
  end

  defp handle_message({:ok, {_, mismatched_seq_id, _, _}}, seq_id, _) do
    exception =
      TApplicationException.exception(
        type: :bad_sequence_id,
        message:
          "Invalid sequence id. The client sent #{seq_id}, but the server replied with #{
            mismatched_seq_id
          }"
      )

    {:error, {:exception, exception}}
  end

  defp handle_message({:error, _} = err, _, _) do
    err
  end

  defp to_host(host) when is_bitstring(host) do
    String.to_charlist(host)
  end

  defp to_host(host) when is_list(host), do: host

  defp maybe_ssl_handshake(sock, host, port, %{ssl_opts: ssl_opts, timeout: timeout} = s) do
    with {optional, ssl_opts} when optional in [:required, :optional] <-
           SSL.configuration(ssl_opts),
         {:ok, ssl_sock} <- :ssl.connect(sock, ssl_opts, timeout) do
      {:ok, %{s | sock: {:ssl, ssl_sock}}}
    else
      nil ->
        {:ok, %{s | sock: {:gen_tcp, sock}}}

      {:error, %_exception{} = err} ->
        Logger.error("Failed SSL configuration due to: " <> Exception.format(:error, err, []))
        {:stop, err, s}

      {:error, reason} = error ->
        Logger.error(
          "Failed SSL handshake to #{host}:#{port} due to #{:ssl.format_error(reason)}"
        )

        {:stop, error, s}
    end
  end

  defp maybe_resend_data({:ok, s}, {:reconnect, {:call, msg, from}}) do
    case handle_call(msg, from, s) do
      {:reply, reply, s} ->
        GenServer.reply(from, reply)
        {:ok, s}

      {:disconnect, info, error, s} ->
        GenServer.reply(from, error)
        disconnect(info, s)

      _ ->
        {:ok, s}
    end
  end

  defp maybe_resend_data({:ok, s}, {:reconnect, {:cast, msg}}) do
    case handle_cast(msg, s) do
      {:noreply, s} ->
        {:ok, s}

      {:disconnect, info, s} ->
        disconnect(info, s)

      _ ->
        {:ok, s}
    end
  end

  defp maybe_resend_data(reply, _), do: reply
end
