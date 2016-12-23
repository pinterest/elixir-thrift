defmodule Thrift.Clients.BinaryFramed do
  alias Thrift.Protocols.Binary
  alias Thrift.TApplicationException

  @immutable_tcp_opts [active: false, packet: 4, mode: :binary]

  @type error :: {:error, atom}
  @type success :: {:ok, binary}

  @type protocol_response :: success | error

  @type retry_count :: integer
  @type backoff_ms :: integer
  @type backoff_fn :: ((retry_count) -> backoff_ms)

  @type data :: iolist | binary
  @type tcp_opts :: [
    timeout: integer,
    send_timeout: integer,
    backoff_calculator: backoff_fn
  ]

  @type genserver_call_options :: [
    timeout: integer
  ]

  @type options :: [
    tcp_opts: tcp_opts,
    gen_server_opts: genserver_call_options
  ]

  defmodule State do
    @type t :: %State{host: String.t,
                      port: (1..65535),
                      tcp_opts: BinaryFramed.tcp_opts,
                      timeout: integer,
                      sock: pid,
                      retries: integer,
                      backoff_calculator: BinaryFramed.backoff_fn}
    defstruct host: nil,
              port: nil,
              tcp_opts: nil,
              timeout: 5000,
              sock: nil,
              retries: 0,
              backoff_calculator: nil
  end

  require Logger
  use Connection

  def init({host, port, opts}) do
    tcp_opts = Keyword.get(opts, :tcp_opts, [])

    backoff_calculator = Keyword.get(tcp_opts, :backoff_calculator, &calculate_backoff/1)
    {timeout, tcp_opts} = Keyword.pop(tcp_opts, :timeout, 5000)

    s = %State{host: to_host(host),
               port: port,
               tcp_opts: tcp_opts,
               timeout: timeout,
               backoff_calculator: backoff_calculator,
               sock: nil}

    {:connect, :init, s}
  end

  @spec start_link(String.t, (0..65535), options) :: GenServer.on_start
  def start_link(host, port, opts) do
    Connection.start_link(__MODULE__, {host, port, opts})
  end

  def close(conn), do: Connection.call(conn, :close)

  def connect(_, %{sock: nil, host: host, port: port, tcp_opts: opts, timeout: timeout, retries: retries, backoff_calculator: backoff_calculator} = s) do
    opts = opts
    |> Keyword.merge(@immutable_tcp_opts)
    |> Keyword.put_new(:send_timeout, 1000)

    case :gen_tcp.connect(host, port, opts, timeout) do
      {:ok, sock} ->
        {:ok, %{s | sock: sock, retries: 0}}

      {:error, _} ->
        new_retries = retries + 1
        backoff = backoff_calculator.(new_retries)

        msg = "Failed to connect to #{host} (after #{new_retries + 1} attempts), retrying in #{backoff}ms."
        if retries <= 5 do
          Logger.info(msg)
        else
          Logger.warn(msg)
        end

        {:backoff, backoff, %{s | retries: new_retries}}
    end
  end

  def disconnect(info, %{sock: sock} = s) do
    :ok = :gen_tcp.close(sock)
    case info do
      {:close, from} ->
        Connection.reply(from, :ok)

      {:error, :closed} ->
        Logger.error("Connection closed")

      {:error, reason} ->
        reason = :inet.format_error(reason)
        Logger.error("Connection error: #{reason}")
    end
    {:connect, :reconnect, %{s | sock: nil}}
  end

  @spec oneway(pid, data) :: :ok
  def oneway(conn, data) do
    Connection.cast(conn, {:oneway, data})
  end

  @spec request(pid, data, options) :: protocol_response
  def request(conn, data, options) do
    tcp_opts = Keyword.get(options, :tcp_opts, [])
    gen_server_opts = Keyword.get(options, :gen_server_opts, [])
    gen_server_timeout = Keyword.get(gen_server_opts, :timeout, 5000)

    Connection.call(conn, {:request, data, tcp_opts}, gen_server_timeout)
  end

  def handle_call(_, _, %{sock: nil} = s) do
    {:reply, {:error, :closed}, s}
  end

  def handle_call({:request, data, options}, _, %{sock: sock, timeout: default_timeout} = s) do
    timeout = Keyword.get(options, :timeout, default_timeout)

    rsp = with :ok <- :gen_tcp.send(sock, data) do
      :gen_tcp.recv(sock, 0, timeout)
    end

    case rsp do
      {:ok, _} = ok ->
        {:reply, ok, s}

      {:error, :timeout} = timeout ->
        {:reply, timeout, s}

      {:error, _} = error ->
        {:disconnect, error, error, s}
    end
  end

  def handle_call(:close, from, s) do
    {:disconnect, {:close, from}, s}
  end

  def handle_cast(_, %{sock: nil} = s) do
    {:noreply, s}
  end

  def handle_cast({:oneway, data}, %{sock: sock} = s) do
    case :gen_tcp.send(sock, data) do
      :ok ->
        {:noreply, s}

      {:error, _} = error ->
        {:disconnect, error, s}
    end
  end

  def deserialize_message_reply(message, rpc_name, sequence_id, reply_module) do
    Binary.deserialize(:message_begin, message)
    |> handle_message(sequence_id, rpc_name, reply_module)
  end

  defp handle_message({:ok, {:reply, sequence_id, rpc_name, decoded_response}},
                      sequence_id, rpc_name, reply_module) do

    case reply_module.deserialize(decoded_response) do
      {%{success: nil}=resp, ""} ->

        response = resp
        |> Map.delete(:__struct__)
        |> Map.values
        |> Enum.reject(&is_nil(&1))

        case response do
          [exception] ->
            {:error, {:exception, exception}}

          [] ->
            # This case is when we have a void return on the
            # remote RPC
            {:ok, nil}
        end

      {%{success: success}, ""} when not is_nil(success) ->
        {:ok, success}

      {resp, extra} ->
        {:error, {:extraneous_data, resp, extra}}
    end
  end

  defp handle_message({:ok, {:exception, sequence_id, rpc_name, response}}, sequence_id, rpc_name, _) do
    exc = read_t_application_exception(response, %TApplicationException{})
    {:error, {:exception, exc}}
  end

  defp handle_message({:ok, {_, decoded_sequence_id, _, decoded_rpc_name, _}},
                      sequence_id, rpc_name, _) do
    ex = case {decoded_sequence_id, decoded_rpc_name} do
           {^sequence_id, mismatched_rpc_name} ->
             message = "The server replied to #{mismatched_rpc_name}, but we sent #{rpc_name}"
             %TApplicationException{message: message,
                                    type: TApplicationException.exception_type(3)}

           {mismatched_sequence_id, ^rpc_name} ->
             message = "Invalid sequence id. The client sent #{sequence_id}, but the server replied with #{mismatched_sequence_id}"
             %TApplicationException{message: message,
                                    type: TApplicationException.exception_type(4)}

           {_mismatched_sequence_id, _mismatched_rpc_name} ->
             message = "Both sequence id and rpc name are wrong. The server is extremely uncompliant."
             %TApplicationException{message: message,
                                    type: :sequence_id_and_rpc_name_mismatched}
         end
    {:error, {:exception, ex}}
  end

  defp handle_message({:error, _} = err, _, _, _) do
    err
  end

  defp read_t_application_exception(
        <<11::size(8),
        1::16-unsigned,
        message_size::32-signed,
        message::binary-size(message_size),
        rest::binary>>, accum) do
    # read the message string
    read_t_application_exception(rest, Map.put(accum, :message, message))
  end
  defp read_t_application_exception(
        <<8::size(8),
        2::16-unsigned,
        type::32-signed,
        rest::binary>>, accum) do
    # read the type
    exception_type = TApplicationException.exception_type(type)
    read_t_application_exception(rest, Map.put(accum, :type, exception_type))
  end
  defp read_t_application_exception(<<0>>, accum) do
    # read the field stop and return
    accum
  end
  defp read_t_application_exception(error, _) do
    message = "Could not decode TApplicationException, remaining was #{inspect error}"
    %TApplicationException{message: message,
                           type: TApplicationException.exception_type(7)}
  end

  defp to_host(host) when is_bitstring(host) do
    String.to_char_list(host)
  end
  defp to_host(host) when is_list(host), do: host

  defp calculate_backoff(0), do: 100
  defp calculate_backoff(1), do: 100
  defp calculate_backoff(2), do: 200
  defp calculate_backoff(3), do: 300
  defp calculate_backoff(4), do: 500
  defp calculate_backoff(5), do: 800
  defp calculate_backoff(_), do: 1000
end
