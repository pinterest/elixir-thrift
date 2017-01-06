defmodule Thrift.Binary.Framed.Client do
  @moduledoc """
  A client implementation of Thrift's Binary Framed protocol.

  This client is meant to be used with a generated Thrift client. This module
  implements framing on top of the Connection behaviour.

  This module ony adds two functions to the connection behaviour,
 `oneway` and `request`.


  """
  alias Thrift.Protocol.Binary
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
    timeout: pos_integer,
    send_timeout: integer,
    backoff_calculator: backoff_fn
  ]

  @type genserver_call_options :: [
    timeout: pos_integer
  ]

  @type options :: [
    tcp_opts: tcp_opts,
    gen_server_opts: genserver_call_options
  ]

  defmodule State do
    @moduledoc false

    @type t :: %State{host: String.t,
                      port: (1..65_535),
                      tcp_opts: Client.tcp_opts,
                      timeout: integer,
                      sock: pid,
                      retries: non_neg_integer,
                      backoff_calculator: Client.backoff_fn,
                      seq_id: integer}
    defstruct host: nil,
              port: nil,
              tcp_opts: nil,
              timeout: 5000,
              sock: nil,
              retries: 0,
              backoff_calculator: nil,
              seq_id: 0
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

  @doc """
  Starts and connects the client.
  When called, the client connects on the appropriate host and port and establishes
  a TCP connection. The options keyword list takes the following options:

    `tcp_opts`: A keyword list that controls how the underlying connection is handled. All options
     not handled below are sent to the underlying gen_tcp (with the exception of the
     following options, which, if overridden, would break the framed client:
     [`active`, `packet`, `mode`x]

     - `timeout`:  An integer that governs how long the gen_tcp connection waits for operations
        to complete. This timeout is used when connecting or receiving data.

     - `send_timeout`: An integer that governs how long our connection waits when sending data.

     - `backoff_calculator`: A single argument function that takes the number of retries and returns the
        amount of time to wait in milliseconds before reconnecting. The default implementation
        waits 100, 100, 200, 300, 500, 800 and then 1000 ms. All retries after that will wait 1000ms.


    `gen_server_opts`: A keyword list of options that control the gen_server behaviour.

     - `timeout`:  The amount of time to wait (in milliseconds) for a reply from a `GenServer` call.

  """
  @spec start_link(String.t, (0..65_535), options) :: GenServer.on_start
  def start_link(host, port, opts) do
    Connection.start_link(__MODULE__, {host, port, opts})
  end

  @doc """
  Closes the client's underlying connection.
  """
  def close(conn), do: Connection.call(conn, :close)

  @doc false
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

  @doc false
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

  @spec oneway(pid, String.t, data, options) :: :ok
  @doc """
  Execute a one way RPC. One way RPC calls do not generate a response,
  and as such, this implementation uses `GenServer.cast`.
  The data argument must be a properly formatted Thrift message.
  """
  def oneway(conn, rpc_name, serialized_args, _opts) do
    :ok = Connection.cast(conn, {:oneway, rpc_name, serialized_args})
    :ok
  end

  @spec call(pid, String.t, data, options) :: protocol_response
  @doc """
  Executes a Thrift RPC. The data argument must be a correctly formatted
  Thrift message.

  The `options` argument takes the same type of keyword list that `start_link` takes.
  """
  def call(conn, rpc_name, serialized_args, opts) do
    tcp_opts = Keyword.get(opts, :tcp_opts, [])
    gen_server_opts = Keyword.get(opts, :gen_server_opts, [])
    gen_server_timeout = Keyword.get(gen_server_opts, :timeout, 5000)

    Connection.call(conn, {:call, rpc_name, serialized_args, tcp_opts}, gen_server_timeout)
  end

  def handle_call(_, _, %{sock: nil} = s) do
    {:reply, {:error, :closed}, s}
  end

  def handle_call({:call, rpc_name, serialized_args, tcp_opts}, _,
                  %{sock: sock, seq_id: seq_id, timeout: default_timeout} = s) do

    s = %{s | seq_id: seq_id + 1}
    message = Binary.serialize(:message_begin, {:call, seq_id, rpc_name})
    timeout = Keyword.get(tcp_opts, :timeout, default_timeout)

    rsp = with :ok <- :gen_tcp.send(sock, [message | serialized_args]) do
      :gen_tcp.recv(sock, 0, timeout)
    end

    case rsp do
      {:ok, message} ->
        reply =  deserialize_message_reply(message, rpc_name, seq_id)
        {:reply, reply, s}

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

  def handle_cast({:oneway, rpc_name, serialized_args},
                  %{sock: sock, seq_id: seq_id} = s) do

    s = %{s | seq_id: seq_id + 1}
    message = Binary.serialize(:message_begin, {:oneway, seq_id, rpc_name})

    case :gen_tcp.send(sock, [message | serialized_args]) do
      :ok ->
        {:noreply, s}

      {:error, _} = error ->
        {:disconnect, error, s}
    end
  end

  def deserialize_message_reply(message, rpc_name, seq_id) do
    Binary.deserialize(:message_begin, message)
    |> handle_message(seq_id, rpc_name)
   end

  defp handle_message({:ok, {:reply, seq_id, rpc_name, serialized_response}}, seq_id, rpc_name) do
    {:ok, serialized_response}
  end
  defp handle_message({:ok, {:exception, seq_id, rpc_name, serialized_response}}, seq_id, rpc_name) do
    exception = Binary.deserialize(:application_exception, serialized_response)
    {:error, {:exception, exception}}
  end
  defp handle_message({:ok, {message_type, seq_id, rpc_name, _}}, seq_id, rpc_name) do
    exception = %TApplicationException{
      message: "The server replied with invalid message type #{message_type}",
      type: :invalid_message_type}
    {:error, {:exception, exception}}
  end
  defp handle_message({:ok, {_, seq_id, mismatched_rpc_name, _}}, seq_id, rpc_name) do
    exception = %TApplicationException{
      message: "The server replied to #{mismatched_rpc_name}, but we sent #{rpc_name}",
      type: :wrong_method_name}
    {:error, {:exception, exception}}
  end
  defp handle_message({:ok, {_, mismatched_seq_id, _, _}}, seq_id, _) do
    exception = %TApplicationException{
      message: "Invalid sequence id. The client sent #{seq_id}, but the server replied with #{mismatched_seq_id}",
      type: :bad_sequence_id}
    {:error, {:exception, exception}}
  end
  defp handle_message({:error, _} = err, _, _) do
    err
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
