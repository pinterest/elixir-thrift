defmodule Thrift.Binary.Framed.ProtocolHandler do
  @moduledoc """
  A GenServer that accepts connections on a server and processes the thrift messages.
  """

  @default_timeout 20_000

  alias Thrift.{
    Protocol,
    TApplicationException
  }
  require Logger

  @spec start_link(reference, pid, module, {module, module, Keyword.t}) :: GenServer.on_start
  def start_link(ref, socket, transport, {server_module, handler_module, tcp_opts}) do
    pid = spawn_link(__MODULE__, :init, [ref, socket, transport, server_module, handler_module, tcp_opts])
    {:ok, pid}
  end

  @spec init(reference, pid, module, module, module, Keyword.t) :: :ok | no_return
  def init(ref, socket, transport, server_module, handler_module, tcp_opts) do
    :ok = :ranch.accept_ack(ref)

    {recv_timeout, tcp_opts} = Keyword.pop(tcp_opts, :recv_timeout, @default_timeout)

    transport_options = Keyword.put(tcp_opts, :packet, 4)
    transport.setopts(socket, transport_options)

    do_thrift_call({transport, socket, server_module, handler_module, recv_timeout})
  end

  defp do_thrift_call({transport, socket, server_module, handler_module, recv_timeout} = args) do
    thrift_response  = with({:ok, message}      <- transport.recv(socket, 0, recv_timeout),
                            parsed_response     <- Protocol.Binary.deserialize(:message_begin, message)) do

      handle_thrift_message(parsed_response, server_module, handler_module)
    end

    case thrift_response do
      {:ok, :reply, thrift_data} ->
        :ok = transport.send(socket, thrift_data)
        do_thrift_call(args)

      {:error, {:server_error, thrift_data}} ->
        :ok = transport.send(socket, thrift_data)
        exit({:shutdown, :server_error})

      {:error, _} = err ->
        Logger.info(inspect err)
        :ok = transport.close(socket)
    end
  end

  defp handle_thrift_message({:ok, {:call, sequence_id, name, args_binary}}, server_module, handler_module) do
    case server_module.handle_thrift(name, args_binary, handler_module) do
      {:reply, serialized_reply} ->
        message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

        {:ok, :reply, [message | serialized_reply]}

      {:server_error, %TApplicationException{} = exc} ->
        message = Protocol.Binary.serialize(:message_begin, {:exception, sequence_id, name})
        serialized_exception = Protocol.Binary.serialize(:application_exception, exc)

        {:error, {:server_error, [message |  serialized_exception]}}

      :noreply ->
        message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

        {:ok, :reply, [message | <<0>>]}
    end

  end

  defp handle_thrift_message({:ok, {:oneway, _seq_id, name, args_binary}}, server_module, handler_module) do
    spawn(server_module, :handle_thrift, [name, args_binary, handler_module])
    {:ok, :reply, <<0>>}
  end

  defp handle_thrift_message({:error, msg} = err, _, _) do
    Logger.warn("Could not decode Thrift message: #{inspect msg}")
    err
  end
end
