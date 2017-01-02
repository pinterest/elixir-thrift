defmodule Thrift.Servers.BinaryFramed.Acceptor do
  @moduledoc """
  A GenServer that accepts connections on a server and processes the thrift messages.
  """

  alias Thrift.{
    Protocol,
    TApplicationException
  }
  require Logger

  @spec start_link(pid, module, module) :: GenServer.on_start
  def start_link(socket, server_module, handler_module) do
    pid = spawn_link(__MODULE__, :init, [socket, server_module, handler_module])
    {:ok, pid}
  end

  def init(socket, server_module, handler_module) do
    {:ok, r_sock} = :gen_tcp.accept(socket)

    do_thrift_call({r_sock, server_module, handler_module})
  end

  defp do_thrift_call({socket, server_module, handler_module} = args) do
    thrift_response  = with({:ok, message}      <- :gen_tcp.recv(socket, 0),
                            parsed_response     <- Protocol.Binary.deserialize(:message_begin, message)) do

      handle_thrift_message(parsed_response, server_module, handler_module)
    end

    case thrift_response do
      {:ok, :reply, thrift_data} ->
        :ok = :gen_tcp.send(socket, thrift_data)
        do_thrift_call(args)

      {:error, {:server_error, thrift_data}} ->
        :ok = :gen_tcp.send(socket, thrift_data)
        :ok = :gen_tcp.close(socket)

      {:error, _} = err ->
        Logger.info("Thrift call failed: #{inspect err}")
        :ok = :gen_tcp.close(socket)
    end
  end

  def handle_thrift_message({:ok, {:call, sequence_id, name, args_binary}}, server_module, handler_module) do
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

  def handle_thrift_message({:ok, {:oneway, _seq_id, name, args_binary}}, server_module, handler_module) do
    spawn(server_module, :handle_thrift, [name, args_binary, handler_module])
    {:ok, :reply, <<0>>}
  end

  def handle_thrift_message({:error, msg} = err, _, _) do
    Logger.warn("Could not decode Thrift message: #{inspect msg}")
    err
  end
end
