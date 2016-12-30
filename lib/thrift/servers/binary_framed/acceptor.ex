defmodule Thrift.Servers.BinaryFramed.Acceptor do
  @moduledoc """
  A GenServer that accepts connections on a server and processes the thrift messages.
  """

  alias Thrift.{
    Protocol,
    TApplicationException
  }
  use GenServer
  require Logger

  @spec start_link(pid, module, module) :: GenServer.on_start
  def start_link(socket, server_module, handler_module) do
    GenServer.start_link(__MODULE__, [socket, server_module, handler_module])
  end

  def init([socket, server_module, handler_module]) do
    Process.send_after(self, :accept, 1)
    {:ok, {socket, server_module, handler_module}}
  end

  def handle_info(:accept, {socket, server_module, handler_module}) do
    {:ok, r_sock} = :gen_tcp.accept(socket)

    do_thrift_call({r_sock, server_module, handler_module})
  end

  defp do_thrift_call({socket, server_module, handler_module} = args) do
    err = with {:ok, message}   <- :gen_tcp.recv(socket, 0),
    parsed_response             <- Protocol.Binary.deserialize(:message_begin, message),
    {:ok, thrift_data}          <- handle_thrift_message(parsed_response, server_module, handler_module) do

      :gen_tcp.send(socket, thrift_data)
      do_thrift_call(args)
    end

    :gen_tcp.close(socket)
    {:stop, err, nil}
  end

  def handle_thrift_message({:ok, {:call, sequence_id, name, args_binary}}, server_module, handler_module) do
    reply = case server_module.handle_thrift(name, args_binary, handler_module) do
              {:reply, serialized_reply} ->
                message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

                [message | serialized_reply]

              {:server_error, %TApplicationException{} = exc} ->
                message = Protocol.Binary.serialize(:message_begin, {:exception, sequence_id, name})
                serialized_exception = Protocol.Binary.serialize(:application_exception, exc)

                [message |  serialized_exception]

              :noreply ->
                message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

                [message | <<0>>]
            end

    {:ok, reply}
  end

  def handle_thrift_message({:ok, {:oneway, _seq_id, name, args_binary}}, server_module, handler_module) do
    spawn(server_module, :handle_thrift, [name, args_binary, handler_module])
    {:ok, <<0>>}
  end

  def handle_thrift_message({:error, msg} = err, _, _) do
    Logger.warn("Could not decode Thrift message: #{inspect msg}")
    err
  end
end
