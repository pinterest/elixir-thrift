defmodule Thrift.Servers.BinaryFramed.AcceptorSupervisor do
  @moduledoc """
  A supervisor for acceptor processes.
  """

  alias Thrift.Servers.BinaryFramed.Acceptor
  use Supervisor

  def start_link(socket, server_module, handler_module, count) do
    Supervisor.start_link(__MODULE__, {socket, server_module, handler_module, count})
  end

  def init({socket, server_module, handler_module, count}) do
    children = for id <- (1..count) do
      worker(Acceptor, [socket, server_module, handler_module], id: {handler_module, id})
    end
    supervise(children, strategy: :one_for_one)
  end
end
