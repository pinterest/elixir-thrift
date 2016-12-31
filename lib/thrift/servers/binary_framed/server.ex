defmodule Thrift.Servers.BinaryFramed do
  @moduledoc false

  @type server_opts :: [
    worker_count: pos_integer
  ]

  alias Thrift.Servers.BinaryFramed.AcceptorSupervisor

  defmodule State do
    @moduledoc false
    defstruct port: nil, socket: nil, supervisor: nil
  end

  @type opts :: [
    port: (1..65535),
  ]

  @spec start_link(module, (1..65535), module, server_opts, GenServer.option) :: GenServer.on_start
  def start_link(server_module, port, handler_module, opts, gen_server_opts) do
    GenServer.start_link(__MODULE__, [server_module, port, handler_module, opts], gen_server_opts)
  end

  def init([server_module, port, handler_module, opts]) do
    worker_count = Keyword.get(opts, :worker_count, 1)

    {:ok, socket} = :gen_tcp.listen(port, [:binary, packet: 4, active: false])
    {:ok, supervisor} = AcceptorSupervisor.start_link(socket, server_module, handler_module, worker_count)
    {:ok, %State{port: port, socket: socket, supervisor: supervisor}}
  end
end
