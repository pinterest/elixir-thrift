defmodule Thrift.Servers.BinaryFramed do
  @moduledoc false

  @type server_opts :: [
    worker_count: pos_integer,
    name: atom
  ]

  alias Thrift.Servers.BinaryFramed

  @spec start_link(module, (1..65535), module, server_opts) :: GenServer.on_start
  def start_link(server_module, port, handler_module, opts) do
    name = Keyword.get(opts, :name, handler_module)
    worker_count = Keyword.get(opts, :worker_count, 1)
    :ranch.start_listener(name,
                          worker_count,
                          :ranch_tcp,
                          [port: port],
                          BinaryFramed.ProtocolHandler,
                          {server_module, handler_module})
  end

  def stop(name) do
    :ok = :ranch.stop_listener(name)
  end
end
