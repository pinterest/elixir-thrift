defmodule Thrift.Binary.Framed.Server do
  @moduledoc false

  @type server_option ::
          {:worker_count, pos_integer}
          | {:name, atom}
          | {:max_restarts, non_neg_integer}
          | {:max_seconds, non_neg_integer}
          | {:tcp_opts, :ranch_tcp.opts()}
          | {:ssl_opts, [Thrift.Transport.SSL.option()]}

  @type server_opts :: [server_option]

  @spec start_link(module, 1..65_535, module, [server_option]) :: GenServer.on_start()
  def start_link(server_module, port, handler_module, opts) do
    name = Keyword.get(opts, :name, handler_module)
    max_restarts = Keyword.get(opts, :max_restarts, 10)
    max_seconds = Keyword.get(opts, :max_seconds, 5)
    worker_count = Keyword.get(opts, :worker_count, 1)
    tcp_opts = Keyword.get(opts, :tcp_opts, [])
    ssl_opts = Keyword.get(opts, :ssl_opts, [])

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.put(:port, port)

    listener =
      :ranch.child_spec(
        name,
        worker_count,
        :ranch_tcp,
        transport_opts,
        Thrift.Binary.Framed.ProtocolHandler,
        {server_module, handler_module, tcp_opts, ssl_opts}
      )

    Supervisor.start_link(
      [listener],
      strategy: :one_for_one,
      max_restarts: max_restarts,
      max_seconds: max_seconds
    )
  end

  def stop(pid) do
    Supervisor.stop(pid)
  end
end
