defmodule Calculator.Application do
  @moduledoc false

  use Application
  alias Calculator.Generated.Service.Binary.Framed.Server

  def start(_type, _args) do
    port = Application.get_env(:calculator, :port, 9090)

    children = [
      server_child_spec(port)
    ]

    opts = [strategy: :one_for_one, name: Calculator.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Note that when adding a server to your supervision tree, you should always ensure to start
  # it as a supervisor instead of a worker. Also, you may be tempted to create a pool of server
  # processes, however this is not needed. Instead, you can configure the `:worker_count`
  # option in the server. See the docs for `Server.start_link/4` for more details.
  defp server_child_spec(port) do
    %{
      id: Server,
      start: {Server, :start_link, [Calculator.ServiceHandler, port]},
      type: :supervisor
    }
  end
end
