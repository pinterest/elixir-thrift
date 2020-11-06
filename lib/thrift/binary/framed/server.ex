defmodule Thrift.Binary.Framed.Server do
  require Logger

  @moduledoc """
  A server implementation of Thrift's Binary Framed protocol.

  See `start_link/4` for the various options.

      {:ok, pid} = Server.start_link(ServiceHandler, 2345, [])
  """

  @type server_option ::
          {:worker_count, pos_integer}
          | {:name, atom}
          | {:tcp_opts, :ranch_tcp.opts()}
          | {:ssl_opts, [Thrift.Transport.SSL.option()]}
          | {:transport_opts, :ranch.opts()}

  @type server_opts :: [server_option]

  @doc """
  Starts the server using the specified handler module.

  The following server options can be specified:

  `:worker_count`: The number of acceptor workers to accept on the socket.

  `:name`: (Optional) The name of the server. The server's pid becomes
  registered under this name. If not specified, the handler module's name
  is used.

  `tcp_opts`: A keyword list that controls how the underlying connection is
  handled. All options are sent directly to `:ranch_tcp`.

  `ssl_opts`: A keyword list of SSL/TLS options:

  - `:enabled`: A boolean indicating whether to upgrade the connection to
    the SSL protocol
  - `:optional`: A boolean indicating whether to accept both SSL and plain
    connections
  - `:configure`: A 0-arity function to provide additional SSL options at
    runtime
  - Additional `:ssl.ssl_option/0` values specifying other `:ssl` options

  `transport_opts` can be used to specify any additional options to pass
  to `:ranch.child_spec/6`.
  """
  @spec start_link(module, port :: 1..65_535, module, [server_option]) :: GenServer.on_start()
  def start_link(server_module, port, handler_module, opts) do
    name = Keyword.get(opts, :name, handler_module)
    worker_count = Keyword.get(opts, :worker_count, 1)
    tcp_opts = Keyword.get(opts, :tcp_opts, [])
    ssl_opts = Keyword.get(opts, :ssl_opts, [])

    transport_opts =
      opts
      |> Keyword.get(:transport_opts, [])
      |> Keyword.put(:port, port)

    validate_ssl_configuration!(ssl_opts)

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
      max_restarts: 0
    )
  end

  @doc """
  Stops the server.
  """
  def stop(pid) do
    Supervisor.stop(pid)
  end

  # Ensure that SSL certs are available before starting server.
  def validate_ssl_configuration!(ssl_opts) do
    case Thrift.Transport.SSL.configuration(ssl_opts) do
      {:error, %_exception{} = err} ->
        Logger.error("Error validating SSL configuration: " <> Exception.format(:error, err, []))

      nil ->
        :ok

      {optional, _ssl_opts} when optional in [:required, :optional] ->
        :ok
    end
  end
end
