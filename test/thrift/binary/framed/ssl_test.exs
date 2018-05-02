defmodule Servers.Binary.Framed.SSLTest do
  use ThriftTestCase

  @thrift_file name: "ssl_test.thrift", contents: """
    service SSLTest {
      bool ping()
    }
  """

  alias Servers.Binary.Framed.SSLTest.SSLTest.Binary.Framed.{Client, Server}

  def define_handler do
    defmodule SSLTestHandler do
      alias Servers.Binary.Framed.SSLTest.SSLTest.Handler
      @behaviour Handler
      def ping, do: true
    end
  end

  setup_all do
    {:module, mod_name, _, _} = define_handler()
    {:ok, _} = Server.start_link(mod_name, 0, [ssl_opts: [enabled: true, configure: &get_certs/0]])
    server_port = :ranch.get_port(mod_name)

    {:ok, handler_name: mod_name, port: server_port}
  end

  setup(ctx) do
    {:ok, client} = Client.start_link("localhost", ctx.port, ssl_opts: ctx[:ssl_opts] ++ [enabled: true])

    {:ok, client: client}
  end

  @tag ssl_opts: []
  thrift_test "it can return a simple boolean value", ctx do
    assert {:ok, true} == Client.ping(ctx.client)
  end

  @tag ssl_opts: [configure: {__MODULE__, :test_configure, []}]
  thrift_test "it can handle live configuration", ctx do
    assert {:ok, true} == Client.ping(ctx.client)
    assert_received :configured
  end

  ## Helpers

  defp get_certs() do
    certs_path = Application.app_dir(:ssl, "examples/certs/etc/server")
    cacerts = Path.join(certs_path, "cacerts.pem")
    cert = Path.join(certs_path, "cert.pem")
    key = Path.join(certs_path, "key.pem")

    {:ok, [cacertfile: cacerts, certfile: cert, keyfile: key]}
  end

  def test_configure() do
    [parent | _] = Process.get(:"$ancestors")
    send(parent, :configured)
    get_certs()
  end
end

