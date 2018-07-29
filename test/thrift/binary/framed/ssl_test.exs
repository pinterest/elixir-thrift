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

    {:ok, handler_name: mod_name}
  end

  def build_ssl_required_server(ctx) do
    {:ok, _} = Server.start_link(ctx[:handler_name], 0, [name: ctx.test, ssl_opts: [enabled: true, configure: &get_certs/0]])
    server_port = :ranch.get_port(ctx.test)

    {:ok, port: server_port}
  end

  def build_ssl_optional_server(ctx) do
    {:ok, _} = Server.start_link(ctx[:handler_name], 0, [name: ctx.test, ssl_opts: [enabled: true, configure: &get_certs/0, optional: true]])
    server_port = :ranch.get_port(ctx.test)

    {:ok, port: server_port}
  end

  def build_ssl_client(ctx) do
    {:ok, ssl_client} = Client.start_link("localhost", ctx.port, ssl_opts: ctx[:ssl_opts] ++ [enabled: true])

    {:ok, ssl_client: ssl_client}
  end

  def build_plain_client(ctx) do
    {:ok, plain_client} = Client.start_link("localhost", ctx.port)

    {:ok, plain_client: plain_client}
  end

  describe "Required-SSL communication" do
    setup [:build_ssl_required_server, :build_ssl_client, :build_plain_client]

    @tag ssl_opts: []
    thrift_test "it can return a simple boolean value", ctx do
      assert {:ok, true} == Client.ping(ctx.ssl_client)
    end

    @tag ssl_opts: [configure: {__MODULE__, :test_configure, []}]
    thrift_test "it can handle live configuration", ctx do
      assert {:ok, true} == Client.ping(ctx.ssl_client)
      assert_received :configured
    end

    @tag ssl_opts: []
    thrift_test "plain client will be rejected", %{plain_client: client} do
      Process.flag(:trap_exit, true)
      assert {:error, :closed} == Client.ping(client)
      assert_receive {:EXIT, ^client, {:error, :closed}}
    end
  end

  describe "Optional-SSL communication" do
    setup [:build_ssl_optional_server, :build_ssl_client, :build_plain_client]

    @tag ssl_opts: []
    thrift_test "ssl client can receive a simple boolean value", ctx do
      assert {:ok, true} == Client.ping(ctx.ssl_client)
    end

    @tag ssl_opts: []
    thrift_test "plain client can receive a simple boolean value", ctx do
      assert {:ok, true} == Client.ping(ctx.plain_client)
    end
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

