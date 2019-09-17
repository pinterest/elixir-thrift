defmodule Servers.Binary.Framed.IntegrationTest do
  use ThriftTestCase

  @thrift_file name: "server_test.thrift",
               contents: """
                 exception TestException {
                   1: string message,
                   2: i32 code,
                 }

                 exception UserNotFound {
                   1: string message,
                 }

                 exception OtherException {
                   2: string message,
                 }

                 struct IdAndName {
                  1: i64 id,
                  2: string name,
                 }

                 service ServerTest {
                   void returns_nothing()
                   oneway void do_async(1: string message);
                   bool ping();
                   bool checked_exception() throws (1: TestException ex);
                   bool multiple_exceptions(1: i32 exc_type) throws
                      (1: TestException e, 2: UserNotFound unf, 3: OtherException other);
                   bool server_exception();
                   IdAndName echo_struct(1: IdAndName id_and_name);
                   i64 myCamelCasedFunction(1: string myUserName);
                 }
               """

  def define_handler do
    defmodule ServerTestHandler do
      alias Servers.Binary.Framed.IntegrationTest, as: T
      alias Servers.Binary.Framed.IntegrationTest.OtherException
      alias Servers.Binary.Framed.IntegrationTest.ServerTest
      alias Servers.Binary.Framed.IntegrationTest.TestException
      alias Servers.Binary.Framed.IntegrationTest.UserNotFound
      @behaviour ServerTest.Handler

      @impl ServerTest.Handler
      def do_async(message) do
        Agent.update(:server_args, fn _ -> message end)
      end

      @impl ServerTest.Handler
      def ping, do: true

      @impl ServerTest.Handler
      def checked_exception do
        raise T.TestException, message: "Oh noes!", code: 400
      end

      @impl ServerTest.Handler
      def server_exception do
        raise "This wasn't supposed to happen"
      end

      @impl ServerTest.Handler
      def echo_struct(id_and_name), do: id_and_name

      @impl ServerTest.Handler
      def returns_nothing, do: nil

      @impl ServerTest.Handler
      def multiple_exceptions(1), do: raise(TestException, message: "BOOM", code: 124)
      def multiple_exceptions(2), do: raise(UserNotFound, message: "Not here!")
      def multiple_exceptions(3), do: raise(OtherException, message: "This is the other")
      def multiple_exceptions(_), do: true

      @impl ServerTest.Handler
      def my_camel_cased_function(user_name) do
        Agent.update(:server_args, fn _ -> user_name end)
        2421
      end
    end
  end

  alias Servers.Binary.Framed.IntegrationTest.ServerTest.Binary.Framed.Client
  alias Servers.Binary.Framed.IntegrationTest.ServerTest.Binary.Framed.Server
  alias Thrift.TApplicationException

  setup_all do
    {:module, mod_name, _, _} = define_handler()
    {:ok, _} = Server.start_link(mod_name, 0, [])
    server_port = :ranch.get_port(mod_name)

    {:ok, handler_name: mod_name, port: server_port}
  end

  setup(%{port: port, test: client_name}) do
    {:ok, agent} = Agent.start_link(fn -> nil end, name: :server_args)

    on_exit(fn ->
      ref = Process.monitor(agent)

      receive do
        {:DOWN, ^ref, _, _, _} ->
          :ok
      end
    end)

    {:ok, client} = Client.start_link("localhost", port, name: client_name)

    {:ok, client: client, client_name: client_name}
  end

  thrift_test "it can return a simple boolean value", ctx do
    assert {:ok, true} == Client.ping(ctx.client)
  end

  thrift_test "it can throw checked exceptions", ctx do
    expected_exception = TestException.exception(message: "Oh noes!", code: 400)
    assert {:error, {:exception, expected_exception}} == Client.checked_exception(ctx.client)
  end

  thrift_test "it can throw many checked exceptions", ctx do
    e1 = TestException.exception(message: "BOOM", code: 124)
    e2 = UserNotFound.exception(message: "Not here!")
    e3 = OtherException.exception(message: "This is the other")

    assert {:ok, true} == Client.multiple_exceptions(ctx.client, 0)
    assert {:error, {:exception, e1}} == Client.multiple_exceptions(ctx.client, 1)
    assert {:error, {:exception, e2}} == Client.multiple_exceptions(ctx.client, 2)
    assert {:error, {:exception, e3}} == Client.multiple_exceptions(ctx.client, 3)
  end

  thrift_test "it can handle unexpected exceptions", ctx do
    {:error, {:exception, %TApplicationException{} = exception}} =
      Client.server_exception(ctx.client)

    assert :internal_error == exception.type
    assert exception.message =~ "Server error: ** (RuntimeError) This wasn't supposed to happen"
  end

  thrift_test "it can return nothing", ctx do
    {:ok, nil} = Client.returns_nothing(ctx.client)
  end

  thrift_test "it can return structs", ctx do
    id_and_name = %IdAndName{id: 1234, name: "stinky"}
    assert {:ok, ^id_and_name} = Client.echo_struct(ctx.client, id_and_name)
  end

  thrift_test "it can handle bogus data", ctx do
    {:ok, socket} = :gen_tcp.connect('localhost', ctx.port, [:binary, packet: 4, active: false])
    :ok = :gen_tcp.send(socket, <<1, 2, 3, 4, 5>>)

    assert {:error, :closed} == :gen_tcp.recv(socket, 0)
    assert {:ok, true} = Client.ping(ctx.client)
  end

  thrift_test "it can handle oneway messages", ctx do
    assert {:ok, nil} = Client.do_async(ctx.client, "my message")

    :timer.sleep(100)

    assert "my message" = Agent.get(:server_args, & &1)
  end

  thrift_test "camel cased functions are converted to underscore", ctx do
    assert {:ok, 2421} == Client.my_camel_cased_function(ctx.client, "username")

    :timer.sleep(100)
    assert "username" == Agent.get(:server_args, & &1)
  end

  thrift_test "client can be found by name", %{client: client, client_name: name} do
    assert client == Process.whereis(name)
  end

  thrift_test "client methods can be called by name instead of pid", %{client_name: name} do
    assert {:ok, true} == Client.ping(name)
  end

  @ping_reply <<128, 1, 0, 2, 0, 0, 0, 4, 112, 105, 110, 103, 0, 0, 0, 0, 2, 0, 0, 1, 0>>
  thrift_test "client can reconnect when connection closed by server", ctx do
    {:ok, sock} = :gen_tcp.listen(0, [:binary, packet: 4, active: false])
    {:ok, port} = :inet.port(sock)

    first_conn =
      Task.async(fn ->
        {:ok, conn} = :gen_tcp.accept(sock)
        :ok = :gen_tcp.close(conn)
      end)

    name = String.to_atom("#{ctx.client_name}_1")
    {:ok, client} = Client.start_link("localhost", port, name: name, reconnect: true)

    second_conn =
      Task.async(fn ->
        {:ok, conn} = :gen_tcp.accept(sock)
        {:ok, _} = :gen_tcp.recv(conn, 0)
        :ok = :gen_tcp.send(conn, @ping_reply)
      end)

    assert {:ok, true} == Client.ping(client)
    Task.await(first_conn)
    Task.await(second_conn)
  end
end
