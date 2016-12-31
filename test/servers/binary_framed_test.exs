defmodule Servers.BinaryFramedIntegrationTest do
  use ThriftTestCase

  @thrift_file name: "server_test.thrift", contents: """
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
      alias Servers.BinaryFramedIntegrationTest.ServerTest.Handler
      alias Servers.BinaryFramedIntegrationTest.{TestException, UserNotFound, OtherException}
      alias Servers.BinaryFramedIntegrationTest, as: T
      @behaviour Handler.Behaviour

      def do_async(message) do
        Agent.update(:server_args, fn(_) -> message end)
      end

      def ping, do: true

      def checked_exception do
        raise %T.TestException{message: "Oh noes!", code: 400}
      end

      def server_exception do
        raise "This wasn't supposed to happen"
      end

      def echo_struct(id_and_name), do: id_and_name

      def returns_nothing, do: nil

      def multiple_exceptions(exception_type) do
        case exception_type do
          1 -> raise %TestException{message: "BOOM", code: 124}
          2 -> raise %UserNotFound{message: "Not here!"}
          3 -> raise %OtherException{message: "This is the other"}
          _ -> true
        end
      end

      def my_camel_cased_function(user_name) do
        Agent.update(:server_args, fn(_) -> user_name end)
        2421
      end
    end
  end

  alias Servers.BinaryFramedIntegrationTest.ServerTest.Client
  alias Servers.BinaryFramedIntegrationTest.ServerTest.Server
  alias Thrift.TApplicationException, as: TAE

  setup do
    server_port = :rand.uniform(10000) + 12000

    {:ok, agent} = Agent.start_link(fn -> nil end, name: :server_args)

    on_exit fn ->
      if Process.alive?(agent) do
        ref = Process.monitor(agent)
        Agent.stop(agent)

          receive do
            {:DOWN, ^ref, _, _, _} ->
              :ok
          end
      end
    end

    :rand.seed(:exs64, :erlang.now)


    {:module, mod_name, _, _} = define_handler()
    {:ok, client} = Client.Framed.start_link("localhost", server_port, [])

    {:ok, _} = Server.Framed.start_link(mod_name, server_port, [worker_count: 20])

    {:ok, client: client, port: server_port}
  end

  thrift_test "it can return a simple boolean value", ctx do
    assert {:ok, true} == Client.Framed.ping(ctx.client)
  end

  thrift_test "it can throw checked exceptions", ctx do
    expected_exception = %TestException{message: "Oh noes!", code: 400}
    assert {:error, {:exception, expected_exception}} == Client.Framed.checked_exception(ctx.client)
  end

  thrift_test "it can throw many checked exceptions", ctx do
    e1 = %TestException{message: "BOOM", code: 124}
    e2 = %UserNotFound{message: "Not here!"}
    e3 = %OtherException{message: "This is the other"}

    assert {:ok, true} == Client.Framed.multiple_exceptions(ctx.client, 0)
    assert {:error, {:exception, e1}} == Client.Framed.multiple_exceptions(ctx.client, 1)
    assert {:error, {:exception, e2}} == Client.Framed.multiple_exceptions(ctx.client, 2)
    assert {:error, {:exception, e3}} == Client.Framed.multiple_exceptions(ctx.client, 3)
  end

  thrift_test "it can handle unexpected exceptions", ctx do
    expected_exception = %TAE{message: "Server error: This wasn't supposed to happen",
                              type: :internal_error}
    assert {:error, {:exception, expected_exception}} == Client.Framed.server_exception(ctx.client)
  end

  thrift_test "it can return nothing", ctx do
    {:ok, nil} = Client.Framed.returns_nothing(ctx.client)
  end

  thrift_test "it can return structs", ctx do
    id_and_name = %IdAndName{id: 1234, name: "stinky"}
    assert {:ok, ^id_and_name} = Client.Framed.echo_struct(ctx.client, id_and_name)
  end

  thrift_test "it can handle bogus data", ctx do
    {:ok, socket} = :gen_tcp.connect('localhost', ctx.port, [:binary, packet: 4, active: false])
    :ok = :gen_tcp.send(socket, <<1, 2, 3, 4, 5>>)

    assert {:error, :closed}  == :gen_tcp.recv(socket, 0)
    assert {:ok, true} = Client.Framed.ping(ctx.client)
  end

  thrift_test "it can handle oneway messages", ctx do
    assert {:ok, nil} = Client.Framed.do_async(ctx.client, "my message")

    :timer.sleep 100

    assert "my message" = Agent.get(:server_args, &(&1))
  end

  thrift_test "camel cased functions are converted to underscore", ctx do
    assert {:ok, 2421} == Client.Framed.my_camel_cased_function(ctx.client, "username")

    :timer.sleep 100
    assert "username" == Agent.get(:server_args, &(&1))
  end
end
