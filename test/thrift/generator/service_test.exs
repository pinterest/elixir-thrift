defmodule Thrift.Generator.ServiceTest do
  use ThriftTestCase, gen_erl: true

  @thrift_file name: "simple_service.thrift", contents: """
  namespace elixir Services.Simple
  struct User {
    1: optional i64 id,
    2: optional string username
  }

  exception UsernameTakenException {
    1: string message
  }

  service SimpleService {
    bool ping(),
    void check_username(1: string username)
      throws(1: UsernameTakenException taken),
    bool update_username(1: i64 id, 2: string new_username)
      throws(1: UsernameTakenException taken),
    User get_by_id(1: i64 user_id),
    bool are_friends(1: User user_a, 2: User user_b),
    void mark_inactive(1: i64 user_id),
    oneway void do_some_work(1: string work),
    list<i64> friend_ids_of(1: i64 user_id),
    map<string, i64> friend_nicknames(1: i64 user_id),
    set<string> tags(1: i64 user_id),
    bool And(1: bool left, 2: bool right),
  }
  """

  defmodule ServerSpy do
    use GenServer
    def init([]) do
      {:ok, {nil, nil}}
    end

    def start_link do
      GenServer.start_link(__MODULE__, [], name: __MODULE__)
    end

    def set_reply(reply), do: GenServer.call(__MODULE__, {:set_reply, reply})
    def set_args(args), do: GenServer.call(__MODULE__, {:set_args, args})

    def get_reply, do: GenServer.call(__MODULE__, :get_reply)
    def get_args, do: GenServer.call(__MODULE__, :get_args)

    def handle_call({:set_reply, reply}, _from, {_, args}) do
      {:reply, :ok, {reply, args}}
    end

    def handle_call(:get_reply, _, {reply, _} = state), do: {:reply, reply, state}

    def handle_call({:set_args, args}, _from, {reply, _}) do
      {:reply, :ok, {reply, args}}
    end

    def handle_call(:get_args, _, {_, args} = state), do: {:reply, args, state}

    def handle_function(name, args) do
      reply = ServerSpy.get_reply
      ServerSpy.set_args({name, args})

      parse_reply(reply)
    end

    defp parse_reply(reply) do
      case reply do
        {:exception, e} ->
          throw e

        :noreply ->
          :ok
        {:sleep, amount, reply} ->
          :timer.sleep(amount)
          parse_reply(reply)
        reply ->
          {:reply, reply}
      end
    end
  end

  alias Thrift.TApplicationException
  alias Thrift.Generator.ServiceTest.User
  alias Thrift.Generator.ServiceTest.SimpleService.Binary.Framed.Client
  alias Thrift.Generator.ServiceTest.UsernameTakenException

  defp start_server(recv_timeout \\ :infinity) do
    opts = [
      port: 0,
      service: :simple_service_thrift,
      handler: ServerSpy,
      socket_opts: [recv_timeout: recv_timeout],
      framed: true]

    case :thrift_socket_server.start(opts) do
      {:ok, pid} ->
        port = GenServer.call(pid, {:get, :port})
        {:ok, pid, port}
      error ->
        error
    end
  end

  defp stop_server(server_pid) when is_pid(server_pid) do
    ref = Process.monitor(server_pid)
    :thrift_socket_server.stop(server_pid)
    assert_receive {:DOWN, ^ref, _, _, _}
  end

  defp wait_for_exit(pid) when is_pid(pid) do
    ref = Process.monitor(pid)
    assert_receive {:DOWN, ^ref, _, _, _}
  end

  setup do
    {:ok, server, port} = start_server()
    {:ok, client} = Client.start_link('127.0.0.1', port,
                                            [tcp_opts: [timeout: 5000]])
    {:ok, handler_pid} = ServerSpy.start_link

    on_exit fn ->
      wait_for_exit(handler_pid)
      wait_for_exit(client)
      wait_for_exit(server)
    end

    {:ok, port: port, client: client, server: server}
  end

  thrift_test "it should generate arg structs" do
    find_by_id_args = %SimpleService.UpdateUsernameArgs{id: 1234, new_username: "foobar"}
    assert find_by_id_args.id == 1234
    assert find_by_id_args.new_username == "foobar"
  end

  thrift_test "it should generate response structs" do
    response = %SimpleService.UpdateUsernameResponse{success: true}
    assert :success in Map.keys(response)
  end

  thrift_test "it should generate exceptions in response structs" do
    assert :taken in Map.keys(%SimpleService.UpdateUsernameResponse{})
  end

  thrift_test "it should be able to serialize the request struct" do
    alias SimpleService.UpdateUsernameArgs

    serialized = %UpdateUsernameArgs{id: 1234, new_username: "stinkypants"}
    |> UpdateUsernameArgs.BinaryProtocol.serialize
    |> IO.iodata_to_binary

    assert <<10, 0, 1, 0, 0, 0, 0, 0, 0, 4, 210, 11, 0, 2, 0, 0, 0, 11, "stinkypants", 0>> == serialized
  end

  thrift_test "it should be able to serialize the response struct" do
    alias SimpleService.UpdateUsernameResponse
    serialized = %UpdateUsernameResponse{success: true}
    |> UpdateUsernameResponse.BinaryProtocol.serialize
    |> IO.iodata_to_binary

    assert <<2, 0, 0, 1, 0>> == serialized
  end

  thrift_test "it serializes the exceptions" do
    # this is because the python client always expects the success struct.
    # silly python client.

    alias SimpleService.UpdateUsernameResponse

    serialized = %UpdateUsernameResponse{taken: %UsernameTakenException{message: "That username is taken"}}
    |> UpdateUsernameResponse.BinaryProtocol.serialize
    |> IO.iodata_to_binary

    assert <<12, 0, 1, 11, 0, 1, 0, 0, 0, 22, rest::binary>> = serialized
    assert <<"That username is taken", 0, 0>> = rest
  end

  thrift_test "it should be able to execute a simple ping", ctx do
    ServerSpy.set_reply(true)

    {:ok, true} = Client.ping(ctx.client)

    assert {:ping, {}} == ServerSpy.get_args
  end

  thrift_test "it should be able to update the username", ctx do
    ServerSpy.set_reply(true)

    assert {:ok, true} = Client.update_username(ctx.client, 1234, "stinkypants")
    assert {:update_username, {1234, "stinkypants"}} == ServerSpy.get_args

  end

  thrift_test "it should be able to handle structs as function arguments", ctx do
    ServerSpy.set_reply(true)

    assert {:ok, true} = Client.are_friends(ctx.client,
                                                  %User{id: 1, username: "stinky"},
                                                  %User{id: 28, username: "less_stinky"})

    expected_args = {:are_friends, {{:User, 1, "stinky"}, {:User, 28, "less_stinky"}}}
    assert expected_args == ServerSpy.get_args
  end

  thrift_test "it should be able to return structs", ctx do
    ServerSpy.set_reply({:User, 28_392, 'stinky'})

    {:ok, user} = Client.get_by_id(ctx.client, 12_345)

    assert %User{id: 28_392, username: "stinky"} == user
  end

  thrift_test "it should handle expected exceptions", ctx do
    ServerSpy.set_reply({:exception, {:UsernameTakenException, 'oh noes'}})

    {:error, {:exception, exc}} = Client.update_username(ctx.client, 8382, "dude")

    assert %UsernameTakenException{message: "oh noes"} == exc
  end

  thrift_test "it handles a TApplicationException when the server blows up", ctx do
    ServerSpy.set_reply(:this_isnt_a_valid_reply)

    {:error, {:exception, ex}} = Client.update_username(ctx.client, 1234, "user")
    assert %TApplicationException{
      message: "An unknown handler error occurred.",
      type: :unknown} = ex
  end

  thrift_test "bang functions return only the value", ctx do
    ServerSpy.set_reply({:User, 2, 'stinky'})

    assert %User{id: 2, username: "stinky"} == Client.get_by_id!(ctx.client, 1234)
  end

  thrift_test "it raises a server exception with the bang function", ctx do
    ServerSpy.set_reply("oh noes")

    assert_raise TApplicationException, fn ->
      Client.update_username!(ctx.client, 88, "blow up")
    end
  end

  thrift_test "it raises an exception with a bang function", ctx do
    ServerSpy.set_reply({:exception, {:UsernameTakenException, 'blowie up'}})

    assert_raise UsernameTakenException, fn ->
      Client.update_username!(ctx.client, 821, "foo")
    end
  end

  thrift_test "it handles void functions", ctx do
    ServerSpy.set_reply(:noreply)

    assert {:ok, nil} == Client.mark_inactive(ctx.client, 5529)
  end

  thrift_test "it handles void functions that can throw exceptions", ctx do
    ServerSpy.set_reply(:noreply)
    assert {:ok, nil} == Client.check_username(ctx.client, "foo")
  end

  thrift_test "it handles void functions that do throw exceptions", ctx do
    ServerSpy.set_reply({:exception, {:UsernameTakenException, 'blowie up'}})
    assert_raise UsernameTakenException, fn ->
      Client.check_username!(ctx.client, "foo")
    end
  end

  thrift_test "it handles void oneway functions", ctx do
    ServerSpy.set_reply(:noreply)

    assert {:ok, nil} == Client.do_some_work(ctx.client, "12345")
    :timer.sleep(10)
  end

  thrift_test "it handles returning a list", ctx do
    friend_ids = [1, 82, 382, 9914, 40_112]

    ServerSpy.set_reply(friend_ids)

    assert {:ok, friend_ids} == Client.friend_ids_of(ctx.client, 14_821)
  end

  thrift_test "it handles returning a map", ctx do
    ServerSpy.set_reply(:dict.from_list([{'bernie', 281}, {'brownie', 9924}]))

    expected = %{"bernie" => 281, "brownie" => 9924}
    assert {:ok, expected} == Client.friend_nicknames(ctx.client, 4810)
  end

  thrift_test "it handles returning a set", ctx do
    ServerSpy.set_reply(:sets.from_list(['sports', 'debate', 'motorcycles']))

    expected = MapSet.new(["sports", "debate", "motorcycles"])
    assert {:ok, expected} == Client.tags(ctx.client, 91_281)
  end

  thrift_test "it handles method names that conflict with Elixir keywords", ctx do
    ServerSpy.set_reply(true)

    expected = true
    assert {:ok, expected} == Client.and(ctx.client, true, true)
  end

  thrift_test "it has a configurable gen_server timeout", ctx do
    ServerSpy.set_reply({:sleep, 1000, [1, 3, 4]})

    assert catch_exit(
      Client.friend_ids_of(ctx.client, 1234, [gen_server_opts: [timeout: 200]])
    )
  end

  thrift_test "it has a configurable socket timeout", %{client: client} do
    Process.flag(:trap_exit, true)
    ServerSpy.set_reply({:sleep, 1000, [1, 3, 4]})

    assert ExUnit.CaptureLog.capture_log(fn ->
      assert {:error, :timeout} = Client.friend_ids_of(client, 12_914, [tcp_opts: [timeout: 1]])
      assert_receive {:EXIT, ^client, {:error, :timeout}}
    end) =~ "1ms"
  end

  # connection tests

  thrift_test "clients can be closed", ctx do
    ref = Process.monitor(ctx.client)
    :ok = Client.close(ctx.client)

    assert_receive {:DOWN, ^ref, _, _, _}
    refute Process.alive?(ctx.client)
  end

  thrift_test "clients exit on connection timeout" do
    Process.flag(:trap_exit, true)

    {:ok, new_server, new_server_port} = start_server(20)
    {:ok, client} = Client.start_link("127.0.0.1", new_server_port, [])
    :timer.sleep(50) # sleep beyond the server's recv_timeout

    assert Client.friend_ids_of(client, 1234) == {:error, :closed}
    assert_receive {:EXIT, ^client, {:error, :closed}}

    on_exit fn ->
      stop_server(new_server)
    end
  end

  thrift_test "clients exit if they try to use a closed client", ctx do
    Process.flag(:trap_exit, true)
    stop_server(ctx.server)
    assert {{:error, :econnrefused}, _} = catch_exit(Client.friend_ids_of(ctx.client, 1234))
  end

  thrift_test "clients exit if the server dies handling a message", ctx do
    ref = Process.monitor(ctx.server)

    ServerSpy.set_reply({:sleep, 5000, 1234})

    # the server will sleep when the RPC is called, so spawn a
    # process to make a request, then kill the server out
    # from under that process. It will trigger the generic
    # error handler in the server
    me = self()
    spawn fn ->
      Process.flag(:trap_exit, true)
      Process.send_after(me, :ok, 20)
      Client.friend_ids_of(ctx.client, 14_821)
    end

    receive do
      :ok ->
        :ok
    end
    Process.flag(:trap_exit, true)
    Process.exit(ctx.server, :kill)

    client = ctx.client
    assert_receive {:DOWN, ^ref, _, _, _}
    assert_receive {:EXIT, ^client, {:error, :closed}}
  end

  thrift_test "oneway functions return :ok if the server dies", %{client: client, server: server} do
    Process.flag(:trap_exit, true)
    ServerSpy.set_reply(:noreply)

    :sys.get_state(client)
    stop_server(server)

    # this assertion is unusual, as it should exit, but the server
    # doesn't do reads during oneway functions, so it won't get the
    # error that the other side has been closed.
    # see: http://erlang.org/pipermail/erlang-questions/2014-April/078545.html
    {:ok, nil} = Client.do_some_work(client, "work!")
    assert_receive {:EXIT, ^client, {:error, :closed}}
  end
end
