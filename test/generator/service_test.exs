defmodule Thrift.Generator.ServiceTest do
  use ThriftTestCase, gen_erl: true

  @thrift_file name: "simple_service.thrift", contents: """
  namespace elixir Services.Simple
  struct User {
    1: i64 id,
    2: string username
  }

  exception UsernameTakenException {
    1: string message
  }

  service SimpleService {
    bool ping(),
    bool update_username(1: i64 id, 2: string new_username)
      throws(1: UsernameTakenException taken),
    User get_by_id(1: i64 user_id),
    bool are_friends(1: User user_a, 2: User user_b),
    void mark_inactive(1: i64 user_id),
    oneway void do_some_work(1: string work),
    list<i64> friend_ids_of(1: i64 user_id),
    map<string, i64> friend_nicknames(1: i64 user_id),
    set<string> tags(1: i64 user_id),
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

    def handle_call(:get_reply, _, {reply, _}=state), do: {:reply, reply, state}

    def handle_call({:set_args, args}, _from, {reply, _}) do
      {:reply, :ok, {reply, args}}
    end

    def handle_call(:get_args, _, {_, args}=state), do: {:reply, args, state}

    def handle_function(name, args) do
      reply = ServerSpy.get_reply
      ServerSpy.set_args({name, args})
      case reply do
        {:exception, e} ->
          throw e

        :noreply ->
          :ok

        reply ->
          {:reply, reply}
      end
    end
  end

  alias Thrift.TApplicationException
  alias Thrift.Generator.ServiceTest.User
  alias Thrift.Generator.ServiceTest.SimpleService.Client.Framed, as: FramedClient
  alias Thrift.Generator.ServiceTest.UsernameTakenException

  setup do
    port = :erlang.unique_integer([:positive, :monotonic]) + 10000

    {:ok, server} = :thrift_socket_server.start(
      handler: ServerSpy,
      port: port,
      framed: true,
      service: :simple_service_thrift)

    {:ok, client} = FramedClient.start_link("127.0.0.1", port, [], 5000)
    {:ok, handler_pid} = ServerSpy.start_link

    on_exit fn ->
      [handler_pid, client, server]
      |> Enum.each(fn pid ->
        ref = Process.monitor(pid)
        Process.exit(pid, :normal)
        receive do
          {:DOWN, ^ref, _, _, _} ->
            :ok
        end
      end)
    end

    {:ok, port: port, client: client}
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

    {:ok, true} = FramedClient.ping(ctx.client)

    assert {:ping, {}} == ServerSpy.get_args
  end

  thrift_test "it should be able to update the username", ctx do
    ServerSpy.set_reply(true)

    assert {:ok, true} = FramedClient.update_username(ctx.client, 1234, "stinkypants")
    assert {:update_username, {1234, "stinkypants"}} == ServerSpy.get_args

  end

  thrift_test "it should be able to handle structs as function arguments", ctx do
    ServerSpy.set_reply(true)

    assert {:ok, true} = FramedClient.are_friends(ctx.client,
                                                  %User{id: 1, username: "stinky"},
                                                  %User{id: 28, username: "less_stinky"})

    expected_args = {:are_friends, {{:User, 1, "stinky"}, {:User, 28, "less_stinky"}}}
    assert expected_args == ServerSpy.get_args
  end

  thrift_test "it should be able to return structs", ctx do
    ServerSpy.set_reply({:User, 28392, 'stinky'})

    {:ok, user} = FramedClient.get_by_id(ctx.client, 12345)

    assert %User{id: 28392, username: "stinky"} == user
  end

  thrift_test "it should handle expected exceptions", ctx do
    ServerSpy.set_reply({:exception, {:UsernameTakenException, 'oh noes'}})

    {:error, {:exception, exc}} = FramedClient.update_username(ctx.client, 8382, "dude")

    assert %UsernameTakenException{message: "oh noes"} == exc
  end

  thrift_test "it handles a TApplicationException when the server blows up", ctx do
    ServerSpy.set_reply(:this_isnt_a_valid_reply)

    {:error, {:exception, ex}} = FramedClient.update_username(ctx.client, 1234, "user")
    assert %TApplicationException{message: "An unknown handler error occurred.", type: :unknown} == ex
  end

  thrift_test "bang functions return only the value", ctx do
    ServerSpy.set_reply({:User, 2, 'stinky'})

    assert %User{id: 2, username: "stinky"} == FramedClient.get_by_id!(ctx.client, 1234)
  end

  thrift_test "it raises a server exception with the bang function", ctx do
    ServerSpy.set_reply("oh noes")

    assert_raise TApplicationException, fn ->
      FramedClient.update_username!(ctx.client, 88, "blow up")
    end
  end

  thrift_test "it raises an exception with a bang function", ctx do
    ServerSpy.set_reply({:exception, {:UsernameTakenException, 'blowie up'}})

    assert_raise UsernameTakenException, fn ->
      FramedClient.update_username!(ctx.client, 821, "foo")
    end
  end

  thrift_test "it handles void functions", ctx do
    ServerSpy.set_reply(:noreply)

    assert {:ok, nil} == FramedClient.mark_inactive(ctx.client, 5529)
  end

  thrift_test "it handles void oneway functions", ctx do
    ServerSpy.set_reply(:noreply)

    assert {:ok, nil} == FramedClient.do_some_work(ctx.client, "12345")
  end

  thrift_test "it handles returning a list", ctx do
    friend_ids = [1, 82, 382, 9914, 40112]

    ServerSpy.set_reply(friend_ids)

    assert {:ok, friend_ids} == FramedClient.friend_ids_of(ctx.client, 14821)
  end

  thrift_test "it handles returning a map", ctx do
    ServerSpy.set_reply(:dict.from_list([{'bernie', 281}, {'brownie', 9924}]))

    expected = %{"bernie" => 281, "brownie" => 9924}
    assert {:ok, expected} == FramedClient.friend_nicknames(ctx.client, 4810)
  end

  thrift_test "it handles returning a set", ctx do
    ServerSpy.set_reply(:sets.from_list(['sports', 'debate', 'motorcycles']))

    expected = MapSet.new(["sports", "debate", "motorcycles"])
    assert {:ok, expected} == FramedClient.tags(ctx.client, 91281)
  end
end
