defmodule ServiceTest do
  use ThriftTestCase

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
    bool update_username(1: i64 id, 2: string new_username)
      throws(1: UsernameTakenException taken),
  }
  """

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
end
