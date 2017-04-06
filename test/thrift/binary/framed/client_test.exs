defmodule BinaryFramedClientTest do
  use ThriftTestCase

  alias Thrift.Binary.Framed.Client
  alias Thrift.TApplicationException

  @thrift_file name: "void_return.thrift", contents: """
  service VoidReturns {
    void my_call(1: i64 id)
  }
  """

  thrift_test "it should be able to deserialize an invalid message" do
    msg = <<128, 1, 0, 2>>
    assert {:error, {:cant_decode_message, ^msg}} = Client.deserialize_message_reply(msg, "my_call", 2757)
  end

  thrift_test "it should be able to read a malformed TApplicationException" do
    begin = Thrift.Protocol.Binary.serialize(:message_begin, {:exception, 941, "bad"})
    msg = begin <> <<1, 1, 1, 1>>

    assert {:error, {:exception, ex}} = Client.deserialize_message_reply(msg, "bad", 941)
    assert %TApplicationException{message: "Unable to decode exception (<<1, 1, 1, 1>>)", type: :protocol_error} = ex
  end

  thrift_test "it should be able to deserialize a message with a bad sequence id" do
    msg = <<128, 1, 0, 2, 0, 0, 0, 7, "my_call", 0, 0, 10, 197, 0>>

    assert {:error, {:exception, ex}} = Client.deserialize_message_reply(msg, "my_call", 1912)
    assert %TApplicationException{type: :bad_sequence_id} = ex
  end

  thrift_test "it should be able to deserialize a message with the wrong method name" do
    msg = <<128, 1, 0, 2, 0, 0, 0, 8, "bad_call", 0, 0, 10, 197, 0>>

    assert {:error, {:exception, ex}} = Client.deserialize_message_reply(msg, "my_call", 2757)
    assert %TApplicationException{type: :wrong_method_name} = ex
  end

  thrift_test "it should be able to deserialize a message with the wrong method name and sequence id " do
    msg = <<128, 1, 0, 2, 0, 0, 0, 8, "bad_call", 0, 0, 10, 197, 0>>

    assert {:error, {:exception, ex}} = Client.deserialize_message_reply(msg, "my_call", 1234)
    assert %TApplicationException{type: :bad_sequence_id} = ex
  end

  thrift_test "it should be able to deserialize a void message" do
    msg = <<128, 1, 0, 2, 0, 0, 0, 7, "my_call", 0, 0, 10, 197, 0>>

    assert {:ok,  <<0>>} =  Client.deserialize_message_reply(msg, "my_call", 2757)
  end

  thrift_test "it should be able to deserialize a message with an empty struct" do
    msg = <<128, 1, 0, 2, 0, 0, 0, 7, "my_call", 0, 0, 10, 197, 12, 0, 0, 0, 0>>

    assert {:ok, <<12, 0, 0, 0, 0>>} =  Client.deserialize_message_reply(msg, "my_call", 2757)
  end
end
