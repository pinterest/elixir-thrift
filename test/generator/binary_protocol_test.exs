defmodule Thrift.Generator.BinaryProtocolTest do
  use ThriftTestCase

  @thrift_file name: "struct.thrift", contents: """
  struct MyStruct {
    1: optional string name = "asdf";
    2: optional byte num8 = 8;
    3: optional i16 num16 = 16;
    4: optional i32 num32 = 32;
    5: optional i64 num64 = 64;
    6: optional bool b1;
  }
  """

  thrift_test "generating struct" do
    struct = %MyStruct{}
    binary = MyStruct.BinaryProtocol.serialize(:struct, struct) |> :erlang.iolist_to_binary
    assert binary == <<
      # string field 1 = "asdf"
      11, 0, 1, 0, 0, 0, 4, ?a, ?s, ?d, ?f,

      # i32 field 2 = 8
      3, 0, 2, 8,

      # i32 field 3 = 16
      6, 0, 3, 0, 16,

      # i32 field 4 = 32
      8, 0, 4, 0, 0, 0, 32,

      # i64 field 5 = 64
      10, 0, 5, 0, 0, 0, 0, 0, 0, 0, 64,

      # bool field 6 = false, don't serialize default
      # 2, 0, 6, 0,

      # stop
      0>>

    assert {^struct, ""} = MyStruct.BinaryProtocol.deserialize(binary)
  end

  @thrift_file name: "lists.thrift", contents: """
  struct StructElement {
    1: optional i32 num;
  }
  struct Lists {
    1: optional list<bool> list_of_bool;
    2: optional list<byte> list_of_byte;
    3: optional list<double> list_of_double;
    4: optional list<i16> list_of_i16;
    5: optional list<i32> list_of_i32;
    6: optional list<i64> list_of_i64;
    7: optional list<string> list_of_string;
    # 8: optional list<StructElement> list_of_structs;
    9: optional list<map<string, i32>> list_of_maps;
    10: optional list<set<string>> list_of_sets;
    11: optional list<list<i32>> list_of_lists_of_i32;
    12: optional list<list<list<i32>>> list_of_lists_of_lists_of_i32;
  }
  """

  thrift_test "lists" do
    struct = %Lists{
      list_of_bool: [true, false, true],
      list_of_byte: [3, 4, 5, 0],
      list_of_double: [0.0, 1.0, 2.0],
      list_of_i16: [4, 5, 6, 7],
      list_of_i32: [40, 50, 60],
      list_of_i64: [500, 600],
      list_of_string: ["", "a", "bb", "ccc"],
      # TODO: Support lists of structs.
      # list_of_structs: [%{StructElement.new | num: 1}, %{StructElement.new | num: 2}],
      list_of_maps: [%{"a" => 1, "b" => 2}, %{"c" => 3}],
      list_of_sets: [MapSet.new(["a", "b"]), MapSet.new(["c"])],
      list_of_lists_of_i32: [[], [1], [2, 3, 4]],
      list_of_lists_of_lists_of_i32: [[], [[], [1, 2, 3]]],
    }
    binary = Lists.BinaryProtocol.serialize(:struct, struct) |> :erlang.iolist_to_binary
    assert {^struct, ""} = Lists.BinaryProtocol.deserialize(binary)
  end
end
