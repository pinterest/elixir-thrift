defmodule Thrift.Generator.BinaryProtocolTest do
  use ThriftTestCase

  alias Thrift.Protocol.Binary
  alias Thrift.Union.TooManyFieldsSetException

  def assert_serializes(%{__struct__: mod} = struct, binary) do
    assert binary == Binary.serialize(:struct, struct) |> IO.iodata_to_binary
    assert {^struct, ""} = mod.deserialize(binary)

    # If we randomly mutate any byte in the binary, it may deserialize to a
    # struct of the proper type, or it may return :error. But it should never
    # raise.
    for i <- 1..byte_size(binary) do
      mutated_binary = binary
      |> :binary.bin_to_list
      |> List.replace_at(i - 1, :rand.uniform(256) - 1)
      |> :binary.list_to_bin

      case mod.deserialize(mutated_binary) do
        {%{__struct__: ^mod}, _} -> :ok
        :error -> :ok
      end
    end
  end

  def assert_serializes(%{__struct__: mod} = struct, binary, %{__struct__: mod} = deserialized_struct) do
    assert binary == Binary.serialize(:struct, struct) |> IO.iodata_to_binary
    assert {^deserialized_struct, ""} = mod.deserialize(binary)
  end

  @thrift_file name: "bool.thrift", contents: """
  struct Bool {
    1: optional bool val;
    2: optional map<bool, bool> val_map;
    3: optional set<bool> val_set;
    4: optional list<bool> val_list;
  }
  """

  thrift_test "bool serialization" do
    assert_serializes %Bool{},                            <<0>>
    assert_serializes %Bool{val: false},                  <<2, 0, 1, 0, 0>>
    assert_serializes %Bool{val: true},                   <<2, 0, 1, 1, 0>>
    assert_serializes %Bool{val_map: %{}},                <<13, 0, 2, 2, 2, 0, 0, 0, 0, 0>>
    assert_serializes %Bool{val_map: %{false => true}},   <<13, 0, 2, 2, 2, 0, 0, 0, 1, 0, 1, 0>>
    assert_serializes %Bool{val_set: MapSet.new},         <<14, 0, 3, 2, 0, 0, 0, 0, 0>>
    assert_serializes %Bool{val_set: MapSet.new([true])}, <<14, 0, 3, 2, 0, 0, 0, 1, 1, 0>>
    assert_serializes %Bool{val_list: []},                <<15, 0, 4, 2, 0, 0, 0, 0, 0>>
    assert_serializes %Bool{val_list: [true]},            <<15, 0, 4, 2, 0, 0, 0, 1, 1, 0>>
    assert_serializes %Bool{val_list: [true, false]},     <<15, 0, 4, 2, 0, 0, 0, 2, 1, 0, 0>>
  end

  @thrift_file name: "byte.thrift", contents: """
  struct Byte {
    1: optional byte val;
    2: optional map<byte, byte> val_map;
    3: optional set<byte> val_set;
    4: optional list<byte> val_list;
  }
  """

  thrift_test "byte serialization" do
    assert_serializes %Byte{},                          <<0>>
    assert_serializes %Byte{val: 0},                    <<3, 0, 1, 0, 0>>
    assert_serializes %Byte{val: 1},                    <<3, 0, 1, 1, 0>>
    assert_serializes %Byte{val: 255},                  <<3, 0, 1, 255, 0>>
    assert_serializes %Byte{val: 256},                  <<3, 0, 1, 0, 0>>, %Byte{val: 0}
    assert_serializes %Byte{val_map: %{}},              <<13, 0, 2, 3, 3, 0, 0, 0, 0, 0>>
    assert_serializes %Byte{val_map: %{91 => 92}},      <<13, 0, 2, 3, 3, 0, 0, 0, 1, 91, 92, 0>>
    assert_serializes %Byte{val_set: MapSet.new},       <<14, 0, 3, 3, 0, 0, 0, 0, 0>>
    assert_serializes %Byte{val_set: MapSet.new([91])}, <<14, 0, 3, 3, 0, 0, 0, 1, 91, 0>>
    assert_serializes %Byte{val_list: []},              <<15, 0, 4, 3, 0, 0, 0, 0, 0>>
    assert_serializes %Byte{val_list: [91]},            <<15, 0, 4, 3, 0, 0, 0, 1, 91, 0>>
    assert_serializes %Byte{val_list: [91, 92]},        <<15, 0, 4, 3, 0, 0, 0, 2, 91, 92, 0>>
    assert_serializes %Byte{val_list: [91, 92, 93]},    <<15, 0, 4, 3, 0, 0, 0, 3, 91, 92, 93, 0>>
  end

  @thrift_file name: "double.thrift", contents: """
  struct Double {
    1: optional double val;
    2: optional map<double, double> val_map;
    3: optional set<double> val_set;
    4: optional list<double> val_list;
  }
  """

  thrift_test "double serialization" do
    assert_serializes %Double{},                            <<0>>
    assert_serializes %Double{val: 0.0},                    <<4, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
    assert_serializes %Double{val: 1.0},                    <<4, 0, 1, 1::signed-float, 0>>
    assert_serializes %Double{val: 255.0},                  <<4, 0, 1, 255::signed-float, 0>>
    assert_serializes %Double{val_map: %{}},                <<13, 0, 2, 4, 4, 0, 0, 0, 0, 0>>
    assert_serializes %Double{val_map: %{91.0 => 92.0}},    <<13, 0, 2, 4, 4, 0, 0, 0, 1, 91::signed-float, 92::signed-float, 0>>
    assert_serializes %Double{val_set: MapSet.new},         <<14, 0, 3, 4, 0, 0, 0, 0, 0>>
    assert_serializes %Double{val_set: MapSet.new([91.0])}, <<14, 0, 3, 4, 0, 0, 0, 1, 91::signed-float, 0>>
    assert_serializes %Double{val_list: []},                <<15, 0, 4, 4, 0, 0, 0, 0, 0>>
    assert_serializes %Double{val_list: [91.0]},            <<15, 0, 4, 4, 0, 0, 0, 1, 91::signed-float, 0>>
  end

  @thrift_file name: "i16.thrift", contents: """
  struct I16 {
    1: optional i16 val;
    2: optional map<i16, i16> val_map;
    3: optional set<i16> val_set;
    4: optional list<i16> val_list;
  }
  """

  thrift_test "i16 serialization" do
    assert_serializes %I16{},                           <<0>>
    assert_serializes %I16{val: 0},                     <<6, 0, 1, 0, 0, 0>>
    assert_serializes %I16{val: 1},                     <<6, 0, 1, 0, 1, 0>>
    assert_serializes %I16{val: 255},                   <<6, 0, 1, 0, 255, 0>>
    assert_serializes %I16{val: 256},                   <<6, 0, 1, 1, 0, 0>>
    assert_serializes %I16{val: 65535},                 <<6, 0, 1, 255, 255, 0>>
    assert_serializes %I16{val: 65536},                 <<6, 0, 1, 0, 0, 0>>, %I16{val: 0}
    assert_serializes %I16{val_map: %{}},               <<13, 0, 2, 6, 6, 0, 0, 0, 0, 0>>
    assert_serializes %I16{val_map: %{91 => 92}},       <<13, 0, 2, 6, 6, 0, 0, 0, 1, 0, 91, 0, 92, 0>>
    assert_serializes %I16{val_set: MapSet.new},        <<14, 0, 3, 6, 0, 0, 0, 0, 0>>
    assert_serializes %I16{val_set: MapSet.new([91])},  <<14, 0, 3, 6, 0, 0, 0, 1, 0, 91, 0>>
    assert_serializes %I16{val_list: []},               <<15, 0, 4, 6, 0, 0, 0, 0, 0>>
    assert_serializes %I16{val_list: [91]},             <<15, 0, 4, 6, 0, 0, 0, 1, 0, 91, 0>>
    assert_serializes %I16{val_list: [91, 92]},         <<15, 0, 4, 6, 0, 0, 0, 2, 0, 91, 0, 92, 0>>
    assert_serializes %I16{val_list: [91, 92, 93]},     <<15, 0, 4, 6, 0, 0, 0, 3, 0, 91, 0, 92, 0, 93, 0>>
  end

  @thrift_file name: "i32.thrift", contents: """
  struct I32 {
    1: optional i32 val;
    2: optional map<i32, i32> val_map;
    3: optional set<i32> val_set;
    4: optional list<i32> val_list;
  }
  """

  thrift_test "i32 serialization" do
    assert_serializes %I32{},                           <<0>>
    assert_serializes %I32{val: 0},                     <<8, 0, 1, 0, 0, 0, 0, 0>>
    assert_serializes %I32{val: 1},                     <<8, 0, 1, 0, 0, 0, 1, 0>>
    assert_serializes %I32{val: 255},                   <<8, 0, 1, 0, 0, 0, 255, 0>>
    assert_serializes %I32{val: 256},                   <<8, 0, 1, 0, 0, 1, 0, 0>>
    assert_serializes %I32{val: 65535},                 <<8, 0, 1, 0, 0, 255, 255, 0>>
    assert_serializes %I32{val_map: %{}},               <<13, 0, 2, 8, 8, 0, 0, 0, 0, 0>>
    assert_serializes %I32{val_map: %{91 => 92}},       <<13, 0, 2, 8, 8, 0, 0, 0, 1, 0, 0, 0, 91, 0, 0, 0, 92, 0>>
    assert_serializes %I32{val_set: MapSet.new},        <<14, 0, 3, 8, 0, 0, 0, 0, 0>>
    assert_serializes %I32{val_set: MapSet.new([91])},  <<14, 0, 3, 8, 0, 0, 0, 1, 0, 0, 0, 91, 0>>
    assert_serializes %I32{val_list: []},               <<15, 0, 4, 8, 0, 0, 0, 0, 0>>
    assert_serializes %I32{val_list: [91]},             <<15, 0, 4, 8, 0, 0, 0, 1, 0, 0, 0, 91, 0>>
    assert_serializes %I32{val_list: [91, 92]},         <<15, 0, 4, 8, 0, 0, 0, 2, 0, 0, 0, 91, 0, 0, 0, 92, 0>>
  end

  @thrift_file name: "i64.thrift", contents: """
  struct I64 {
    1: optional i64 val;
    2: optional map<i64, i64> val_map;
    3: optional set<i64> val_set;
    4: optional list<i64> val_list;
  }
  """

  thrift_test "i64 serialization" do
    assert_serializes %I64{},                           <<0>>
    assert_serializes %I64{val: 0},                     <<10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
    assert_serializes %I64{val: 1},                     <<10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0>>
    assert_serializes %I64{val: 255},                   <<10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 255, 0>>
    assert_serializes %I64{val: 256},                   <<10, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0>>
    assert_serializes %I64{val: 65535},                 <<10, 0, 1, 0, 0, 0, 0, 0, 0, 255, 255, 0>>
    assert_serializes %I64{val_map: %{}},               <<13, 0, 2, 10, 10, 0, 0, 0, 0, 0>>
    assert_serializes %I64{val_map: %{91 => 92}},       <<13, 0, 2, 10, 10, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 91, 0, 0, 0, 0, 0, 0, 0, 92, 0>>
    assert_serializes %I64{val_set: MapSet.new},        <<14, 0, 3, 10, 0, 0, 0, 0, 0>>
    assert_serializes %I64{val_set: MapSet.new([91])},  <<14, 0, 3, 10, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 91, 0>>
    assert_serializes %I64{val_list: []},               <<15, 0, 4, 10, 0, 0, 0, 0, 0>>
    assert_serializes %I64{val_list: [91]},             <<15, 0, 4, 10, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 91, 0>>
    assert_serializes %I64{val_list: [91, 92]},         <<15, 0, 4, 10, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 91, 0, 0, 0, 0, 0, 0, 0, 92, 0>>
  end

  @thrift_file name: "string.thrift", contents: """
  struct String {
    1: optional string val;
    2: optional map<string, string> val_map;
    3: optional set<string> val_set;
    4: optional list<string> val_list;
  }
  """

  thrift_test "string serialization" do
    assert_serializes %String{},                              <<0>>
    assert_serializes %String{val: ""},                       <<11, 0, 1, 0, 0, 0, 0, 0>>
    assert_serializes %String{val: "abc"},                    <<11, 0, 1, 0, 0, 0, 3, "abc", 0>>
    assert_serializes %String{val_map: %{}},                  <<13, 0, 2, 11, 11, 0, 0, 0, 0, 0>>
    assert_serializes %String{val_map: %{"abc" => "def"}},    <<13, 0, 2, 11, 11, 0, 0, 0, 1, 0, 0, 0, 3, "abc", 0, 0, 0, 3, "def", 0>>
    assert_serializes %String{val_set: MapSet.new},           <<14, 0, 3, 11, 0, 0, 0, 0, 0>>
    assert_serializes %String{val_set: MapSet.new(["abc"])},  <<14, 0, 3, 11, 0, 0, 0, 1, 0, 0, 0, 3, "abc", 0>>
    assert_serializes %String{val_list: []},                  <<15, 0, 4, 11, 0, 0, 0, 0, 0>>
    assert_serializes %String{val_list: ["abc"]},             <<15, 0, 4, 11, 0, 0, 0, 1, 0, 0, 0, 3, "abc", 0>>
    assert_serializes %String{val_list: ["abc", "def"]},      <<15, 0, 4, 11, 0, 0, 0, 2, 0, 0, 0, 3, "abc", 0, 0, 0, 3, "def", 0>>
  end

  @thrift_file name: "struct.thrift", contents: """
  struct Val {
    99: optional byte num;
  }
  struct Struct {
    1: optional Val val;
    2: optional map<Val, Val> val_map;
    3: optional set<Val> val_set;
    4: optional list<Val> val_list;
  }
  """

  thrift_test "struct serialization" do
    assert_serializes %Struct{},                                      <<0>>
    assert_serializes %Struct{val: %Val{}},                           <<12, 0, 1, 0, 0>>
    assert_serializes %Struct{val: %Val{num: 91}},                    <<12, 0, 1, 3, 0, 99, 91, 0, 0>>
    assert_serializes %Struct{val_map: %{}},                          <<13, 0, 2, 12, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Struct{val_map: %{%Val{num: 91} => %Val{num: 92}}},
                                                                      <<13, 0, 2, 12, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 3, 0, 99, 92, 0, 0>>
    assert_serializes %Struct{val_set: MapSet.new},                   <<14, 0, 3, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Struct{val_set: MapSet.new([%Val{num: 91}])},  <<14, 0, 3, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 0>>
    assert_serializes %Struct{val_list: []},                          <<15, 0, 4, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Struct{val_list: [%Val{num: 91}]},             <<15, 0, 4, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 0>>
  end

  @thrift_file name: "unions.thrift", contents: """
  struct StructValue {
    1: optional string username;
  }
  union Union {
    1: i64 int_field,
    2: StructValue struct_field,
    3: string string_field,
    4: list<i16> list_field;
  }

  struct UStruct {
    1: optional Union my_union,
    2: optional map<Union, Union> u_map,
    3: optional set<Union> u_set,
    4: optional list<Union> u_list,
  }
  """
  thrift_test "union serialization" do
    assert_serializes %Union{},                                       <<0>>
    assert_serializes %Union{int_field: 205},                         <<10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 205, 0>>
    assert_serializes %Union{struct_field: %StructValue{username: "stinky"}},
                                                                      <<12, 0, 2, 11, 0, 1, 0, 0, 0, 6, 115, 116, 105, 110, 107, 121, 0, 0>>
    assert_serializes %Union{string_field: "hello"},                  <<11, 0, 3, 0, 0, 0, 5, "hello", 0>>
    assert_serializes %Union{list_field: [5, 9, 7]},                  <<15, 0, 4, 6, 0, 0, 0, 3, 0, 5, 0, 9, 0, 7, 0>>

    assert_raise TooManyFieldsSetException, fn ->
      Binary.serialize(:union, %Union{int_field: 205, list_field: [1, 2]})
    end
  end

  thrift_test "structs can have unions" do
    assert_serializes %UStruct{},                                     <<0>>
    assert_serializes %UStruct{my_union: %Union{int_field: 2}},       <<12, 0, 1, 10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0>>
    assert_serializes %UStruct{u_map: %{}},                           <<13, 0, 2, 12, 12, 0, 0, 0, 0, 0>>
    assert_serializes %UStruct{u_map: %{%Union{int_field: 23} => %Union{int_field: 33}}},
                                                                      <<13, 0, 2, 12, 12, 0, 0, 0, 1, 10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 23, 0, 10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0>>
    assert_serializes %UStruct{u_set: MapSet.new([%Union{int_field: 2239}])},
                                                                      <<14, 0, 3, 12, 0, 0, 0, 1, 10, 0, 1, 0, 0, 0, 0, 0, 0, 8, 191, 0, 0>>
    assert_serializes %UStruct{u_list: [%Union{int_field: 1291}]},    <<15, 0, 4, 12, 0, 0, 0, 1, 10, 0, 1, 0, 0, 0, 0, 0, 0, 5, 11, 0, 0>>

    assert_raise TooManyFieldsSetException, fn ->
      Binary.serialize(:struct,  %UStruct{my_union: %Union{int_field: 123, string_field: "oops"}})
    end
  end

  @thrift_file name: "exception.thrift", contents: """
  exception Ex {
     1: optional string message,
    99: optional byte num;
  }
  struct Exception {
    1: optional Ex val;
    2: optional map<Ex, Ex> val_map;
    3: optional set<Ex> val_set;
    4: optional list<Ex> val_list;
  }
  """

  thrift_test "exception serialization" do
    assert_serializes %Exception{},                                     <<0>>
    assert_serializes %Exception{val: %Ex{}},                           <<12, 0, 1, 0, 0>>
    assert_serializes %Exception{val: %Ex{num: 91}},                    <<12, 0, 1, 3, 0, 99, 91, 0, 0>>
    assert_serializes %Exception{val_map: %{}},                         <<13, 0, 2, 12, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Exception{val_map: %{%Ex{num: 91} => %Ex{num: 92}}},
                                                                        <<13, 0, 2, 12, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 3, 0, 99, 92, 0, 0>>
    assert_serializes %Exception{val_set: MapSet.new},                  <<14, 0, 3, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Exception{val_set: MapSet.new([%Ex{num: 91}])},  <<14, 0, 3, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 0>>
    assert_serializes %Exception{val_list: []},                         <<15, 0, 4, 12, 0, 0, 0, 0, 0>>
    assert_serializes %Exception{val_list: [%Ex{num: 91}]},             <<15, 0, 4, 12, 0, 0, 0, 1, 3, 0, 99, 91, 0, 0>>
  end

  @thrift_file name: "composite.thrift", contents: """
  struct Composite {
    1: optional map<map<byte, byte>, map<byte, byte>> map_of_maps;
    2: optional map<set<byte>, set<byte>> map_of_sets;
    3: optional map<list<byte>, list<byte>> map_of_lists;
    4: optional set<map<byte, byte>> set_of_maps;
    5: optional set<set<byte>> set_of_sets;
    6: optional set<list<byte>> set_of_lists;
    7: optional list<map<byte, byte>> list_of_maps;
    8: optional list<set<byte>> list_of_sets;
    9: optional list<list<byte>> list_of_lists;
  }
  """

  thrift_test "composite serialization" do
    assert_serializes %Composite{},                                           <<0>>
    assert_serializes %Composite{map_of_maps: %{}},                           <<13, 0, 1, 13, 13, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{map_of_maps: %{%{91 => 92} => %{93 => 94, 95 => 96}}},
                                                                              <<13, 0, 1, 13, 13, 0, 0, 0, 1, 3, 3, 0, 0, 0, 1, 91, 92, 3, 3, 0, 0, 0, 2, 93, 94, 95, 96, 0>>
    assert_serializes %Composite{map_of_sets: %{}},                           <<13, 0, 2, 14, 14, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{map_of_sets: %{MapSet.new([91]) => MapSet.new([92, 93])}},
                                                                              <<13, 0, 2, 14, 14, 0, 0, 0, 1, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
    assert_serializes %Composite{map_of_lists: %{}},                          <<13, 0, 3, 15, 15, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{map_of_lists: %{[91] => [92, 93]}},          <<13, 0, 3, 15, 15, 0, 0, 0, 1, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
    assert_serializes %Composite{set_of_maps: MapSet.new},                    <<14, 0, 4, 13, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{set_of_maps: MapSet.new([%{91 => 92}])},     <<14, 0, 4, 13, 0, 0, 0, 1, 3, 3, 0, 0, 0, 1, 91, 92, 0>>
    assert_serializes %Composite{set_of_sets: MapSet.new},                    <<14, 0, 5, 14, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{set_of_sets: MapSet.new([MapSet.new([91]), MapSet.new([92, 93])])},
                                                                              <<14, 0, 5, 14, 0, 0, 0, 2, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
    assert_serializes %Composite{set_of_lists: MapSet.new},                   <<14, 0, 6, 15, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{set_of_lists: MapSet.new([[91], [92, 93]])}, <<14, 0, 6, 15, 0, 0, 0, 2, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
    assert_serializes %Composite{list_of_maps: []},                           <<15, 0, 7, 13, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{list_of_maps: [%{91 => 92}]},                <<15, 0, 7, 13, 0, 0, 0, 1, 3, 3, 0, 0, 0, 1, 91, 92, 0>>
    assert_serializes %Composite{list_of_sets: []},                           <<15, 0, 8, 14, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{list_of_sets: [MapSet.new([91]), MapSet.new([92, 93])]},
                                                                              <<15, 0, 8, 14, 0, 0, 0, 2, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
    assert_serializes %Composite{list_of_lists: []},                          <<15, 0, 9, 15, 0, 0, 0, 0, 0>>
    assert_serializes %Composite{list_of_lists: [[91], [92, 93]]},            <<15, 0, 9, 15, 0, 0, 0, 2, 3, 0, 0, 0, 1, 91, 3, 0, 0, 0, 2, 92, 93, 0>>
  end

  @thrift_file name: "typedefs.thrift", contents: """
  typedef set<i32> intSet
  typedef map<string, string> mapping
  typedef list<i64> numList

  struct Typedefs {
    1: optional intSet ints,
    2: optional mapping mappings,
    3: optional numList numbers
  }
  """

  thrift_test "it should be able to serialize complex typedefs" do
    assert_serializes %Typedefs{},                          <<0>>
    assert_serializes %Typedefs{ints: MapSet.new([1, 2])},  <<14, 0, 1, 8, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2, 0>>
    assert_serializes %Typedefs{mappings: %{"foo" => "bar", "baz" => "quux"}},
                                                            <<13, 0, 2, 11, 11, 0, 0, 0, 2, 0, 0, 0, 3, "baz", 0, 0, 0, 4, "quux", 0, 0, 0, 3, "foo", 0, 0, 0, 3, "bar", 0>>

    assert_serializes %Typedefs{numbers: [9, 32, 104]},     <<15, 0, 3, 10, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 104, 0>>
  end

  @thrift_file name: "requiredness.thrift", contents: """
  struct RequiredBool         { 1: required bool val }
  struct DefaultRequiredBool  { 1: bool val }
  struct OptionalBool         { 1: optional bool val }
  struct RequiredField        { 1: required string val }
  struct DefaultRequiredField { 1: string val }
  struct OptionalField        { 1: optional string val }
  """

  thrift_test "required boolean fields must not be nil during serialization" do
    assert_raise Thrift.MissingFieldException, "Required boolean field :val must be true or false", fn ->
      RequiredBool.serialize(%RequiredBool{})
    end
  end

  thrift_test "default required boolean fields must not be nil during serialization" do
    assert_raise Thrift.MissingFieldException, "Required boolean field :val must be true or false", fn ->
      DefaultRequiredBool.serialize(%DefaultRequiredBool{})
    end
  end

  thrift_test "optional boolean fields must not be nil during serialization" do
    assert OptionalBool.serialize(%OptionalBool{})
  end

  thrift_test "required fields must not be nil during serialization" do
    assert_raise Thrift.MissingFieldException, "Required field :val must not be nil", fn ->
      RequiredField.serialize(%RequiredField{})
    end
  end

  thrift_test "default required fields must not be nil during serialization" do
    assert_raise Thrift.MissingFieldException, "Required field :val must not be nil", fn ->
      DefaultRequiredField.serialize(%DefaultRequiredField{})
    end
  end

  thrift_test "optional fields must not be nil during serialization" do
    OptionalField.serialize(%OptionalField{})
  end
end
