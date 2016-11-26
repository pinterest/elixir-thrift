defmodule Thrift.Generator.BinaryProtocolTest do
  use ThriftTestCase

  alias Thrift.Protocols.Binary

  def assert_serializes(struct=%{__struct__: mod}, binary) do
    assert binary == Binary.serialize(:struct, struct) |> IO.iodata_to_binary
    assert {^struct, ""} = mod.deserialize(binary)
  end

  def assert_serializes(struct=%{__struct__: mod}, binary, deserialized_struct=%{__struct__: mod}) do
    assert binary == Binary.serialize(:struct, struct) |> IO.iodata_to_binary
    assert {^deserialized_struct, ""} = mod.deserialize(binary)
  end

  @thrift_file name: "bool.thrift", contents: """
  struct Bool {
    1: bool val;
    2: map<bool, bool> val_map;
    3: set<bool> val_set;
    4: list<bool> val_list;
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
    1: byte val;
    2: map<byte, byte> val_map;
    3: set<byte> val_set;
    4: list<byte> val_list;
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

  @thrift_file name: "i16.thrift", contents: """
  struct I16 {
    1: i16 val;
    2: map<i16, i16> val_map;
    3: set<i16> val_set;
    4: list<i16> val_list;
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

  @thrift_file name: "string.thrift", contents: """
  struct String {
    1: string val;
    2: map<string, string> val_map;
    3: set<string> val_set;
    4: list<string> val_list;
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
    99: byte num;
  }
  struct Struct {
    1: Val val;
    2: map<Val, Val> val_map;
    3: set<Val> val_set;
    4: list<Val> val_list;
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
    # assert_serializes %Struct{val_list: ["abc", "def"]},      <<15, 0, 4, 11, 0, 0, 0, 2, 0, 0, 0, 3, "abc", 0, 0, 0, 3, "def", 0>>
  end

  @thrift_file name: "composite.thrift", contents: """
  struct Composite {
    1: map<map<byte, byte>, map<byte, byte>> map_of_maps;
    2: map<set<byte>, set<byte>> map_of_sets;
    3: map<list<byte>, list<byte>> map_of_lists;
    4: set<map<byte, byte>> set_of_maps;
    5: set<set<byte>> set_of_sets;
    6: set<list<byte>> set_of_lists;
    7: list<map<byte, byte>> list_of_maps;
    8: list<set<byte>> list_of_sets;
    9: list<list<byte>> list_of_lists;
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
end
