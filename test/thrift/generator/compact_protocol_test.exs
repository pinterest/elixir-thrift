defmodule Thrift.Generator.CompactProtocolTest do
  use ThriftTestCase

  alias Thrift.Union.TooManyFieldsSetError

  # @thrift_test_opts [cleanup: false]

  def assert_serializes(%{__struct__: mod} = struct, binary) do
    assert binary == IO.iodata_to_binary(mod.serialize(struct, :compact))
    assert {^struct, ""} = mod.deserialize(binary, :compact)

    # If we randomly mutate any byte in the binary, it may deserialize to a
    # struct of the proper type, or it may return :error. But it should never
    # raise.

    for i <- 1..byte_size(binary) do
      mutated_binary =
        binary
        |> :binary.bin_to_list()
        |> List.replace_at(i - 1, :rand.uniform(256) - 1)
        |> :binary.list_to_bin()

      case mod.deserialize(mutated_binary, :compact) do
        {%{__struct__: ^mod}, _} -> :ok
        :error -> :ok
      end
    end
  end

  def assert_serializes(
        %{__struct__: mod} = struct,
        binary,
        %{__struct__: mod} = deserialized_struct
      ) do
    assert binary == IO.iodata_to_binary(mod.serialize(struct, :compact))
    assert {^deserialized_struct, ""} = mod.deserialize(binary, :compact)
  end

  @thrift_file name: "bool.thrift",
               contents: """
               struct Bool {
                 1: optional bool val;
                 2: optional map<bool, bool> val_map;
                 3: optional set<bool> val_set;
                 4: optional list<bool> val_list;
                 16: optional bool large_field_id_val;
               }
               """

  thrift_test "bool serialization" do
    assert_serializes(%Bool{}, <<0>>)
    assert_serializes(%Bool{val: false}, <<18, 0>>)
    assert_serializes(%Bool{val: true}, <<17, 0>>)
    assert_serializes(%Bool{large_field_id_val: false}, <<2, 32, 0>>)
    assert_serializes(%Bool{large_field_id_val: true}, <<1, 32, 0>>)
    assert_serializes(%Bool{val_map: %{}}, <<43, 0, 0>>)
    assert_serializes(%Bool{val_map: %{false => true}}, <<43, 1, 17, 2, 1, 0>>)
    assert_serializes(%Bool{val_set: MapSet.new()}, <<58, 1, 0>>)
    assert_serializes(%Bool{val_set: MapSet.new([true])}, <<58, 17, 1, 0>>)
    assert_serializes(%Bool{val_list: []}, <<73, 1, 0>>)
    assert_serializes(%Bool{val_list: [true]}, <<73, 17, 1, 0>>)

    # The way size and type is encoded changes at element size 15.
    assert_serializes(
      %Bool{val_list: List.duplicate(true, 15)},
      <<73, 241, 15, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0>>
    )

    assert_serializes(
      %Bool{val_list: List.duplicate(true, 14)},
      <<73, 225, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0>>
    )

    assert_serializes(%Bool{val_list: [true, false]}, <<73, 33, 1, 2, 0>>)
  end

  @thrift_file name: "multifield.thrift",
               contents: """
               struct Multifield {
                 1: optional bool bool_one;
                 2: optional bool bool_two;
                 3: optional bool bool_three;
                 4: optional map<bool, bool> map_four;
                 5: optional list<bool> list_five;
                 6: optional byte byte_six;
               }
               """
  thrift_test "with multiple fields, field ids are relative to the last field" do
    assert_serializes(%Multifield{bool_one: true}, <<17, 0>>)
    assert_serializes(%Multifield{bool_two: true}, <<33, 0>>)
    assert_serializes(%Multifield{bool_one: true, bool_two: true}, <<17, 17, 0>>)
    assert_serializes(%Multifield{bool_one: false, bool_three: false}, <<18, 34, 0>>)
    assert_serializes(%Multifield{bool_two: true, bool_three: true}, <<33, 17, 0>>)

    assert_serializes(
      %Multifield{bool_one: true, bool_two: true, bool_three: true},
      <<17, 17, 17, 0>>
    )

    assert_serializes(%Multifield{bool_three: true}, <<49, 0>>)

    assert_serializes(%Multifield{bool_three: true, map_four: %{}}, <<49, 27, 0, 0>>)

    assert_serializes(
      %Multifield{bool_three: true, map_four: %{true => false}},
      <<49, 27, 1, 17, 1, 2, 0>>
    )

    assert_serializes(%Multifield{bool_three: true, list_five: []}, <<49, 41, 1, 0>>)
    assert_serializes(%Multifield{bool_three: true, list_five: [true]}, <<49, 41, 17, 1, 0>>)
  end

  @thrift_file name: "byte.thrift",
               contents: """
               struct Byte {
                 1: optional byte val;
                 2: optional map<byte, byte> val_map;
                 3: optional set<byte> val_set;
                 4: optional list<byte> val_list;
               }
               """

  thrift_test "byte serialization" do
    assert_serializes(%Byte{}, <<0>>)
    assert_serializes(%Byte{val: 0}, <<19, 0, 0>>)
    assert_serializes(%Byte{val: 1}, <<19, 1, 0>>)
    assert_serializes(%Byte{val: -1}, <<19, 255, 0>>)
    assert_serializes(%Byte{val: 256}, <<19, 0, 0>>, %Byte{val: 0})
    assert_serializes(%Byte{val_map: %{}}, <<43, 0, 0>>)
    assert_serializes(%Byte{val_map: %{91 => 92}}, <<43, 1, 51, 91, 92, 0>>)
    assert_serializes(%Byte{val_set: MapSet.new()}, <<58, 3, 0>>)
    assert_serializes(%Byte{val_set: MapSet.new([91])}, <<58, 19, 91, 0>>)

    # The bool serialization could not exercise the change in encoding of sets
    # at a size of 15, so let's exercise that here
    assert_serializes(
      %Byte{val_set: MapSet.new(1..14)},
      <<58, 227, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0>>
    )

    assert_serializes(
      %Byte{val_set: MapSet.new(1..15)},
      <<58, 243, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0>>
    )

    assert_serializes(%Byte{val_list: []}, <<73, 3, 0>>)
    assert_serializes(%Byte{val_list: [91]}, <<73, 19, 91, 0>>)
    assert_serializes(%Byte{val_list: [91, 92]}, <<73, 35, 91, 92, 0>>)
    assert_serializes(%Byte{val_list: [91, 92, 93]}, <<73, 51, 91, 92, 93, 0>>)
  end

  @thrift_file name: "double.thrift",
               contents: """
               struct Double {
                 1: optional double val;
                 2: optional map<double, double> val_map;
                 3: optional set<double> val_set;
                 4: optional list<double> val_list;
               }
               """

  thrift_test "double serialization" do
    assert_serializes(%Double{}, <<0>>)
    assert_serializes(%Double{val: 0.0}, <<23, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)
    assert_serializes(%Double{val: 1.0}, <<23, 0, 0, 0, 0, 0, 0, 240, 63, 0>>)
    assert_serializes(%Double{val: 255.0}, <<23, 0, 0, 0, 0, 0, 224, 111, 64, 0>>)
    assert_serializes(%Double{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %Double{val_map: %{91.0 => 92.0}},
      <<43, 1, 119, 0, 0, 0, 0, 0, 192, 86, 64, 0, 0, 0, 0, 0, 0, 87, 64, 0>>
    )

    assert_serializes(%Double{val_set: MapSet.new()}, <<58, 7, 0>>)

    assert_serializes(
      %Double{val_set: MapSet.new([91.0])},
      <<58, 23, 0, 0, 0, 0, 0, 192, 86, 64, 0>>
    )

    assert_serializes(%Double{val_list: []}, <<73, 7, 0>>)
    assert_serializes(%Double{val_list: [91.0]}, <<73, 23, 0, 0, 0, 0, 0, 192, 86, 64, 0>>)
  end

  @thrift_file name: "i16.thrift",
               contents: """
               struct I16 {
                 1: optional i16 val;
                 2: optional map<i16, i16> val_map;
                 3: optional set<i16> val_set;
                 4: optional list<i16> val_list;
               }
               """

  thrift_test "i16 serialization" do
    assert_serializes(%I16{}, <<0>>)
    assert_serializes(%I16{val: 0}, <<20, 0, 0>>)
    assert_serializes(%I16{val: 1}, <<20, 2, 0>>)
    assert_serializes(%I16{val: 255}, <<20, 254, 3, 0>>)
    assert_serializes(%I16{val: 256}, <<20, 128, 4, 0>>)
    assert_serializes(%I16{val: 32767}, <<20, 254, 255, 3, 0>>)
    assert_serializes(%I16{val: -1}, <<20, 1, 0>>)
    assert_serializes(%I16{val: -32768}, <<20, 255, 255, 3, 0>>)

    # Ruby thrift ignores the constraint of the field size. (Python errors)
    # Keeping this consistent with the binary protocol, seems like the right thing
    # to do though
    assert_serializes(%I16{val: 65536}, <<20, 0, 0>>, %I16{val: 0})
    assert_serializes(%I16{val_map: %{}}, <<43, 0, 0>>)
    assert_serializes(%I16{val_map: %{91 => 92}}, <<43, 1, 68, 182, 1, 184, 1, 0>>)
    assert_serializes(%I16{val_set: MapSet.new()}, <<58, 4, 0>>)
    assert_serializes(%I16{val_set: MapSet.new([91])}, <<58, 20, 182, 1, 0>>)
    assert_serializes(%I16{val_list: []}, <<73, 4, 0>>)
    assert_serializes(%I16{val_list: [91]}, <<73, 20, 182, 1, 0>>)
    assert_serializes(%I16{val_list: [91, 92]}, <<73, 36, 182, 1, 184, 1, 0>>)

    assert_serializes(%I16{val_list: [91, 92, 93]}, <<73, 52, 182, 1, 184, 1, 186, 1, 0>>)
  end

  @thrift_file name: "i32.thrift",
               contents: """
               struct I32 {
                 1: optional i32 val;
                 2: optional map<i32, i32> val_map;
                 3: optional set<i32> val_set;
                 4: optional list<i32> val_list;
               }
               """

  thrift_test "i32 serialization" do
    assert_serializes(%I32{}, <<0>>)
    assert_serializes(%I32{val: 0}, <<21, 0, 0>>)
    assert_serializes(%I32{val: 1}, <<21, 2, 0>>)
    assert_serializes(%I32{val: 255}, <<21, 254, 3, 0>>)
    assert_serializes(%I32{val: 256}, <<21, 128, 4, 0>>)
    assert_serializes(%I32{val: 65535}, <<21, 254, 255, 7, 0>>)
    assert_serializes(%I32{val: 2_147_483_647}, <<21, 254, 255, 255, 255, 15, 0>>)
    # errors in Ruby
    assert_serializes(%I32{val: -2_147_483_648}, <<21, 255, 255, 255, 255, 15, 0>>)
    assert_serializes(%I32{val: -1}, <<21, 1, 0>>)
    assert_serializes(%I32{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(%I32{val_map: %{91 => 92}}, <<43, 1, 85, 182, 1, 184, 1, 0>>)

    assert_serializes(%I32{val_set: MapSet.new()}, <<58, 5, 0>>)

    assert_serializes(%I32{val_set: MapSet.new([91])}, <<58, 21, 182, 1, 0>>)

    assert_serializes(%I32{val_list: []}, <<73, 5, 0>>)
    assert_serializes(%I32{val_list: [91]}, <<73, 21, 182, 1, 0>>)

    assert_serializes(%I32{val_list: [91, 92]}, <<73, 37, 182, 1, 184, 1, 0>>)
  end

  @thrift_file name: "i64.thrift",
               contents: """
               struct I64 {
                 1: optional i64 val;
                 2: optional map<i64, i64> val_map;
                 3: optional set<i64> val_set;
                 4: optional list<i64> val_list;
               }
               """

  thrift_test "i64 serialization" do
    i64_max = 9_223_372_036_854_775_807
    i64_min = -9_223_372_036_854_775_808

    assert_serializes(%I64{}, <<0>>)
    assert_serializes(%I64{val: 0}, <<22, 0, 0>>)
    assert_serializes(%I64{val: 1}, <<22, 2, 0>>)
    assert_serializes(%I64{val: 255}, <<22, 254, 3, 0>>)
    assert_serializes(%I64{val: 256}, <<22, 128, 4, 0>>)
    assert_serializes(%I64{val: 65535}, <<22, 254, 255, 7, 0>>)

    assert_serializes(
      %I64{val: i64_min},
      <<22, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0>>
    )

    assert_serializes(
      %I64{val: i64_max},
      <<22, 254, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0>>
    )

    assert_serializes(%I64{val: -1}, <<22, 1, 0>>)
    assert_serializes(%I64{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(%I64{val_map: %{91 => 92}}, <<43, 1, 102, 182, 1, 184, 1, 0>>)

    assert_serializes(
      %I64{val_map: %{i64_max => i64_min}},
      <<43, 1, 102, 254, 255, 255, 255, 255, 255, 255, 255, 255, 1, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 1, 0>>
    )

    assert_serializes(%I64{val_set: MapSet.new()}, <<58, 6, 0>>)

    assert_serializes(%I64{val_set: MapSet.new([91])}, <<58, 22, 182, 1, 0>>)

    assert_serializes(%I64{val_list: []}, <<73, 6, 0>>)

    assert_serializes(%I64{val_list: [91]}, <<73, 22, 182, 1, 0>>)

    assert_serializes(%I64{val_list: [91, 92]}, <<73, 38, 182, 1, 184, 1, 0>>)

    assert_serializes(
      %I64{val_list: [i64_max, i64_min]},
      <<73, 38, 254, 255, 255, 255, 255, 255, 255, 255, 255, 1, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 1, 0>>
    )
  end

  @thrift_file name: "string.thrift",
               contents: """
               struct String {
                 1: optional string val;
                 2: optional map<string, string> val_map;
                 3: optional set<string> val_set;
                 4: optional list<string> val_list;
               }
               """

  thrift_test "string serialization" do
    assert_serializes(%String{}, <<0>>)
    assert_serializes(%String{val: ""}, <<24, 0, 0>>)
    assert_serializes(%String{val: "abc"}, <<24, 3, 97, 98, 99, 0>>)
    assert_serializes(%String{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %String{val_map: %{"abc" => "def"}},
      <<43, 1, 136, 3, 97, 98, 99, 3, 100, 101, 102, 0>>
    )

    assert_serializes(%String{val_set: MapSet.new()}, <<58, 8, 0>>)

    assert_serializes(%String{val_set: MapSet.new(["abc"])}, <<58, 24, 3, 97, 98, 99, 0>>)

    assert_serializes(%String{val_list: []}, <<73, 8, 0>>)

    assert_serializes(%String{val_list: ["abc"]}, <<73, 24, 3, 97, 98, 99, 0>>)

    assert_serializes(
      %String{val_list: ["abc", "def"]},
      <<73, 40, 3, 97, 98, 99, 3, 100, 101, 102, 0>>
    )
  end

  @thrift_file name: "binary.thrift",
               contents: """
               struct BinaryStruct {
                 1: optional binary val;
                 2: optional map<binary, binary> val_map;
                 3: optional set<binary> val_set;
                 4: optional list<binary> val_list;
               }
               """

  thrift_test "binary serialization" do
    assert_serializes(%BinaryStruct{}, <<0>>)
    assert_serializes(%BinaryStruct{val: ""}, <<24, 0, 0>>)
    assert_serializes(%BinaryStruct{val: "abc"}, <<24, 3, 97, 98, 99, 0>>)
    assert_serializes(%BinaryStruct{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %BinaryStruct{val_map: %{"abc" => "def"}},
      <<43, 1, 136, 3, 97, 98, 99, 3, 100, 101, 102, 0>>
    )

    assert_serializes(%BinaryStruct{val_set: MapSet.new()}, <<58, 8, 0>>)

    assert_serializes(%BinaryStruct{val_set: MapSet.new(["abc"])}, <<58, 24, 3, 97, 98, 99, 0>>)

    assert_serializes(%BinaryStruct{val_list: []}, <<73, 8, 0>>)

    assert_serializes(%BinaryStruct{val_list: ["abc"]}, <<73, 24, 3, 97, 98, 99, 0>>)

    assert_serializes(
      %BinaryStruct{val_list: ["abc", "def"]},
      <<73, 40, 3, 97, 98, 99, 3, 100, 101, 102, 0>>
    )
  end

  @thrift_file name: "struct.thrift",
               contents: """
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
    assert_serializes(%Struct{}, <<0>>)
    assert_serializes(%Struct{val: %Val{}}, <<28, 0, 0>>)
    assert_serializes(%Struct{val: %Val{num: 91}}, <<28, 3, 198, 1, 91, 0, 0>>)
    assert_serializes(%Struct{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %Struct{val_map: %{%Val{num: 91} => %Val{num: 92}}},
      <<43, 1, 204, 3, 198, 1, 91, 0, 3, 198, 1, 92, 0, 0>>
    )

    assert_serializes(%Struct{val_set: MapSet.new()}, <<58, 12, 0>>)

    assert_serializes(
      %Struct{val_set: MapSet.new([%Val{num: 91}])},
      <<58, 28, 3, 198, 1, 91, 0, 0>>
    )

    assert_serializes(%Struct{val_list: []}, <<73, 12, 0>>)
    assert_serializes(%Struct{val_list: [%Val{num: 91}]}, <<73, 28, 3, 198, 1, 91, 0, 0>>)
  end

  @thrift_file name: "unions.thrift",
               contents: """
               struct StructValue {
                 1: optional string username;
               }
               union Union {
                 1: i64 int_field,
                 2: StructValue struct_field,
                 3: string string_field,
                 4: list<i16> list_field;
                 5: bool bool_field;
                 6: map<byte, bool> map_field;
               }

               struct UStruct {
                 1: optional Union my_union,
                 2: optional map<Union, Union> u_map,
                 3: optional set<Union> u_set,
                 4: optional list<Union> u_list,
               }
               """

  thrift_test "union serialization" do
    # Compact below. Don't feel ready for unionisation yet
    assert_serializes(%Union{}, <<0>>)
    assert_serializes(%Union{int_field: 205}, <<22, 154, 3, 0>>)

    assert_serializes(
      %Union{struct_field: %StructValue{username: "stinky"}},
      <<44, 24, 6, 115, 116, 105, 110, 107, 121, 0, 0>>
    )

    assert_serializes(%Union{string_field: "hello"}, <<56, 5, 104, 101, 108, 108, 111, 0>>)
    assert_serializes(%Union{list_field: [5, 9, 7]}, <<73, 52, 10, 18, 14, 0>>)
    assert_serializes(%Union{bool_field: true}, <<81, 0>>)
    assert_serializes(%Union{bool_field: false}, <<82, 0>>)
    assert_serializes(%Union{map_field: %{}}, <<107, 0, 0>>)
    assert_serializes(%Union{map_field: %{47 => true}}, <<107, 1, 49, 47, 1, 0>>)

    assert_raise TooManyFieldsSetError, fn ->
      Union.serialize(%Union{int_field: 205, list_field: [1, 2]}, :compact)
    end
  end

  thrift_test "structs can have unions" do
    assert_serializes(%UStruct{}, <<0>>)

    assert_serializes(%UStruct{my_union: %Union{int_field: 2}}, <<28, 22, 4, 0, 0>>)

    assert_serializes(%UStruct{u_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %UStruct{u_map: %{%Union{int_field: 23} => %Union{int_field: 33}}},
      <<43, 1, 204, 22, 46, 0, 22, 66, 0, 0>>
    )

    assert_serializes(
      %UStruct{u_set: MapSet.new([%Union{int_field: 2239}])},
      <<58, 28, 22, 254, 34, 0, 0>>
    )

    assert_serializes(%UStruct{u_list: [%Union{int_field: 1291}]}, <<73, 28, 22, 150, 20, 0, 0>>)

    assert_raise TooManyFieldsSetError, fn ->
      UStruct.serialize(
        %UStruct{my_union: %Union{int_field: 123, string_field: "oops"}},
        :compact
      )
    end
  end

  @thrift_file name: "exception.thrift",
               contents: """
               exception Ex {
                  1: optional string message,
                 99: optional byte num;
               }
               exception Ex2 {
                 1: optional i16 error_code;
               }
               struct Exception {
                 1: optional Ex val;
                 2: optional map<Ex, Ex> val_map;
                 3: optional set<Ex> val_set;
                 4: optional list<Ex> val_list;
               }
               """

  thrift_test "exception serialization" do
    assert_serializes(%Exception{}, <<0>>)
    assert_serializes(%Exception{val: %Ex{}}, <<28, 0, 0>>)
    assert_serializes(%Exception{val: %Ex{num: 91}}, <<28, 3, 198, 1, 91, 0, 0>>)
    assert_serializes(%Exception{val_map: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %Exception{val_map: %{%Ex{num: 91} => %Ex{num: 92}}},
      <<43, 1, 204, 3, 198, 1, 91, 0, 3, 198, 1, 92, 0, 0>>
    )

    assert_serializes(%Exception{val_set: MapSet.new()}, <<58, 12, 0>>)

    assert_serializes(
      %Exception{val_set: MapSet.new([%Ex{num: 91}])},
      <<58, 28, 3, 198, 1, 91, 0, 0>>
    )

    assert_serializes(%Exception{val_list: []}, <<73, 12, 0>>)
    assert_serializes(%Exception{val_list: [%Ex{num: 91}]}, <<73, 28, 3, 198, 1, 91, 0, 0>>)
  end

  thrift_test "exceptions always provide message/1" do
    assert Ex.message(%Ex{message: "text", num: 1}) == "text"

    assert Ex2.message(%Ex2{error_code: 1}) ==
             ~s(%Thrift.Generator.CompactProtocolTest.Ex2{error_code: 1})
  end

  @thrift_file name: "composite.thrift",
               contents: """
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
    assert_serializes(%Composite{}, <<0>>)
    assert_serializes(%Composite{map_of_maps: %{}}, <<27, 0, 0>>)

    assert_serializes(
      %Composite{map_of_maps: %{%{91 => 92} => %{93 => 94, 95 => 96}}},
      <<27, 1, 187, 1, 51, 91, 92, 2, 51, 93, 94, 95, 96, 0>>
    )

    assert_serializes(%Composite{map_of_sets: %{}}, <<43, 0, 0>>)

    assert_serializes(
      %Composite{map_of_sets: %{MapSet.new([91]) => MapSet.new([92, 93])}},
      <<43, 1, 170, 19, 91, 35, 92, 93, 0>>
    )

    assert_serializes(%Composite{map_of_lists: %{}}, <<59, 0, 0>>)

    assert_serializes(
      %Composite{map_of_lists: %{[91] => [92, 93]}},
      <<59, 1, 153, 19, 91, 35, 92, 93, 0>>
    )

    assert_serializes(%Composite{set_of_maps: MapSet.new()}, <<74, 11, 0>>)

    assert_serializes(
      %Composite{set_of_maps: MapSet.new([%{91 => 92}])},
      <<74, 27, 1, 51, 91, 92, 0>>
    )

    assert_serializes(%Composite{set_of_sets: MapSet.new()}, <<90, 10, 0>>)

    assert_serializes(
      %Composite{set_of_sets: MapSet.new([MapSet.new([91]), MapSet.new([92, 93])])},
      <<90, 42, 19, 91, 35, 92, 93, 0>>
    )

    assert_serializes(%Composite{set_of_lists: MapSet.new()}, <<106, 9, 0>>)

    assert_serializes(
      %Composite{set_of_lists: MapSet.new([[91], [92, 93]])},
      <<106, 41, 19, 91, 35, 92, 93, 0>>
    )

    assert_serializes(%Composite{list_of_maps: []}, <<121, 11, 0>>)

    assert_serializes(
      %Composite{list_of_maps: [%{91 => 92}]},
      <<121, 27, 1, 51, 91, 92, 0>>
    )

    assert_serializes(%Composite{list_of_sets: []}, <<137, 10, 0>>)

    assert_serializes(
      %Composite{list_of_sets: [MapSet.new([91]), MapSet.new([92, 93])]},
      <<137, 42, 19, 91, 35, 92, 93, 0>>
    )

    assert_serializes(%Composite{list_of_lists: []}, <<153, 9, 0>>)

    assert_serializes(
      %Composite{list_of_lists: [[91], [92, 93]]},
      <<153, 41, 19, 91, 35, 92, 93, 0>>
    )
  end

  @thrift_file name: "typedefs.thrift",
               contents: """
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
    assert_serializes(%Typedefs{}, <<0>>)

    assert_serializes(%Typedefs{ints: MapSet.new([1, 2])}, <<26, 37, 2, 4, 0>>)

    assert_serializes(
      %Typedefs{mappings: %{"foo" => "bar", "baz" => "quux"}},
      <<43, 2, 136, 3, "baz", 4, "quux", 3, "foo", 3, "bar", 0>>
    )

    assert_serializes(%Typedefs{numbers: [9, 32, 104]}, <<57, 54, 18, 64, 208, 1, 0>>)
  end

  @thrift_file name: "requiredness.thrift",
               contents: """
               struct RequiredBool         { 1: required bool val }
               struct DefaultRequiredBool  { 1: bool val }
               struct OptionalBool         { 1: optional bool val }
               struct RequiredField        { 1: required i8 val }
               struct DefaultRequiredField { 1: i8 val }
               struct OptionalField        { 1: optional i8 val }
               struct RequiredMap          { 1: required map<byte, byte> val}
               """

  thrift_test "required boolean fields must not be nil during serialization" do
    message =
      "Required field :val on Thrift.Generator.CompactProtocolTest.RequiredBool must not be nil"

    assert_raise Thrift.InvalidValueError, message, fn ->
      RequiredBool.serialize(%RequiredBool{}, :compact)
    end
  end

  thrift_test "default required boolean fields may be nil during serialization" do
    assert_serializes(%DefaultRequiredBool{}, <<0>>)
    assert_serializes(%DefaultRequiredBool{val: true}, <<17, 0>>)
  end

  thrift_test "optional boolean fields may be nil during serialization" do
    assert_serializes(%OptionalBool{}, <<0>>)
    assert_serializes(%OptionalBool{val: true}, <<17, 0>>)
  end

  thrift_test "required fields must not be nil during serialization" do
    message =
      "Required field :val on Thrift.Generator.CompactProtocolTest.RequiredField must not be nil"

    assert_raise Thrift.InvalidValueError, message, fn ->
      RequiredField.serialize(%RequiredField{}, :compact)
    end

    assert_serializes(%RequiredField{val: 92}, <<19, 92, 0>>)
  end

  thrift_test "required map fields must not be nil during serialization" do
    message =
      "Required field :val on Thrift.Generator.CompactProtocolTest.RequiredMap must not be nil"

    assert_raise Thrift.InvalidValueError, message, fn ->
      RequiredMap.serialize(%RequiredMap{}, :compact)
    end
  end

  thrift_test "default required fields may be nil during serialization" do
    assert_serializes(%DefaultRequiredField{}, <<0>>)
    assert_serializes(%DefaultRequiredField{val: 123}, <<19, 123, 0>>)
  end

  thrift_test "optional fields may be nil during serialization" do
    assert_serializes(%OptionalField{}, <<0>>)
    assert_serializes(%OptionalField{val: 123}, <<19, 123, 0>>)
  end

  @thrift_file name: "const.thrift",
               contents: """
               struct ConstStructVal {
                 1: byte num
               }

               const bool ConstBool = true
               const byte ConstByte = 5
               const double ConstDouble = 5.0
               const i16 ConstI16 = 5
               const i32 ConstI32 = 5
               const i64 ConstI64 = 5
               const string ConstString = "abc123"
               const binary ConstBinary = "abc123"
               const ConstStructVal ConstStruct = {"num": 5}
               const map<string, byte> ConstMap = {"a": 1, "b": 2}
               const set<string> ConstSet = ["a", "b"]
               const list<string> ConstList = ["a", "b"]

               struct ConstFieldsStruct {
                 1: bool bool_val = ConstBool,
                 2: byte byte_val = ConstByte,
                 3: double double_val = ConstDouble,
                 4: i16 i16_val = ConstI16,
                 5: i32 i32_val = ConstI32,
                 6: i64 i64_val = ConstI64,
                 7: string string_val = ConstString,
                 71: binary binary_val = ConstBinary,
                 8: ConstStructVal struct_val = ConstStruct,
                 13: map<string, byte> map_val = ConstMap,
                 14: set<string> set_val = ConstSet,
                 15: list<string> list_val = ConstList,
               }
               """

  @thrift_file name: "x_man.thrift",
               contents: """
               enum PowerLevel {
                 ALPHA,
                 BETA,
                 OMEGA
               }

               const string EARTH_616 = "Earth-616"

               struct XMan{
                 1: string handle
                 2: string name
                 3: string universe = EARTH_616
                 4: PowerLevel power_level
               }

               const XMan Wolverine = {"handle": "Wolverine", "name": "Logan", "power_level": PowerLevel.BETA}
               const XMan Cyclops = {"handle": "Cyclops", "name": "Scott Summers", "power_level": PowerLevel.BETA}
               const XMan Storm = {"handle": "Storm", "name": "Ororo Monroe", "power_level": PowerLevel.ALPHA}
               const XMan Phoenix = {"handle": "Phoenix", "name": "Jean Grey", "power_level": PowerLevel.OMEGA}
               """

  thrift_test "constants and structs defined in the same file" do
    assert %XMan{
             handle: "Wolverine",
             name: "Logan",
             power_level: PowerLevel.beta(),
             universe: XMan.earth_616()
           } == XMan.wolverine()

    assert %XMan{
             handle: "Cyclops",
             name: "Scott Summers",
             power_level: PowerLevel.beta(),
             universe: XMan.earth_616()
           } == XMan.cyclops()

    assert %XMan{
             handle: "Storm",
             name: "Ororo Monroe",
             power_level: PowerLevel.alpha(),
             universe: XMan.earth_616()
           } == XMan.storm()

    assert %XMan{
             handle: "Phoenix",
             name: "Jean Grey",
             power_level: PowerLevel.omega(),
             universe: XMan.earth_616()
           } == XMan.phoenix()
  end

  @thrift_file name: "included_constants.thrift",
               contents: """
               const i8 Z = 26
               """

  @thrift_file name: "includes_constants.thrift",
               contents: """
               include "included_constants.thrift"

               struct IncludesConstants {
                 1: string name
                 2: i8 z = included_constants.Z
               }
               """

  @thrift_file name: "also_includes.thrift",
               contents: """
               include "included_constants.thrift"

               struct SomeOtherName {
                 1: i32 id
               }
               """

  thrift_test "including a file with constants" do
    assert 26 == IncludedConstants.z()
    assert %IncludesConstants{z: 26} == %IncludesConstants{}
    assert_raise UndefinedFunctionError, fn -> 26 == IncludesConstants.z() end
    assert %SomeOtherName{} == SomeOtherName.new()
    assert Code.ensure_loaded?(IncludedConstants)
    # AlsoIncludes should not define anything
    refute Code.ensure_loaded?(AlsoIncludes)
  end

  thrift_test "lists serialize into maps" do
    binary = <<43, 1, 51, 91, 92, 0>>

    assert binary ==
             %Byte{val_map: %{91 => 92}} |> Byte.serialize(:compact) |> IO.iodata_to_binary()

    assert binary ==
             %Byte{val_map: [{91, 92}]} |> Byte.serialize(:compact) |> IO.iodata_to_binary()
  end

  thrift_test "lists serialize into sets" do
    binary = <<14, 0, 3, 3, 0, 0, 0, 1, 91, 0>>
    assert binary == %Byte{val_set: MapSet.new([91])} |> Byte.serialize() |> IO.iodata_to_binary()
    assert binary == %Byte{val_set: [91]} |> Byte.serialize() |> IO.iodata_to_binary()
  end

  @thrift_file name: "additions.thrift",
               contents: """
               enum ChocolateAdditionsType {
                 ALMONDS = 1,
                 NOUGAT  = 2,
                 HAIR    = 3
               }

               typedef set<ChocolateAdditionsType> ChocolateAdditions
               typedef map<ChocolateAdditionsType, string> ChocolateMapping
               """

  @thrift_file name: "chocolate.thrift",
               contents: """
               include "additions.thrift"

               struct Chocolate {
                 1: optional additions.ChocolateAdditions extra_stuff = [ChocolateAdditionsType.HAIR]
                 2: optional additions.ChocolateAdditionsType secret_ingredient = ChocolateAdditionsType.HAIR
               }

               struct Allergies {
                  1: optional list<additions.ChocolateAdditionsType> may_contain = [ChocolateAdditionsType.ALMONDS]
               }

               struct OddSnackIngredients {
                  1: optional set<additions.ChocolateAdditionsType> other_things = [ChocolateAdditionsType.NOUGAT]
               }

               struct ChocoMappings {
                 1: optional map<additions.ChocolateAdditionsType, string> common_name = {ChocolateAdditionsType.HAIR: "love"}
               }

               struct AdditionalMappings {
                 1: optional additions.ChocolateMapping mapping = {ChocolateAdditionsType.ALMONDS: "almonds",
                                                                   ChocolateAdditionsType.NOUGAT: "nougat"}
               }

               struct AlreadyNamespaced {
                 1: optional additions.ChocolateAdditionsType namespaced = additions.ChocolateAdditionsType.ALMONDS
               }
               """

  thrift_test "including a file with typedefs and defaults" do
    choco = %Chocolate{extra_stuff: MapSet.new([1, 2])}

    assert choco.secret_ingredient == ChocolateAdditionsType.hair()

    assert %Allergies{}.may_contain == [ChocolateAdditionsType.almonds()]
    assert %OddSnackIngredients{}.other_things == MapSet.new([ChocolateAdditionsType.nougat()])
    assert %ChocoMappings{}.common_name == %{ChocolateAdditionsType.hair() => "love"}

    assert %AdditionalMappings{}.mapping == %{
             ChocolateAdditionsType.almonds() => "almonds",
             ChocolateAdditionsType.nougat() => "nougat"
           }

    assert %AlreadyNamespaced{}.namespaced == ChocolateAdditionsType.almonds()

    actual =
      choco
      |> Chocolate.serialize(:compact)
      |> IO.iodata_to_binary()

    expected = <<26, 37, 2, 4, 21, 6, 0>>
    assert actual == expected
  end

  @thrift_file name: "missingfields.thrift",
               contents: """
               struct ManyFields {
               1: optional byte val1;
               25: optional byte val2;
               26: optional byte val3;
               27: optional byte val4;
               }

               struct FewerFields {
               26: optional byte val3;
               }
               """
  thrift_test "struct with missing field" do
    serialised =
      %ManyFields{val1: 1, val2: 2, val3: 3, val4: 4}
      |> ManyFields.serialize(:compact)
      |> IO.iodata_to_binary()

    assert {%FewerFields{} = deserialised, ""} = FewerFields.deserialize(serialised, :compact)

    assert deserialised.val3 == 3
  end

  @thrift_file name: "out_of_order.thrift",
               contents: """
               struct OutOfOrder {
               5: optional byte byte_five;
               4: optional byte byte_four;
               3: optional bool bool_three;
               2: optional bool bool_two;
               1: optional bool bool_one;
               }
               """
  thrift_test "fields defined out of order" do
    assert_serializes(%OutOfOrder{bool_one: false, bool_three: false}, <<18, 34, 0>>)
    assert_serializes(%OutOfOrder{byte_four: 4, byte_five: 5}, <<67, 4, 19, 5, 0>>)
  end

  @thrift_file name: "default_field_ids.thrift",
               contents: """
               struct DefaultFieldIds {
               optional byte minus_one;
               optional byte minus_two;
               optional byte minus_three;
               1: optional byte one;
               2: optional byte two;
               14: optional byte fourteen;
               }
               """

  thrift_test "with negative field ids" do
    assert_serializes(%DefaultFieldIds{}, <<0>>)
    assert_serializes(%DefaultFieldIds{minus_one: -1}, <<3, 1, 255, 0>>)
    assert_serializes(%DefaultFieldIds{minus_two: -2}, <<3, 3, 254, 0>>)
    assert_serializes(%DefaultFieldIds{minus_one: -1, minus_two: -2}, <<3, 3, 254, 19, 255, 0>>)
    assert_serializes(%DefaultFieldIds{minus_one: -1, fourteen: 14}, <<3, 1, 255, 243, 14, 0>>)
    assert_serializes(%DefaultFieldIds{minus_two: -2, fourteen: 14}, <<3, 3, 254, 3, 28, 14, 0>>)

    assert_serializes(
      %DefaultFieldIds{minus_three: 120, fourteen: 14},
      <<3, 5, 120, 3, 28, 14, 0>>
    )
  end

  @thrift_file name: "long_headers.thrift",
               contents: """
               struct LongHeaders {
               16: optional byte sixteen;
               35: optional byte thirty_five;
               }
               """
  thrift_test "long headers" do
    assert_serializes(%LongHeaders{sixteen: 10, thirty_five: 20}, <<3, 32, 10, 3, 70, 20, 0>>)
  end
end
