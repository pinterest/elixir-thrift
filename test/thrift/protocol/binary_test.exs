defmodule BinaryProtocolTest do
  use ThriftTestCase, async: true

  alias Thrift.Protocol.Binary

  defp serialize(module, struct) do
    struct
    |> module.serialize
    |> IO.iodata_to_binary()
  end

  defp deserialize(module, binary_data) do
    {struct, ""} = module.deserialize(binary_data)
    struct
  end

  defp assert_serde(%module{} = struct, thrift_binary_file) do
    thrift_binary_file = Path.join("test/data/binary", thrift_binary_file)
    thrift_binary = File.read!(thrift_binary_file)

    serialized = serialize(module, struct)
    deserialized_test_data = deserialize(module, thrift_binary)
    assert deserialize(module, serialized) == deserialized_test_data
  end

  @thrift_file name: "enums.thrift",
               contents: """
               enum Status {
                 ACTIVE,
                 INACTIVE,
                 BANNED = 6,
                 EVIL = 0x20,
               }

               struct StructWithEnum {
                 1: optional Status status
               }
               """
  thrift_test "encoding enums" do
    assert_serde(%StructWithEnum{status: Status.banned()}, "enums/banned.thriftbin")
    assert_serde(%StructWithEnum{status: Status.evil()}, "enums/evil.thriftbin")
  end

  @thrift_file name: "scalars.thrift",
               contents: """
               enum Weather {
                  SUNNY,
                  CLOUDY,
                  RAINY,
                  SNOWY
               }

               struct Scalars {
                 1: optional bool is_true,
                 2: optional byte byte_value,
                 3: optional i16 sixteen_bits,
                 4: optional i32 thirty_two_bits,
                 5: optional i64 sixty_four_bits,
                 6: optional double double_value,
                 7: optional string string_value,
                 8: optional binary raw_binary
               }
               """

  thrift_test "it should be able to encode scalar values" do
    assert_serde(%Scalars{is_true: true}, "scalars/bool.thriftbin")
    assert_serde(%Scalars{byte_value: 127}, "scalars/byte.thriftbin")
    assert_serde(%Scalars{sixteen_bits: 12723}, "scalars/i16.thriftbin")
    assert_serde(%Scalars{thirty_two_bits: 18_362_832}, "scalars/i32.thriftbin")
    assert_serde(%Scalars{sixty_four_bits: 8_872_372}, "scalars/i64.thriftbin")
    assert_serde(%Scalars{double_value: 2.37219}, "scalars/double.thriftbin")
    assert_serde(%Scalars{string_value: "I am a string"}, "scalars/string.thriftbin")
    assert_serde(%Scalars{raw_binary: <<224, 186, 2, 1, 0>>}, "scalars/binary.thriftbin")
  end

  thrift_test "it should not encode unset fields" do
    assert <<0>> == IO.iodata_to_binary(Scalars.serialize(%Scalars{}))
  end

  @thrift_file name: "containers.thrift",
               contents: """
               enum Weather {
                 SUNNY,
                 CLOUDY,
                 RAINY,
                 SNOWY
               }

               struct Friend {
                 1: optional i64 id,
                 2: optional string username,
               }

               struct Containers {
                1: optional list<i64> users,
                2: optional list<Weather> weekly_forecast,
                3: optional set<string> taken_usernames,
                4: list<Friend> friends,
                5: map<i64, Weather> user_forecasts,
                6: map<string, Friend> friends_by_username
               }
               """

  thrift_test "containers serialize properly" do
    # containers can have no fields set
    assert_serde(%Containers{}, "containers/unset.thriftbin")

    # empty containers are sent
    assert_serde(%Containers{users: []}, "containers/empty_list.thriftbin")

    # containers can contain enums
    forecast = [
      Weather.sunny(),
      Weather.sunny(),
      Weather.sunny(),
      Weather.sunny(),
      Weather.cloudy(),
      Weather.sunny(),
      Weather.sunny()
    ]

    assert_serde(%Containers{weekly_forecast: forecast}, "containers/enums_list.thriftbin")

    taken_usernames = ["scohen", "pguillory"]

    assert_serde(
      %Containers{taken_usernames: MapSet.new(taken_usernames)},
      "containers/strings_set.thriftbin"
    )

    structs_list = %Containers{
      friends: [
        %Friend{id: 1, username: "scohen"},
        %Friend{id: 2, username: "pguillory"},
        %Friend{id: 3, username: "dantswain"}
      ]
    }

    assert_serde(structs_list, "containers/structs_list.thriftbin")

    enums_map = %Containers{
      user_forecasts: %{
        1 => Weather.sunny(),
        -1 => Weather.sunny(),
        12345 => Weather.cloudy()
      }
    }

    assert_serde(enums_map, "containers/enums_map.thriftbin")

    structs_map = %Containers{
      friends_by_username: %{
        "scohen" => %Friend{id: 1, username: "scohen"},
        "pguillory" => %Friend{id: 2, username: "pguillory"},
        "dantswain" => %Friend{id: 3, username: "dantswain"}
      }
    }

    assert_serde(structs_map, "containers/structs_map.thriftbin")
  end

  @thrift_file name: "across.thrift",
               contents: """
                 include "containers.thrift"

                 struct User {
                   1: optional i64 id,
                   2: optional containers.Friend best_friend;
                 }
               """

  thrift_test "serializing structs across modules" do
    erl_user = %User{
      id: 1234,
      best_friend: %Friend{id: 3282, username: "stinkypants"}
    }

    assert_serde(erl_user, "across/across.thriftbin")
  end

  @thrift_file name: "old.thrift",
               contents: """
                 struct OldChangeyStruct {
                   1: optional i64 id,
                   2: optional string username
                 }
               """
  @thrift_file name: "new.thrift",
               contents: """
                 struct SubSubStruct {
                   1: optional bool is_this_excessive;
                 }

                 struct SubStruct {
                   1: optional string password,
                   2: optional SubSubStruct sub_sub;
                 }

                 enum Grooviness {
                   ALL_GOOD
                   PARTIALLY_GOOD
                   NOT_SO_GOOD
                 }

                 struct ChangeyStruct {
                   1: optional i64 id,
                   2: optional string username,
                   3: optional bool new_bool
                   4: optional byte new_byte,
                   5: optional double new_double,
                   6: optional i16 new_i16,
                   7: optional i32 new_i32,
                   8: optional i64 new_i64,
                   9: optional string new_string,
                   10: optional Grooviness new_enum,
                   11: optional SubStruct new_substruct,
                   12: optional list<SubStruct> new_list,
                   13: optional set<SubStruct> new_set,
                   14: optional map<i16, SubStruct> new_map,
                   15: optional ChangeyStruct my_twin
                 }

               """
  thrift_test "deserializing scalar fields you don't know about" do
    changey = %ChangeyStruct{
      id: 12345,
      username: "stinkypants",
      new_bool: false,
      new_byte: 3,
      new_double: 19.3,
      new_i16: 4821,
      new_i32: 1_284_291,
      new_i64: 128_382_100_315,
      new_string: "suprise! an unexpected field!",
      new_enum: Grooviness.partially_good()
    }

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a substruct you don't know about" do
    sub_struct = %SubStruct{password: "letmein"}

    changey = %ChangeyStruct{
      id: 12345,
      username: "stinkypants",
      new_substruct: sub_struct
    }

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing nested structs you don't know about" do
    sub_sub = %SubSubStruct{is_this_excessive: true}
    sub = %SubStruct{password: "1234", sub_sub: sub_sub}
    changey = %ChangeyStruct{id: 12345, username: "stinkypants", new_substruct: sub}

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a list of structs you don't know about" do
    sub = %SubStruct{password: "1234"}
    changey = %ChangeyStruct{id: 12345, username: "stinkypants", new_list: [sub]}

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a set of structs you don't know about" do
    sub = %SubStruct{password: "1234"}
    changey = %ChangeyStruct{id: 12345, username: "stinkypants", new_set: MapSet.new([sub, sub])}

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a map of structs you don't know about" do
    sub = %SubStruct{password: "1234"}
    changey = %ChangeyStruct{id: 12345, username: "stinkypants", new_map: %{1 => sub, 2 => sub}}

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a recursive sub struct with all types" do
    sub = %SubStruct{password: "1234"}

    twin = %ChangeyStruct{
      id: 6789,
      new_bool: false,
      new_byte: 4,
      new_i16: 102,
      new_i32: 919_482,
      new_i64: 18_281_038_461,
      new_double: 22.4,
      new_list: [sub],
      new_string: "This is in the sub struct",
      new_set: MapSet.new([sub, sub, sub]),
      new_map: %{16 => sub, 32 => sub},
      username: "mr. stinky"
    }

    changey = %ChangeyStruct{id: 12345, username: "stinkypants", my_twin: twin}

    serialized = serialize(ChangeyStruct, changey)
    {deserialized, ""} = OldChangeyStruct.deserialize(serialized)

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  test "primitive serializers work as expected" do
    assert <<1::size(8)>> == Binary.serialize(:bool, true)
    assert <<0::size(8)>> == Binary.serialize(:bool, false)

    assert <<38::signed-8>> == Binary.serialize(:i8, 38)
    assert <<-23::signed-8>> == Binary.serialize(:i8, -23)

    assert <<65_535::signed-16>> == Binary.serialize(:i16, 65_535)
    assert <<-65_535::signed-16>> == Binary.serialize(:i16, -65_535)

    max_32 = round(:math.pow(2, 31)) - 1
    negative_max_32 = -max_32
    assert <<^max_32::signed-32>> = Binary.serialize(:i32, max_32)
    assert <<^negative_max_32::signed-32>> = Binary.serialize(:i32, -max_32)

    max_64 = round(:math.pow(2, 63)) - 1
    negative_max_64 = -max_64
    assert <<^max_64::signed-64>> = Binary.serialize(:i64, max_64)
    assert <<^negative_max_64::signed-64>> = Binary.serialize(:i64, -max_64)

    nan = %Thrift.NaN{sign: 1, fraction: 456}
    assert <<332.2178::signed-float>> == Binary.serialize(:double, 332.2178)
    assert <<1::1, 2047::11, 456::52>> == Binary.serialize(:double, nan)
    assert <<0::1, 2047::11, 0::52>> == Binary.serialize(:double, :inf)
    assert <<1::1, 2047::11, 0::52>> == Binary.serialize(:double, :"-inf")

    assert [<<5::size(32)>>, "Hello"] == Binary.serialize(:string, "Hello")
  end

  test "list serializers encode correctly" do
    list_of_i16s = [382, 0, -320, 28]
    assert [<<6::size(8), 4::32-signed>>, rest] = Binary.serialize({:list, :i16}, list_of_i16s)
    assert rest == Enum.map(list_of_i16s, &Binary.serialize(:i16, &1))
  end

  test "set serializers encode correctly" do
    set_of_i16s = MapSet.new([382, 0, -320, 28])
    assert [<<6::size(8), 4::32-signed>>, rest] = Binary.serialize({:set, :i16}, set_of_i16s)
    assert rest == Enum.map(set_of_i16s, &Binary.serialize(:i16, &1))
  end

  test "map serializers encode correctly" do
    map_to_serialize = %{"elixir" => 100, "Java" => 1, "Ruby" => 75, "Python" => 74}
    serialized = Binary.serialize({:map, {:string, :i16}}, map_to_serialize)
    assert [<<11::size(8), 6::size(8), 4::32-signed>>, kvps] = serialized

    assert kvps ==
             Enum.map(
               map_to_serialize,
               fn {k, v} -> [Binary.serialize(:string, k), Binary.serialize(:i16, v)] end
             )
  end
end
