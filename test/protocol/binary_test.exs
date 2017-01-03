defmodule BinaryProtocolTest do
  use ThriftTestCase, gen_erl: true
  use ExUnit.Case

  alias Thrift.Protocol.Binary

  @moduletag :integration

  def round_trip_struct(data, serializer_mf, deserializer_mf) do
    {serializer_mod, serializer_fn} = serializer_mf
    {deserializer_mod, deserializer_fn} = deserializer_mf

    serialized = :erlang.apply(serializer_mod, serializer_fn, [data, :binary])
    |> IO.iodata_to_binary

    :erlang.apply(deserializer_mod, deserializer_fn, [serialized])
  end

  @thrift_file name: "enums.thrift", contents: """
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
    encoder = {StructWithEnum, :serialize}
    decoder = {Erlang.Enums, :deserialize_struct_with_enum}

    assert {:StructWithEnum, 1} == round_trip_struct(StructWithEnum.new, encoder, decoder)
    assert {:StructWithEnum, 32} == round_trip_struct(%StructWithEnum{status: Status.evil}, encoder, decoder)
    assert {:StructWithEnum, 6} == round_trip_struct(%StructWithEnum{status: Status.banned}, encoder, decoder)
  end

  @thrift_file name: "scalars.thrift", contents: """
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
    encoder = {Scalars, :serialize}
    decoder = {Erlang.Scalars, :deserialize_scalars}

    assert Erlang.Scalars.new_scalars(is_true: true) == round_trip_struct(%Scalars{is_true: true}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(byte_value: 127) == round_trip_struct(%Scalars{byte_value: 127}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(sixteen_bits: 12723) == round_trip_struct(%Scalars{sixteen_bits: 12723}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(thirty_two_bits: 1_8362_832) == round_trip_struct(%Scalars{thirty_two_bits: 1_8362_832}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(sixty_four_bits: 8872372) == round_trip_struct(%Scalars{sixty_four_bits: 8872372}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(double_value: 2.37219) == round_trip_struct(%Scalars{double_value: 2.37219}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(string_value: "I am a string") == round_trip_struct(%Scalars{string_value: "I am a string"}, encoder, decoder)

    assert Erlang.Scalars.new_scalars(raw_binary: <<224, 186, 2, 1, 0>>) == round_trip_struct(%Scalars{raw_binary: <<224, 186, 2, 1, 0>>}, encoder, decoder)
  end

  thrift_test "it should not encode unset fields" do
    encoded =  Scalars.serialize(%Scalars{})
    |> IO.iodata_to_binary

    assert <<0>> == encoded
  end

  @thrift_file name: "containers.thrift", contents: """
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
   // Lists of structs are broken
   //  4: list<Friend> friends,
   // Deserializers for maps break the build
   // 3: map<i64, Weather> user_forecasts,
   //  4: map<string, User> users_by_username
  }
  """

  thrift_test "containers serialize properly" do
    encoder = {Containers, :serialize}
    decoder = {Erlang.Containers, :deserialize_containers}

    # unset containers become undefined
    assert Erlang.Containers.new_containers() == round_trip_struct(%Containers{}, encoder, decoder)

    # empty containers are sent
    assert Erlang.Containers.new_containers(users: []) == round_trip_struct(%Containers{users: []}, encoder, decoder)

    # containers can contain enums
    forecast = [Weather.sunny,
                Weather.sunny,
                Weather.sunny,
                Weather.sunny,
                Weather.cloudy,
                Weather.sunny,
                Weather.sunny]
    assert Erlang.Containers.new_containers(weekly_forecast: [1, 1, 1, 1, 2, 1, 1]) == round_trip_struct(%Containers{weekly_forecast: forecast}, encoder, decoder)
    taken_usernames = ["scohen", "pguillory"]
    assert Erlang.Containers.new_containers(taken_usernames: :sets.from_list(taken_usernames)) == round_trip_struct(%Containers{taken_usernames: MapSet.new(taken_usernames)}, encoder, decoder)

    # # containers can contain structs
    # erlang_friends = [
    #   Erlang.Containers.new_friend(id: 1, username: "scohen"),
    #   Erlang.Containers.new_friend(id: 2, username: "pguillory"),
    #   Erlang.Containers.new_friend(id: 3, username: "dantswain"),
    # ]
    # assert Erlang.Containers.new_containers(friends: erlang_friends) == round_trip_struct(%Containers{
    #       friends:
    #       [%Friend{id: 1, username: "scohen"},
    #        %Friend{id: 2, username: "pguillory"},
    #        %Friend{id: 3, username: "dantswain"}
    #       ]}, encoder, decoder)
  end


  @thrift_file name: "across.thrift", contents: """
    include "containers.thrift"

    struct User {
      1: optional i64 id,
      2: optional containers.Friend best_friend;
    }
  """

  thrift_test "serializing structs across modules" do
    encoder = {User, :serialize}
    decoder = {Erlang.Across, :deserialize_user}

    erl_user = Erlang.Across.new_user(
      id: 1234,
      best_friend: Erlang.Containers.new_friend(id: 3282, username: "stinkypants"))

    assert erl_user == round_trip_struct(%User{
          id: 1234,
          best_friend: %Friend{id: 3282, username: "stinkypants"}}, encoder, decoder)
  end

  @thrift_file name: "old.thrift", contents: """
    struct OldChangeyStruct {
      1: optional i64 id,
      2: optional string username
    }
  """
  @thrift_file name: "new.thrift", contents: """
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
    changey = Erlang.New.new_changey_struct(
      id: 12345,
      username: "stinkypants",
      my_bool: false,
      new_byte: 3,
      new_double: 19.3,
      new_i16: 4821,
      new_i32: 1_284_291,
      new_i64: 128_382_100_315,
      new_string: "suprise! an unexpected field!",
      grooviness: 1
    )
    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a substruct you don't know about" do
    sub_struct = Erlang.New.new_sub_struct(password: "letmein")

    changey = Erlang.New.new_changey_struct(
      id: 12345,
      username: "stinkypants",
      new_substruct: sub_struct
    )
    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing nested structs you don't know about" do
    sub_sub = Erlang.New.new_sub_sub_struct(is_this_excessive: true)
    sub = Erlang.New.new_sub_struct(password: "1234", sub_sub: sub_sub)
    changey = Erlang.New.new_changey_struct(id: 12345,
                                            username: "stinkypants",
                                            new_substruct: sub)

    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a list of structs you don't know about" do
    sub = Erlang.New.new_sub_struct(password: "1234")
    changey = Erlang.New.new_changey_struct(id: 12345,
                                            username: "stinkypants",
                                            new_list: [sub])

    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a set of structs you don't know about" do
    sub = Erlang.New.new_sub_struct(password: "1234")
    changey = Erlang.New.new_changey_struct(id: 12345,
                                            username: "stinkypants",
                                            new_set: :sets.from_list([sub, sub]))

    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a map of structs you don't know about" do
    sub = Erlang.New.new_sub_struct(password: "1234")
    changey = Erlang.New.new_changey_struct(id: 12345,
                                            username: "stinkypants",
                                            new_map: :dict.from_list([{1, sub}, {2, sub}]))

    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end

  thrift_test "deserializing a recursive sub struct with all types" do
    sub = Erlang.New.new_sub_struct(password: "1234")
    twin = Erlang.New.new_changey_struct(
      id: 6789,
      new_bool: false,
      new_byte: 4,
      new_i16: 102,
      new_i32: 919482,
      new_i64: 18281038461,
      new_double: 22.4,
      new_list: [sub],
      new_string: "This is in the sub struct",
      new_set: :sets.from_list([sub, sub, sub]),
      new_map: :dict.from_list([{16, sub}, {32, sub}]),
      username: "mr. stinky")

    changey = Erlang.New.new_changey_struct(
      id: 12345,
      username: "stinkypants",
      my_twin: twin)

    {deserialized, ""} = changey
    |> Erlang.New.serialize_changey_struct
    |> OldChangeyStruct.BinaryProtocol.deserialize

    assert %OldChangeyStruct{id: 12345, username: "stinkypants"} == deserialized
  end


  # test "nil nested fields get their default value" do
  #   erlang_nested = serialize_nesting_to_erlang(user: user(:elixir, username: "frank"))

  #   assert {:Nesting, user, nested} = erlang_nested
  #   assert user == user(:erlang, username: "frank")
  #   assert nested == {:SharedStruct, 44291, "Look at my value..."}

  #   erlang_nested = serialize_nesting_to_erlang(nested: %Shared.SharedStruct{key: 2916, value: "my value"})

  #   assert {:Nesting, user, nested} = erlang_nested
  #   assert nested == {:SharedStruct, 2916, "my value"}
  #   assert user == user(:erlang)
  # end

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

    assert <<332.2178::signed-float>> == Binary.serialize(:double, 332.2178)

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

    assert kvps == Enum.map(map_to_serialize,
      fn {k, v} -> [Binary.serialize(:string, k), Binary.serialize(:i16, v)] end)
  end

end
