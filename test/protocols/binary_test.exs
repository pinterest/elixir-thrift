defmodule BinaryProtocolTest do
  use ThriftTestCase, gen_erl: true
  use ExUnit.Case

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
    1: Status status
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
    1: bool is_true,
    2: byte byte_value,
    3: i16 sixteen_bits,
    4: i32 thirty_two_bits,
    5: i64 sixty_four_bits,
    6: double double_value,
    7: string string_value,
    8: binary raw_binary
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
    1: i64 id,
    2: string username,
  }

  struct Containers {
   1: list<i64> users,
   2: list<Weather> weekly_forecast,
   3: set<string> taken_usernames,
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
      1: i64 id,
      2: containers.Friend best_friend;
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
end
