defmodule BinaryProtocolTest do
  use ExUnit.Case

  @thrift_file_path "./test/fixtures/app/thrift/simple.thrift"

  import ParserUtils

  setup_all do
    @thrift_file_path
    |> parse_thrift
    |> compile_module

    :ok
  end

  test "it should not encode empty fields" do
    assert <<0>> == serialize_user(user(:elixir))
  end

  test "if only one field is specified" do
    assert serialize_user(user(:erlang, username: "esteban")) == serialize_user(user(:elixir, username: "esteban"))
  end

  test "all the supported types are available" do
    assert user(:erlang, username: "jedi") == serialize_user_to_erlang(username: "jedi")

    assert user(:erlang, is_evil: false) == serialize_user_to_erlang(is_evil: false)
    assert user(:erlang, is_evil: true) == serialize_user_to_erlang(is_evil: true)

    assert user(:erlang, number_of_hairs_on_head: 824) == serialize_user_to_erlang(number_of_hairs_on_head: 824)

    assert user(:erlang, amount_of_red: 127)  == serialize_user_to_erlang(amount_of_red: 127)

    assert user(:erlang, nineties_era_color: 32767) == serialize_user_to_erlang(nineties_era_color: 32767)
    assert user(:erlang, mint_gum: 2834.4814) == serialize_user_to_erlang(mint_gum: 2834.4814)
    assert user(:erlang, friends: [user(:erlang, username: "Cedric")]) == serialize_user_to_erlang(friends: [user(:elixir, username: "Cedric")])

    assert user(:erlang, blocked_user_ids: :sets.from_list([4, 8, 32])) == serialize_user_to_erlang(blocked_user_ids: MapSet.new([4,8, 32]))
    assert user(:erlang, my_map: :dict.from_list([{23, "mornin"}])) == serialize_user_to_erlang(my_map: %{23 => "mornin"})
  end

  test "it should be able to be decoded by thrift" do
    erlang_user = serialize_user_to_erlang(username: "esteban", mint_gum: 23.832,
                                           friends: [user(:elixir, username: "frank"), user(:elixir, username: "erica")])

    assert {:User, :undefined, :undefined, :undefined, :undefined, :undefined, 23.832,
            "esteban", friends, :undefined, :undefined, :undefined} = erlang_user

    assert [user(:erlang, username: "frank"), user(:erlang, username: "erica")] == friends
  end

  test "optional empty lists are sent" do
    assert <<0>> == serialize_user(user(:elixir, optional_integers: nil))
    assert user(:erlang, optional_integers: [])  == serialize_user_to_erlang(optional_integers: [])
  end
end
