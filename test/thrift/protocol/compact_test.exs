defmodule Thrift.Protocol.CompactTest do
  use ExUnit.Case
  alias Thrift.Protocol.Compact

  describe "field_header" do
    test "short form when id delta is under 16" do
      assert Compact.field_header({0, 1}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({0, 15}, 3) == <<15::size(4), 3::size(4)>>
    end

    test "long form when id delta is over 15" do
      assert Compact.field_header({0, 16}, 3) == <<3, 32>>
      assert Compact.field_header({0, 64}, 3) == <<3, 128, 1>>
    end

    test "based on deltas" do
      assert Compact.field_header({0, 1}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({1, 2}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({1, 3}, 2) == <<2::size(4), 2::size(4)>>
    end
  end

  test "type id" do
    assert Compact.type_id({:bool, true}) == 1
    assert Compact.type_id({:bool, false}) == 2
  end

  test "deserialize binary" do
    assert Compact.deserialize_binary(<<5, "hello"::binary, 1, 2, 3>>) == {"hello", <<1, 2, 3>>}
    assert Compact.deserialize_binary(<<7, "hêllÕ"::binary, 1, 2, 3>>) == {"hêllÕ", <<1, 2, 3>>}

    assert Compact.deserialize_binary(<<5, "hell"::binary>>) == :error
    # assert Compact.deserialize_binary(<<255>>) == :error # todo
  end
end
