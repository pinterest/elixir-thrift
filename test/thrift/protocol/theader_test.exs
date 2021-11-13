defmodule Thrift.Protocol.THeaderTest do
  alias Thrift.Protocol.THeader
  use ExUnit.Case, async: true

  test "decode_varint" do
    assert THeader.decode_varint(<<0b0000_0001>>) == {1, ""}
    assert THeader.decode_varint(<<0b1010_1100, 0b0000_0010>>) == {300, ""}

    assert THeader.decode_varint(<<0b00000000>>) == {0b00000000000000000000000000000000, ""}
    assert THeader.decode_varint(<<0b00000001>>) == {0b00000000000000000000000000000001, ""}
    assert THeader.decode_varint(<<0b00000010>>) == {0b00000000000000000000000000000010, ""}
    assert THeader.decode_varint(<<0b00000100>>) == {0b00000000000000000000000000000100, ""}
    assert THeader.decode_varint(<<0b00001000>>) == {0b00000000000000000000000000001000, ""}
    assert THeader.decode_varint(<<0b00010000>>) == {0b00000000000000000000000000010000, ""}
    assert THeader.decode_varint(<<0b00100000>>) == {0b00000000000000000000000000100000, ""}
    assert THeader.decode_varint(<<0b01000000>>) == {0b00000000000000000000000001000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00000001>>) ==
             {0b00000000000000000000000010000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00000010>>) ==
             {0b00000000000000000000000100000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00000100>>) ==
             {0b00000000000000000000001000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00001000>>) ==
             {0b00000000000000000000010000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00010000>>) ==
             {0b00000000000000000000100000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b00100000>>) ==
             {0b00000000000000000001000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b01000000>>) ==
             {0b00000000000000000010000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00000001>>) ==
             {0b00000000000000000100000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00000010>>) ==
             {0b00000000000000001000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00000100>>) ==
             {0b00000000000000010000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00001000>>) ==
             {0b00000000000000100000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00010000>>) ==
             {0b00000000000001000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b00100000>>) ==
             {0b00000000000010000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b01000000>>) ==
             {0b00000000000100000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00000001>>) ==
             {0b00000000001000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00000010>>) ==
             {0b00000000010000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00000100>>) ==
             {0b00000000100000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00001000>>) ==
             {0b00000001000000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00010000>>) ==
             {0b00000010000000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b00100000>>) ==
             {0b00000100000000000000000000000000, ""}

    assert THeader.decode_varint(<<0b10000000, 0b10000000, 0b10000000, 0b01000000>>) ==
             {0b00001000000000000000000000000000, ""}
  end

  test "deserialize_header" do
    header_binary =
      <<0, 0, 1, 2, 2, 98, 51, 49, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
        51, 50, 53, 99, 99, 52, 52, 97, 98, 98, 98, 51, 55, 102, 100, 100, 45, 51, 50, 53, 99, 99,
        52, 52, 97, 98, 98, 98, 51, 55, 102, 100, 100, 10, 58, 99, 108, 105, 101, 110, 116, 45,
        105, 100, 18, 97, 98, 99, 97, 98, 99, 97, 98, 99, 97, 98, 99, 97, 98, 99, 97, 98, 99, 0>>

    assert THeader.deserialize_header(header_binary) == %{
             :protocol => Thrift.Protocol.Binary,
             ":client-id" => "abcabcabcabcabcabc",
             "b3" => "0000000000000000325cc44abbb37fdd-325cc44abbb37fdd"
           }
  end
end
