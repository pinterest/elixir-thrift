defmodule Thrift.Protocol.Compact.IntegerEncodingTest do
  use ExUnit.Case
  alias Thrift.Protocol.Compact.IntegerEncoding
  doctest IntegerEncoding

  test "encode_varint" do
    assert IntegerEncoding.encode_varint(0) == <<0>>
    assert IntegerEncoding.encode_varint(127) == <<127>>
    assert IntegerEncoding.encode_varint(128) == <<128, 1>>
    assert IntegerEncoding.encode_varint(255) == <<255, 1>>
    assert IntegerEncoding.encode_varint(256) == <<128, 2>>

    assert_raise ArgumentError, "argument error: -1", fn -> IntegerEncoding.encode_varint(-1) end
  end

  test "encode_zigzag_varint" do
    assert IntegerEncoding.encode_zigzag_varint(0) == <<0>>
    assert IntegerEncoding.encode_zigzag_varint(-1) == <<1>>
    assert IntegerEncoding.encode_zigzag_varint(1) == <<2>>
    assert IntegerEncoding.encode_zigzag_varint(-2) == <<3>>
    assert IntegerEncoding.encode_zigzag_varint(2) == <<4>>
    assert IntegerEncoding.encode_zigzag_varint(-64) == <<127>>
    assert IntegerEncoding.encode_zigzag_varint(64) == <<128, 1>>
    assert IntegerEncoding.encode_zigzag_varint(-128) == <<255, 1>>
  end

  test "decode_varint" do
    assert IntegerEncoding.decode_varint(<<0>>) == {0, ""}
    assert IntegerEncoding.decode_varint(<<0, "hello">>) == {0, "hello"}

    assert IntegerEncoding.decode_varint(<<127>>) == {127, ""}
    assert IntegerEncoding.decode_varint(<<128, 1>>) == {128, ""}
    assert IntegerEncoding.decode_varint(<<255, 1>>) == {255, ""}
    assert IntegerEncoding.decode_varint(<<128, 2>>) == {256, ""}

    assert IntegerEncoding.decode_varint(<<255>>) == :error
    assert IntegerEncoding.decode_varint(<<128>>) == :error
  end

  test "decode_zigzag_varint" do
    assert IntegerEncoding.decode_zigzag_varint(<<0>>) == {0, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<0, "hello">>) == {0, "hello"}

    assert IntegerEncoding.decode_zigzag_varint(<<1>>) == {-1, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<2>>) == {1, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<3>>) == {-2, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<127>>) == {-64, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<128, 1>>) == {64, ""}
    assert IntegerEncoding.decode_zigzag_varint(<<128, 1>>) == {64, ""}

    assert IntegerEncoding.decode_zigzag_varint(<<255>>) == :error

    assert IntegerEncoding.decode_zigzag_varint(<<128>>) == :error
  end
end
