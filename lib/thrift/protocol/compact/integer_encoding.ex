defmodule Thrift.Protocol.Compact.IntegerEncoding do
  @moduledoc """
  Compact protocol encodes integers using the same method as Protocol buffers. See
  https://developers.google.com/protocol-buffers/docs/encoding and
  https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md

  For now this is a wrapper around https://github.com/whatyouhide/small_ints to make it
  easier to replace that library if warranted.

  Todo: it definitely does want replacing to fail more gracefully with invalid binaries, eg <<255>>
  """

  @spec encode_zigzag_varint(integer()) :: binary()
  defdelegate encode_zigzag_varint(integer), to: :small_ints

  @doc """
  Encode the signed integer, but as if it was being encoded in
  normally, signed, with `max_pseudo_bits` of bits available.
  eg

  iex> 127 |> IntegerEncoding.encode_zigzag_varint(8) |> IntegerEncoding.decode_zigzag_varint()
  {127, ""}

  iex> 128 |> IntegerEncoding.encode_zigzag_varint(8) |> IntegerEncoding.decode_zigzag_varint()
  {-128, ""}
  """
  @spec encode_zigzag_varint(integer(), non_neg_integer()) :: binary()
  def encode_zigzag_varint(integer, max_pseudo_bits) do
    integer
    |> constrain_to_bits(max_pseudo_bits)
    |> :small_ints.encode_zigzag_varint()
  end

  @spec encode_varint(non_neg_integer()) :: binary()
  defdelegate encode_varint(integer), to: :small_ints

  @spec decode_varint(binary()) :: non_neg_integer()
  defdelegate decode_varint(binary), to: :small_ints

  @spec decode_zigzag_varint(binary()) :: non_neg_integer()
  defdelegate decode_zigzag_varint(binary), to: :small_ints

  defp constrain_to_bits(integer, bits) do
    <<i::size(bits)-signed>> = <<integer::size(bits)-signed>>
    i
  end
end
