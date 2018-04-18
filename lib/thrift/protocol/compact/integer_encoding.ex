defmodule Thrift.Protocol.Compact.IntegerEncoding do
  @moduledoc """
  Compact protocol encodes integers using the same method as Protocol buffers. See
  https://developers.google.com/protocol-buffers/docs/encoding and
  https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md

  Copied shamelessly from https://github.com/whatyouhide/small_ints to avoid an extra dependency,
  but also to fail gracefully when decoding invalid binaries, eg <<255>>
  """

  @spec encode_zigzag_varint(integer()) :: binary()
  def encode_zigzag_varint(i) when i < 0 do
    encode_varint(-i * 2 - 1)
  end

  def encode_zigzag_varint(i) do
    encode_varint(i * 2)
  end

  import Bitwise, only: [band: 2, bsr: 2, bsl: 2]

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
    |> encode_zigzag_varint()
  end

  @spec encode_varint(non_neg_integer()) :: binary()
  def encode_varint(i) when i >= 0 and i <= 127, do: <<i>>

  def encode_varint(i) when i > 127 do
    <<1::1, band(i, 127)::7, encode_varint(bsr(i, 7))::binary>>
  end

  def encode_varint(i), do: raise(ArgumentError, "argument error: #{inspect(i)}")

  @spec decode_varint(binary()) :: :error | {non_neg_integer(), binary()}
  def decode_varint(binary) do
    decode_varint(binary, {0, 0})
  end

  @spec decode_zigzag_varint(binary()) :: :error | {integer(), binary()}
  def decode_zigzag_varint(binary) do
    case decode_varint(binary) do
      :error -> :error

      {varint_val, rest} ->
        {decode_zigzag(varint_val), rest}
    end
  end

  defp decode_zigzag(i) do
    case rem(i, 2) do
      0 -> div(i, 2)
      1 -> div(-i, 2) - 1
    end
  end

  defp decode_varint(<<0::1, i::7, rest::binary>>, {pos, acc}) do
    {acc + bsl(i, pos), rest}
  end

  defp decode_varint(<<1::1, i::7, rest::binary>>, {pos, acc}) do
    decode_varint(rest, {pos + 7, acc + bsl(i, pos)})
  end

  defp decode_varint(_, _) do
    :error
  end

  defp constrain_to_bits(integer, bits) do
    <<i::size(bits)-signed>> = <<integer::size(bits)-signed>>
    i
  end
end
