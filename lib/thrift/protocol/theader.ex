defmodule Thrift.Protocol.THeader do
  alias Thrift.Protocol.Binary
  alias Thrift.TApplicationException

  # THeader format:
  # https://github.com/apache/thrift/blob/master/doc/specs/HeaderFormat.md

  @header_magic 0x0FFF

  @binary_protocol 0
  @compact_protocol 2

  @zlib_transform 1
  @hmac_transform 2
  @snappy_transform 3

  @info_keyvalue 1

  # THeader format:
  # https://github.com/apache/thrift/blob/master/doc/specs/HeaderFormat.md
  def deserialize(
        <<@header_magic::size(16), _flags::size(16), _sequence_number::size(32),
          header_size::size(16), rest::binary>>
      ) do
    header_size_bytes = header_size * 4
    <<header_binary::binary-size(header_size_bytes), payload::binary>> = rest
    headers = deserialize_header(header_binary)

    with {:ok, message} <- headers.protocol.deserialize(:message_begin, payload) do
      {:ok, headers, message}
    end
  end

  # Try other protocols.
  def deserialize(payload) do
    with {:ok, message} <- Binary.deserialize(:message_begin, payload) do
      headers = %{protocol: Binary}
      {:ok, headers, message}
    end
  end

  def deserialize_header(rest) do
    {protocol_id, rest} = decode_varint(rest)
    headers = %{protocol: protocol_from_id(protocol_id)}
    {num_transforms, rest} = decode_varint(rest)
    deserialize_transforms(rest, headers, num_transforms)
  end

  def protocol_from_id(@binary_protocol) do
    Binary
  end

  def protocol_from_id(@compact_protocol) do
    raise TApplicationException,
      type: :invalid_protocol,
      message: "Compact protocol not supported"
  end

  def protocol_from_id(protocol_id) do
    raise TApplicationException,
      type: :invalid_protocol,
      message: "Invalid protocol: #{protocol_id}"
  end

  def deserialize_transforms(
        <<@zlib_transform, rest::binary>>,
        headers,
        num_transforms
      )
      when num_transforms > 0 do
    headers = Map.put(headers, :zlib, true)
    deserialize_transforms(rest, headers, num_transforms - 1)
  end

  def deserialize_transforms(
        <<@hmac_transform, size, data::binary-size(size), rest::binary>>,
        headers,
        num_transforms
      )
      when num_transforms > 0 do
    headers = Map.put(headers, :hmac, data)
    deserialize_transforms(rest, headers, num_transforms - 1)
  end

  def deserialize_transforms(
        <<@snappy_transform, rest::binary>>,
        headers,
        num_transforms
      )
      when num_transforms > 0 do
    headers = Map.put(headers, :snappy, true)
    deserialize_transforms(rest, headers, num_transforms - 1)
  end

  def deserialize_transforms(<<transform, _rest::binary>>, _headers, num_transforms)
      when num_transforms > 0 do
    raise TApplicationException,
      type: :invalid_transform,
      message: "Invalid transform: #{transform}"
  end

  def deserialize_transforms(rest, headers, 0 = _num_transforms) do
    deserialize_infos(rest, headers)
  end

  def deserialize_infos(<<@info_keyvalue, rest::binary>>, headers) do
    {num_headers, rest} = decode_varint(rest)
    {headers, rest} = deserialize_info_keyvalue(rest, headers, num_headers)
    deserialize_infos(rest, headers)
  end

  # We're done when either we run out of binary to parse or we encounter an unrecognized info type.
  def deserialize_infos(_rest, headers) do
    headers
  end

  def deserialize_info_keyvalue(rest, headers, num_headers) when num_headers > 0 do
    {key, rest} = decode_varstring(rest)
    {value, rest} = decode_varstring(rest)
    headers = Map.put(headers, key, value)
    deserialize_info_keyvalue(rest, headers, num_headers - 1)
  end

  def deserialize_info_keyvalue(rest, headers, 0 = _num_headers) do
    {headers, rest}
  end

  def decode_varint(<<0::1, a::7, rest::binary>>) do
    varint = a
    {varint, rest}
  end

  def decode_varint(<<1::1, a::7, 0::1, b::7, rest::binary>>) do
    <<varint::16>> = <<0::2, b::7, a::7>>
    {varint, rest}
  end

  def decode_varint(<<1::1, a::7, 1::1, b::7, 0::1, c::7, rest::binary>>) do
    <<varint::24>> = <<0::3, c::7, b::7, a::7>>
    {varint, rest}
  end

  def decode_varint(<<1::1, a::7, 1::1, b::7, 1::1, c::7, 0::1, d::7, rest::binary>>) do
    <<varint::32>> = <<0::4, d::7, c::7, b::7, a::7>>
    {varint, rest}
  end

  def decode_varstring(rest) do
    {length, rest} = decode_varint(rest)
    <<string::binary-size(length), rest::binary>> = rest
    {string, rest}
  end
end
