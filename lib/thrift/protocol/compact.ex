defmodule Thrift.Protocol.Compact do
  alias Thrift.Protocol.Compact.IntegerEncoding
  require Thrift.Protocol.Compact.Type, as: Type

  def field_header({previous_id, id}, type) when is_integer(type) do
    do_field_header(id, id - previous_id, type)
  end

  def type_id(type) do
    Type.of(type)
  end

  defp do_field_header(_id, delta, type) when delta < 16 do
    <<delta::4-unsigned, type::4-unsigned>>
  end

  defp do_field_header(id, _delta, type) do
    :erlang.iolist_to_binary([<<type::8-unsigned>>, IntegerEncoding.encode_zigzag_varint(id)])
  end

  def deserialize_binary(binary) do
    with {size, rest} <- IntegerEncoding.decode_varint(binary),
         <<result::binary-size(size), rest::binary>> <- rest do
      {result, rest}
    else
      _ ->
        :error
    end
  end
end
