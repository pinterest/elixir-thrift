defmodule Thrift.Protocol.Compact do
  @moduledoc """
  Provides a set of high-level functions for working with the Thrift compact
  protocol.

  The Thrift compact protocol is comparable with the binary protocol but uses
  various tricks and optimisations to decrease the size of the serialised form.

  These make the compact protocol a little more complicated.
  """
  alias Thrift.Protocol.Compact.IntegerEncoding
  require Thrift.Protocol.Compact.Type, as: Type

  @type remainder :: binary()

  @fixed_length_contained_types [Type.contained_bool(), Type.byte(), Type.double()]

  @doc """
  Creates a Struct field header. Short form if `id` - `previous_id` is between
  1 and 15 (inclusive), else long form. Type is a type code.
  eg
  iex> Compact.field_header({0, 15}, 3)
  <<15::4, 3::4>>

  iex> Compact.field_header({0, 16}, 3)
  <<0::4, 3::4, 32>>
  """
  @spec field_header({integer(), integer()}, Type.t) :: binary()
  def field_header({previous_id, id}, type) when is_integer(type) do
    do_field_header(id, id - previous_id, type)
  end

  @doc """
  Type definition to type id.
  """
  @spec type_id(Type.data_type) :: Type.t
  def type_id(type), do: Type.of(type)

  defp do_field_header(_id, delta, type) when delta > 0 and delta < 16 do
    <<delta::4-unsigned, type::4-unsigned>>
  end

  defp do_field_header(id, _delta, type) do
    :erlang.iolist_to_binary([<<type::unsigned>>, IntegerEncoding.encode_zigzag_varint(id)])
  end

  @doc """
  Deserialise a compact binary. Takes binary of the appropriate size out of the binary
  and returns a tuple with the result, and any remainder.
  """
  @spec deserialize_binary(binary()) :: :error | {binary(), remainder()}
  def deserialize_binary(binary) do
    with {size, rest} <- IntegerEncoding.decode_varint(binary),
         <<result::binary-size(size), rest::binary>> <- rest do
      {result, rest}
    else
      _ ->
        :error
    end
  end

  @doc """
  Skip the next field encoded in the binary, returning the remainder.
  """
  @spec skip_field(binary()) :: :error | remainder()
  def skip_field(<<0::4, type_id::4-unsigned, rest::binary>>) do
    case IntegerEncoding.decode_zigzag_varint(rest) do
      {_field_delta, rest} -> skip_value(type_id, rest)
      err -> err
    end
  end

  def skip_field(<<_::4, type_id::4-unsigned, rest::binary>>) do
    skip_value(type_id, rest)
  end

  def skip_field(_), do: :error

  defp skip_value(type, <<0b1111::4, elem_type::4, rest::binary>>)
       when type in [Type.list(), Type.set()] do
    skip_n_elements(elem_type, rest)
  end

  defp skip_value(type, <<size::4, elem_type::4, rest::binary>>)
       when type in [Type.list(), Type.set()] do
    skip_n_elements(elem_type, size, rest)
  end

  defp skip_value(Type.true_bool(), rest), do: rest
  defp skip_value(Type.false_bool(), rest), do: rest
  defp skip_value(Type.byte(), <<_, rest::binary>>), do: rest
  defp skip_value(Type.double(), <<_::64-float-little, rest::binary>>), do: rest
  defp skip_value(Type.string(), rest), do: skip_n_elements(Type.byte(), rest)
  defp skip_value(Type.map(), rest), do: skip_map(rest)
  defp skip_value(Type.struct(), rest), do: skip_struct(rest)

  defp skip_value(type, binary) when type in [Type.i16(), Type.i32(), Type.i64()] do
    case IntegerEncoding.decode_zigzag_varint(binary) do
      {_, rest} -> rest
      err -> err
    end
  end

  defp skip_value(_, _), do: :error

  defp skip_contained_value(Type.contained_bool(), <<_, rest::binary>>), do: {:ok, rest}
  defp skip_contained_value(Type.contained_bool(), _), do: :error

  defp skip_contained_value(type, rest) do
    case skip_value(type, rest) do
      :error -> :error
      rest -> {:ok, rest}
    end
  end

  defp skip_n_elements(type, binary) do
    case IntegerEncoding.decode_varint(binary) do
      {number_of_elements, rest} -> skip_n_elements(type, number_of_elements, rest)
      _ -> :error
    end
  end

  defp skip_n_elements(type, number_of_elements, rest)
       when type in @fixed_length_contained_types do
    size = element_byte_size(type) * number_of_elements

    case rest do
      <<_::binary-size(size), rest::binary>> -> rest
      _ -> :error
    end
  end

  defp skip_n_elements(_type, 0, rest) do
    rest
  end

  defp skip_n_elements(type, remaining, rest) do
    case skip_contained_value(type, rest) do
      {:ok, rest} ->
        skip_n_elements(type, remaining - 1, rest)

      _ ->
        :error
    end
  end

  defp skip_map(<<0, rest::binary>>), do: rest

  defp skip_map(rest) do
    case IntegerEncoding.decode_varint(rest) do
      {size, <<key_type::4-unsigned, value_type::4-unsigned, rest::binary>>} ->
        skip_map(size, {key_type, value_type}, rest)

      _ ->
        :error
    end
  end

  defp skip_map(size, {key_type, value_type}, rest)
       when key_type in @fixed_length_contained_types and
              value_type in @fixed_length_contained_types do
    skip_size = (element_byte_size(key_type) + element_byte_size(value_type)) * size

    case rest do
      <<_::binary-size(skip_size), rest::binary>> -> rest
      _ -> :error
    end
  end

  defp skip_map(0, _kv, rest), do: rest

  defp skip_map(remaining, {key_type, value_type} = kv, rest) do
    with {:ok, rest} <- skip_contained_value(key_type, rest),
         {:ok, rest} <- skip_contained_value(value_type, rest) do
      skip_map(remaining - 1, kv, rest)
    end
  end

  defp skip_struct(<<0, rest::binary>>), do: rest
  defp skip_struct(rest) do
    case skip_field(rest) do
      :error -> :error
      rest -> skip_struct(rest)
    end
  end

  defp element_byte_size(Type.contained_bool()), do: 1
  defp element_byte_size(Type.byte()), do: 1
  defp element_byte_size(Type.double()), do: 8
end
