defmodule Thrift.Protocols.Binary do
  @moduledoc """
  Provides a set of high-level functions for working with the Thrift binary
  protocol.

  The Thrift binary protocol uses a fairly simple binary encoding scheme in
  which the length and type of a field are encoded as bytes followed by the
  actual value of the field.
  """

  @typedoc "Binary protocol field type identifier"
  @type type_id :: (2..15)

  @bool 2
  @byte 3
  @double 4
  @i16 6
  @i32 8
  @i64 10
  @string 11
  @struct 12
  @map 13
  @set 14
  @list 15

  @spec type_id(atom | {atom, atom}) :: type_id
  defp type_id(:bool),      do: @bool
  defp type_id(:byte),      do: @byte
  defp type_id(:i16),       do: @i16
  defp type_id(:i32),       do: @i32
  defp type_id(:i64),       do: @i64
  defp type_id(:double),    do: @double
  defp type_id(:string),    do: @string
  defp type_id(:struct),    do: @struct
  defp type_id(:map),       do: @map
  defp type_id(:set),       do: @set
  defp type_id(:list),      do: @list
  defp type_id({:map, _}),  do: @map
  defp type_id({:set, _}),  do: @set
  defp type_id({:list, _}), do: @list

  defp to_message_type(:call), do: 1
  defp to_message_type(:reply), do: 2
  defp to_message_type(:exception), do: 3
  defp to_message_type(:oneway), do: 4

  defp from_message_type(1), do: :call
  defp from_message_type(2), do: :reply
  defp from_message_type(3), do: :exception
  defp from_message_type(4), do: :oneway

  def serialize(_, nil) do
    []
  end
  def serialize(:bool, false), do: <<0::8-signed>>
  def serialize(:bool, true),  do: <<1::8-signed>>
  def serialize(:i8, value) do
    <<value::8-signed>>
  end
  def serialize(:i16, value) do
    <<value::16-signed>>
  end
  def serialize(:i32, value) do
    <<value::32-signed>>
  end
  def serialize(:i64, value) do
    <<value::64-signed>>
  end
  def serialize(:double, value) do
    <<value::signed-float>>
  end
  def serialize(:string, value) do
    [<<byte_size(value)::size(32)>>, value]
  end
  def serialize(:binary, value) do
    [<<byte_size(value)::size(32)>>, value]
  end
  def serialize({:list, elem_type}, elems) when is_list(elems) do
    rest = Enum.map(elems, &serialize(elem_type, &1))

    [<<type_id(elem_type)::size(8), Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:set, elem_type}, %MapSet{} = elems) do
    rest = Enum.map(elems, &serialize(elem_type, &1))

    [<<type_id(elem_type)::size(8), Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:map, {key_type, val_type}}, map) when is_map(map) do
    elem_count = map_size(map)
    rest = Enum.map(map, fn {key, value} ->
      [serialize(key_type, key), serialize(val_type, value)]
    end)
    [<<type_id(key_type)::size(8), type_id(val_type)::size(8), elem_count::32-signed>>, rest]
  end
  def serialize(:struct, %{__struct__: mod} = struct) do
    mod.serialize(struct, :binary)
  end
  def serialize(:union, %{__struct__: mod} = struct) do
    mod.serialize(struct, :binary)
  end
  def serialize(:message_begin, {message_type, sequence_id, name}) do
    # Taken from https://erikvanoosten.github.io/thrift-missing-specification/#_message_encoding

    <<1::size(1), 1::size(15), 0::size(8),
    # ^^ Strange, I know. We could integrate the 8-bit zero here with the 5 bit zero below.
    0::size(5), to_message_type(message_type)::size(3),
    byte_size(name)::32-signed, name::binary, sequence_id::32-signed>>
  end

  def deserialize(:message_begin,<<1::size(1), 1::size(15), _::size(8),
                  0::size(5), message_type::size(3),
                  name_size::32-signed, name::binary-size(name_size), sequence_id::32-signed, rest::binary>>) do
    {:ok, {from_message_type(message_type), sequence_id, name, rest}}
  end

  # the old format, see here:
  # https://erikvanoosten.github.io/thrift-missing-specification/#_message_encoding
  def deserialize(:message_begin, <<name_size::32-signed, name::binary-size(name_size),
                  0::size(5), message_type::size(3), sequence_id::32-signed, rest::binary>>) do
    {:ok, {from_message_type(message_type), sequence_id, name, rest}}
  end

  def deserialize(:message_begin, rest) do
    {:error, {:cant_decode_message, rest}}
  end

  @doc """
  Skips over the bytes representing a binary-encoded field.

  This is useful for jumping over unrecognized fields in the serialized byte
  stream.
  """
  @spec skip_field(binary, type_id) :: binary | :error
  def skip_field(<<_, rest::binary>>, unquote(@bool)), do: rest
  def skip_field(<<_, rest::binary>>, unquote(@byte)), do: rest
  def skip_field(<<_::signed-float, rest::binary>>, unquote(@double)), do: rest
  def skip_field(<<_::size(16), rest::binary>>, unquote(@i16)), do: rest
  def skip_field(<<_::size(32), rest::binary>>, unquote(@i32)), do: rest
  def skip_field(<<_::size(64), rest::binary>>, unquote(@i64)), do: rest
  def skip_field(<<length::32-signed, _::binary-size(length), rest::binary>>, unquote(@string)) do
    rest
  end
  def skip_field(<<rest::binary>>, unquote(@struct)) do
    rest |> skip_struct
  end
  def skip_field(<<key_type, val_type, length::size(32), rest::binary>>, unquote(@map)) do
    rest |> skip_map_entry(key_type, val_type, length)
  end
  def skip_field(<<elem_type, length::size(32), rest::binary>>, unquote(@set)) do
    rest |> skip_list_element(elem_type, length)
  end
  def skip_field(<<elem_type, length::size(32), rest::binary>>, unquote(@list)) do
    rest |> skip_list_element(elem_type, length)
  end
  def skip_field(_, _), do: :error

  defp skip_list_element(<<rest::binary>>, _, 0), do: rest
  defp skip_list_element(<<rest::binary>>, elem_type, remaining) do
    rest
    |> skip_field(elem_type)
    |> skip_list_element(elem_type, remaining - 1)
  end
  defp skip_list_element(:error, _, _), do: :error

  defp skip_map_entry(<<rest::binary>>, _, _, 0), do: rest
  defp skip_map_entry(<<rest::binary>>, key_type, val_type, remaining) do
    rest
    |> skip_field(key_type)
    |> skip_field(val_type)
    |> skip_map_entry(key_type, val_type, remaining - 1)
  end
  defp skip_map_entry(:error, _, _, _), do: :error

  defp skip_struct(<<0, rest::binary>>), do: rest
  defp skip_struct(<<type, _id::16-signed, rest::binary>>) do
    rest
    |> skip_field(type)
    |> skip_struct
  end
  defp skip_struct(_), do: :error
end
