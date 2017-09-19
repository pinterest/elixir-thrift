defmodule Thrift.Protocol.Binary do
  @moduledoc """
  Provides a set of high-level functions for working with the Thrift binary
  protocol.

  The Thrift binary protocol uses a fairly simple binary encoding scheme in
  which the length and type of a field are encoded as bytes followed by the
  actual value of the field.
  """

  alias Thrift.TApplicationException

  require Thrift.Protocol.Binary.Type, as: Type

  @type serializable :: Thrift.data_type | :message_begin | :application_exception
  @type deserializable :: :message_begin | :application_exception

  @stop 0

  @typedoc "Binary protocol message type identifier"
  @type message_type_id :: (1..4)

  @spec from_message_type(Thrift.message_type) :: message_type_id
  defp from_message_type(:call),      do: 1
  defp from_message_type(:reply),     do: 2
  defp from_message_type(:exception), do: 3
  defp from_message_type(:oneway),    do: 4

  @spec to_message_type(message_type_id) :: Thrift.message_type
  defp to_message_type(1), do: :call
  defp to_message_type(2), do: :reply
  defp to_message_type(3), do: :exception
  defp to_message_type(4), do: :oneway

  @typedoc "Binary protocol message sequence identifier"
  @type message_seq_id :: non_neg_integer

  @doc """
  Serializes a value as an IO list using Thrift's type-specific encoding rules.
  """
  @spec serialize(serializable, any) :: iolist
  def serialize(:bool, false),   do: <<0::8-signed>>
  def serialize(:bool, true),    do: <<1::8-signed>>
  def serialize(:i8, value),     do: <<value::8-signed>>
  def serialize(:i16, value),    do: <<value::16-signed>>
  def serialize(:i32, value),    do: <<value::32-signed>>
  def serialize(:i64, value),    do: <<value::64-signed>>
  def serialize(:double, value), do: <<value::float-signed>>
  def serialize(:string, value), do: [<<byte_size(value)::32-signed>>, value]
  def serialize(:binary, value), do: [<<byte_size(value)::32-signed>>, value]

  def serialize({:list, elem_type}, elems) when is_list(elems) do
    rest = Enum.map(elems, &serialize(elem_type, &1))
    [<<Type.of(elem_type)::8-signed, Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:set, elem_type}, %MapSet{} = elems) do
    rest = Enum.map(elems, &serialize(elem_type, &1))
    [<<Type.of(elem_type)::8-signed, Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:map, {key_type, val_type}}, map) when is_map(map) do
    elem_count = map_size(map)
    rest = Enum.map(map, fn {key, value} ->
      [serialize(key_type, key), serialize(val_type, value)]
    end)
    [<<Type.of(key_type)::8-signed, Type.of(val_type)::8-signed, elem_count::32-signed>>, rest]
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
    0::size(5), from_message_type(message_type)::size(3),
    byte_size(name)::32-signed, name::binary, sequence_id::32-signed>>
  end
  def serialize(:application_exception, %TApplicationException{message: message, type: type}) do
    type_id = TApplicationException.type_id(type)
    <<Type.string::size(8), 1::16-signed, byte_size(message)::size(32), message::binary,
      Type.i32::size(8), 2::16-signed, type_id::32-signed, @stop>>
  end

  @doc """
  Deserializes a Thrift-encoded binary.
  """
  @spec deserialize(deserializable, binary) ::
    {:ok, {Thrift.message_type, message_seq_id, name :: String.t, binary}} |
    {:error, {atom, binary}}
  def deserialize(:message_begin, <<1::size(1), 1::size(15), _::size(8),
                  0::size(5), message_type::size(3),
                  name_size::32-signed, name::binary-size(name_size), sequence_id::32-signed, rest::binary>>) do
    {:ok, {to_message_type(message_type), sequence_id, name, rest}}
  end

  # the old format, see here:
  # https://erikvanoosten.github.io/thrift-missing-specification/#_message_encoding
  def deserialize(:message_begin, <<name_size::32-signed, name::binary-size(name_size),
                  0::size(5), message_type::size(3), sequence_id::32-signed, rest::binary>>) do
    {:ok, {to_message_type(message_type), sequence_id, name, rest}}
  end

  def deserialize(:message_begin, rest) do
    {:error, {:cant_decode_message, rest}}
  end

  def deserialize(:application_exception, binary) when is_binary(binary) do
    do_read_application_exception(binary, Keyword.new())
  end

  defp do_read_application_exception(<<Type.string::size(8), 1::16-unsigned,
                                       message_size::32-signed,
                                       message::binary-size(message_size),
                                       rest::binary>>, opts) do
    do_read_application_exception(rest, Keyword.put(opts, :message, message))
  end
  defp do_read_application_exception(<<Type.i32::size(8), 2::16-unsigned,
                                       type::32-signed, rest::binary>>, opts) do
    do_read_application_exception(rest, Keyword.put(opts, :type, type))
  end
  defp do_read_application_exception(<<@stop>>, opts) do
    TApplicationException.exception(opts)
  end
  defp do_read_application_exception(error, _) do
    TApplicationException.exception(
      type: :protocol_error,
      message: "Unable to decode exception (#{inspect error})")
  end

  @doc """
  Skips over the bytes representing a binary-encoded field.

  This is useful for jumping over unrecognized fields in the serialized byte
  stream.
  """
  @spec skip_field(binary, Type.t) :: binary | :error
  def skip_field(<<_, rest::binary>>, unquote(Type.bool)), do: rest
  def skip_field(<<_, rest::binary>>, unquote(Type.byte)), do: rest
  def skip_field(<<_::float-signed, rest::binary>>, unquote(Type.double)), do: rest
  def skip_field(<<_::16-signed, rest::binary>>, unquote(Type.i16)), do: rest
  def skip_field(<<_::32-signed, rest::binary>>, unquote(Type.i32)), do: rest
  def skip_field(<<_::64-signed, rest::binary>>, unquote(Type.i64)), do: rest
  def skip_field(<<length::32-signed, _::binary-size(length), rest::binary>>, unquote(Type.string)) do
    rest
  end
  def skip_field(<<rest::binary>>, unquote(Type.struct)) do
    skip_struct(rest)
  end
  def skip_field(<<key_type, val_type, length::32-signed, rest::binary>>, unquote(Type.map)) do
    skip_map_entry(rest, key_type, val_type, length)
  end
  def skip_field(<<elem_type, length::32-signed, rest::binary>>, unquote(Type.set)) do
    skip_list_element(rest, elem_type, length)
  end
  def skip_field(<<elem_type, length::32-signed, rest::binary>>, unquote(Type.list)) do
    skip_list_element(rest, elem_type, length)
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
