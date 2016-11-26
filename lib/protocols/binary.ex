defmodule Thrift.Protocols.Binary do
  alias Thrift.Parser.FileGroup
  alias Thrift.Generator.Utils

  # field types, which are the type ids from the thrift spec.
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

  @types %{bool: @bool,
           byte: @byte,
           double: @double,
           i8: @byte,
           i16: @i16,
           i32: @i32,
           i64: @i64,
           string: @string,
           binary: @string,
           struct: @struct,
           map: @map,
           set: @set,
           list: @list
          }

  alias Thrift.Parser.Models.{
    Exception,
    Field,
    Struct,
    TEnum,
  }

  def build(file_group, struct) do
    alias Thrift.Generator.Models.BinaryProtocol, as: Deserializer
    name = FileGroup.dest_module(file_group, struct.name)

    defs = [
      Deserializer.struct_deserializer(struct, name, file_group),
    ]
    |> Utils.merge_blocks
    |> Utils.sort_defs

    quote do
      defmodule BinaryProtocol do
        unquote_splicing(defs)
      end
    end
  end

  for {atom_type, int_type} <- @types do
    def int_type(unquote(atom_type)) do
      unquote(int_type)
    end
  end
  def int_type({:map, _}), do: 13
  def int_type({:set, _}), do: 14
  def int_type({:list, _}), do: 15

  defp bool_to_int(false), do: 0
  defp bool_to_int(nil), do: 0
  defp bool_to_int(_), do: 1

  defp to_message_type(:call), do: 1
  defp to_message_type(:reply), do: 2
  defp to_message_type(:exception), do: 3
  defp to_message_type(:oneway), do: 4

  def serialize(_, nil) do
    []
  end
  def serialize(:bool, value) do
    value = bool_to_int(value)
    <<value::8-signed>>
  end
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

    [<<int_type(elem_type)::size(8), Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:set, elem_type}, %MapSet{}=elems) do
    rest = Enum.map(elems, &serialize(elem_type, &1))

    [<<int_type(elem_type)::size(8), Enum.count(elems)::32-signed>>, rest]
  end
  def serialize({:map, {key_type, val_type}}, map) when is_map(map) do
    elem_count = map_size(map)
    rest = Enum.map(map, fn {key, value} ->
      [serialize(key_type, key), serialize(val_type, value)]
    end)
    [<<int_type(key_type)::size(8), int_type(val_type)::size(8), elem_count::32-signed>>, rest]
  end
  def serialize(:struct, %{__struct__: mod}=struct) do
    mod.serialize(struct, :binary)
  end
  def serialize(:message_begin, {sequence_id, message_type, name}) do
    # Taken from https://erikvanoosten.github.io/thrift-missing-specification/#_message_encoding

    <<1::size(1), 1::size(15), 0::size(8),
    # ^^ Strange, I know. We could integrate the 8-bit zero here with the 5 bit zero below.
    0::size(5), to_message_type(message_type)::size(3),
    byte_size(name)::32-signed, sequence_id::32-signed>>
  end
end
