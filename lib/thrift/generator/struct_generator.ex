defmodule Thrift.Generator.StructGenerator do
  alias Thrift.Generator.StructBinaryProtocol
  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup

  def generate(label, schema, name, struct) when label in [:struct, :union, :exception] do
    struct_parts = Enum.map(struct.fields, fn
      %{name: name, default: nil, type: type} ->
        {name, zero(schema, type)}
      %{name: name, default: %MapSet{map: m}} ->
        values = Map.keys(m)
        {name, quote do: MapSet.new(unquote(values))}
      %{name: name, default: default} when not is_nil(default) ->
        {name, default}
    end)

    binary_protocol_defs = [
      StructBinaryProtocol.struct_serializer(struct, name, schema.file_group),
      StructBinaryProtocol.struct_deserializer(struct, name, schema.file_group),
    ]
    |> Utils.merge_blocks
    |> Utils.sort_defs

    define_block = case label do
      :struct ->
        quote do: defstruct unquote(struct_parts)
      :union ->
        quote do: defstruct unquote(struct_parts)
      :exception ->
        quote do: defexception unquote(struct_parts)
    end

    quote do
      defmodule unquote(name) do
        _ = unquote "Auto-generated Thrift #{label} #{struct.name}"
        unquote_splicing(for field <- struct.fields do
          quote do
            _ = unquote "#{field.id}: #{to_thrift(field.type, schema.file_group)} #{field.name}"
          end
        end)
        unquote(define_block)
        def new, do: %__MODULE__{}
        defmodule BinaryProtocol do
          unquote_splicing(binary_protocol_defs)
        end
        def serialize(struct) do
          BinaryProtocol.serialize(struct)
        end
        def serialize(struct, :binary) do
          BinaryProtocol.serialize(struct)
        end
        def deserialize(binary) do
          BinaryProtocol.deserialize(binary)
        end
      end
    end
  end

  # Zero values for built-in types
  defp zero(_schema, :bool), do: nil
  defp zero(_schema, :byte), do: nil
  defp zero(_schema, :i8), do: nil
  defp zero(_schema, :i16), do: nil
  defp zero(_schema, :i32), do: nil
  defp zero(_schema, :i64), do: nil
  defp zero(_schema, :double), do: nil
  defp zero(_schema, :string), do: nil
  defp zero(_schema, :binary), do: nil
  defp zero(_schema, {:map, _}), do: nil
  defp zero(_schema, {:list, _}), do: nil
  defp zero(_schema, {:set, _}), do: nil
  defp zero(_schema, %{values: [{_, value} | _]}), do: value
  defp zero(_schema, %Thrift.Parser.Models.Struct{}), do: nil
  defp zero(_schema, %Thrift.Parser.Models.Exception{}), do: nil
  defp zero(_schema, %Thrift.Parser.Models.Union{}), do: nil
  defp zero(_schema, :void), do: nil

  # Zero values for user defined types
  defp zero(schema, %{referenced_type: type} = ref) do
    cond do
      # Local references
      Map.has_key?(schema.enums, type) ->
        zero(schema, schema.enums[type])
      Map.has_key?(schema.typedefs, type) ->
        zero(schema, schema.typedefs[type])
      Map.has_key?(schema.structs, type) ->
        quote do: nil

      # Included references
      true ->
        case FileGroup.resolve(schema.file_group, ref) do
          nil ->
            raise "Unknown type: #{inspect type}"
          thing ->
            zero(schema, thing)
        end
    end
  end

  def to_thrift(base_type, _file_group) when is_atom(base_type) do
    Atom.to_string(base_type)
  end
  def to_thrift({:map, {key_type, val_type}}, file_group) do
    "map<#{to_thrift key_type, file_group},#{to_thrift val_type, file_group}>"
  end
  def to_thrift({:set, element_type}, file_group) do
    "set<#{to_thrift element_type, file_group}>"
  end
  def to_thrift({:list, element_type}, file_group) do
    "list<#{to_thrift element_type, file_group}>"
  end
  def to_thrift(%Thrift.Parser.Models.TEnum{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Thrift.Parser.Models.Struct{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Thrift.Parser.Models.Exception{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Thrift.Parser.Models.Union{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Thrift.Parser.Models.StructRef{referenced_type: type}, file_group) do
    FileGroup.resolve(file_group, type) |> to_thrift(file_group)
  end
end
