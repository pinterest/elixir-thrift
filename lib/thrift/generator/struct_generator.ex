defmodule Thrift.Generator.StructGenerator do
  alias Thrift.Generator.StructBinaryProtocol
  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    Constant,
    Exception,
    Field,
    Struct,
    TypeRef,
    TEnum,
    Union,
    ValueRef,
  }

  def generate(label, schema, name, struct) when label in [:struct, :union, :exception] do
    struct_parts = Enum.map(struct.fields, fn
      %Field{name: name, type: type, default: default} ->
        {name, default_value(default, type, schema)}
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

  def default_value(%ValueRef{} = ref, type, schema) do
    value = FileGroup.resolve(schema.file_group, ref)
    default_value(value, type, schema)
  end
  def default_value(value, %TypeRef{} = ref, schema) do
    type = FileGroup.resolve(schema.file_group, ref)
    default_value(value, type, schema)
  end
  def default_value(%Constant{type: %TypeRef{} = ref} = constant, type, schema) do
    constant = %Constant{constant | type: FileGroup.resolve(schema.file_group, ref)}
    default_value(constant, type, schema)
  end
  def default_value(%Constant{type: type, value: value}, type, schema) do
    default_value(value, type, schema)
  end
  def default_value(nil, %TEnum{values: [{_, value} | _]}, _schema) do
    value
  end
  def default_value(nil, _type, _schema) do
    nil
  end
  def default_value(value, :bool, _schema) when is_boolean(value), do: value
  def default_value(value, :byte, _schema) when is_integer(value), do: value
  def default_value(value, :double, _schema) when is_number(value), do: value
  def default_value(value, :i8, _schema) when is_integer(value), do: value
  def default_value(value, :i16, _schema) when is_integer(value), do: value
  def default_value(value, :i32, _schema) when is_integer(value), do: value
  def default_value(value, :i64, _schema) when is_integer(value), do: value
  def default_value(value, :string, _schema) when is_binary(value), do: value
  def default_value(value, :string, _schema) when is_list(value) do
    List.to_string(value)
  end
  def default_value(value, :binary, schema) do
    default_value(value, :string, schema)
  end
  def default_value(values, %Struct{fields: fields} = struct, schema) when is_list(values) do
    struct_module = FileGroup.dest_module(schema.file_group, struct)
    types = Map.new(fields, fn %Field{name: name, type: type} ->
      {name, type}
    end)
    defaults = Enum.map(fields, fn %Field{name: name, type: type, default: default} ->
      {name, default_value(default, type, schema)}
    end)
    values = Enum.map(values, fn {name_charlist, value} ->
      name = List.to_string(name_charlist) |> String.to_existing_atom
      type = Map.fetch!(types, name)
      {name, default_value(value, type, schema)}
    end)
    quote do
      %{unquote_splicing([{:__struct__, struct_module} | Keyword.new(defaults ++ values)])}
    end
  end
  def default_value(%{} = value, {:map, {key_type, value_type}}, schema) do
    Map.new(value, fn {key, value} ->
      {
        default_value(key, key_type, schema),
        default_value(value, value_type, schema),
      }
    end)
  end
  def default_value(%MapSet{} = set, {:set, type}, schema) do
    values = Enum.map(set, &default_value(&1, type, schema))
    quote do
      MapSet.new(unquote(values))
    end
  end
  def default_value(list, {:list, type}, schema) when is_list(list) do
    Enum.map(list, &default_value(&1, type, schema))
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
  def to_thrift(%TEnum{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Struct{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Exception{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%Union{name: name}, _file_group) do
    "#{name}"
  end
  def to_thrift(%TypeRef{referenced_type: type}, file_group) do
    FileGroup.resolve(file_group, type) |> to_thrift(file_group)
  end
end
