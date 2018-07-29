defmodule Thrift.Generator.StructGenerator do
  alias Thrift.AST.{
    Exception,
    Field,
    Struct,
    TEnum,
    TypeRef,
    Union,
  }
  alias Thrift.Generator.{StructBinaryProtocol, Utils}
  alias Thrift.Parser.FileGroup

  def generate(label, schema, name, struct) when label in [:struct, :union, :exception] do
    struct_parts = Enum.map(struct.fields, fn
      %Field{name: name, type: type, default: default} ->
        {name, Utils.quote_value(default, type, schema)}
    end)

    binary_protocol_defs = [
      StructBinaryProtocol.struct_serializer(struct, name, schema.file_group),
      StructBinaryProtocol.struct_deserializer(struct, name, schema.file_group),
    ]
    |> Utils.merge_blocks
    |> Utils.sort_defs

    define_block = case label do
      :exception ->
        quote do: defexception unquote(struct_parts)
      _ ->
        quote do: defstruct unquote(struct_parts)
    end

    extra_defs = if label == :exception and not Keyword.has_key?(struct_parts, :message) do
      quote do
        @spec message(Exception.t) :: String.t
        def message(exception), do: inspect(exception)
      end
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
        @type t :: %__MODULE__{}
        def new, do: %__MODULE__{}
        unquote_splicing(List.wrap(extra_defs))
        defmodule BinaryProtocol do
          @moduledoc false
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

  defp to_thrift(base_type, _file_group) when is_atom(base_type) do
    Atom.to_string(base_type)
  end
  defp to_thrift({:map, {key_type, val_type}}, file_group) do
    "map<#{to_thrift key_type, file_group},#{to_thrift val_type, file_group}>"
  end
  defp to_thrift({:set, element_type}, file_group) do
    "set<#{to_thrift element_type, file_group}>"
  end
  defp to_thrift({:list, element_type}, file_group) do
    "list<#{to_thrift element_type, file_group}>"
  end
  defp to_thrift(%TEnum{name: name}, _file_group) do
    "#{name}"
  end
  defp to_thrift(%Struct{name: name}, _file_group) do
    "#{name}"
  end
  defp to_thrift(%Exception{name: name}, _file_group) do
    "#{name}"
  end
  defp to_thrift(%Union{name: name}, _file_group) do
    "#{name}"
  end
  defp to_thrift(%TypeRef{referenced_type: type}, file_group) do
    to_thrift(FileGroup.resolve(file_group, type), file_group)
  end
end
