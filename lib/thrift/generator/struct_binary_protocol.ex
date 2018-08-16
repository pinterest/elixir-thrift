# This module implements code generation of binary protocol deserialization.
#
# Consider a Thrift struct.
#
#   struct MyStruct {
#     1: i32 num;
#     2: list<i32> nums;
#     3: map<string, OtherStruct> structs;
#   }
#
# You could visual this as a tree of types like the following.
#
#   struct (MyStruct)
#   ├ i32
#   ├ list
#   │ └ i32
#   └ map
#     ├ key
#     │ └ string
#     └ value
#       └ struct (OtherStruct)
#
# We care about the edges of this graph. This module needs to know how to
# implement deserializers for each transition from one type to another.
#
# - struct -> i32
# - struct -> list
# - list -> i32
# - struct -> map
# - map key -> string
# - map value -> struct
#
# For the struct at the root of the graph, we generate a deserializer for each
# field. Other structs are leaf nodes in the graph. Rather than generating the
# deserialization logic inline, we make a call to the module we expect to have
# been generated for that struct.
defmodule Thrift.Generator.StructBinaryProtocol do
  @moduledoc false

  alias Thrift.AST.{
    Exception,
    Field,
    Struct,
    TEnum,
    TypeRef,
    Union
  }

  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup

  require Thrift.Protocol.Binary.Type, as: Type

  @doc """
  Generate a deserializer for a Thrift struct, union or exception.
  """
  def struct_deserializer(%{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    field_deserializers =
      fields
      |> Enum.map(&field_deserializer(&1.type, &1, :deserialize, file_group))
      |> Utils.merge_blocks()

    quote do
      def deserialize(binary) do
        deserialize(binary, %unquote(name){})
      end

      defp deserialize(<<0, rest::binary>>, %unquote(name){} = acc) do
        {acc, rest}
      end

      unquote_splicing(field_deserializers)

      # Skip over unknown fields. This can happen when we encounter serialized
      # data from a newer schema than our own.
      defp deserialize(<<field_type, _id::16-signed, rest::binary>>, acc) do
        rest
        |> Thrift.Protocol.Binary.skip_field(field_type)
        |> deserialize(acc)
      end

      defp deserialize(_, _), do: :error
    end
  end

  defp field_deserializer(:bool, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.bool()), unquote(field.id)::16-signed, 1, rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => true})
      end

      defp unquote(name)(
             <<unquote(Type.bool()), unquote(field.id)::16-signed, 0, rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => false})
      end
    end
  end

  defp field_deserializer(:byte, field, name, file_group) do
    field_deserializer(:i8, field, name, file_group)
  end

  defp field_deserializer(:i8, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.byte()), unquote(field.id)::16-signed, value::8-signed,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(:double, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.double()), unquote(field.id)::16-signed, 0::1, 2047::11, 0::52,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => :inf})
      end

      defp unquote(name)(
             <<unquote(Type.double()), unquote(field.id)::16-signed, 1::1, 2047::11, 0::52,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => :"-inf"})
      end

      defp unquote(name)(
             <<unquote(Type.double()), unquote(field.id)::16-signed, sign::1, 2047::11, frac::52,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{
          acc
          | unquote(field.name) => %Thrift.NaN{sign: sign, fraction: frac}
        })
      end

      defp unquote(name)(
             <<unquote(Type.double()), unquote(field.id)::16-signed, value::float-signed,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(:i16, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.i16()), unquote(field.id)::16-signed, value::16-signed,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(:i32, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.i32()), unquote(field.id)::16-signed, value::32-signed,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(%TEnum{}, field, name, file_group) do
    field_deserializer(:i32, field, name, file_group)
  end

  defp field_deserializer(:i64, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.i64()), unquote(field.id)::16-signed, value::64-signed,
               rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(:binary, field, name, file_group) do
    field_deserializer(:string, field, name, file_group)
  end

  defp field_deserializer(:string, field, name, _file_group) do
    quote do
      defp unquote(name)(
             <<unquote(Type.string()), unquote(field.id)::16-signed, string_size::32-signed,
               value::binary-size(string_size), rest::binary>>,
             acc
           ) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  defp field_deserializer(%Struct{} = struct, field, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(name)(
             <<unquote(Type.struct()), unquote(field.id)::16-signed, rest::binary>>,
             acc
           ) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(name)(rest, %{acc | unquote(field.name) => value})

          :error ->
            :error
        end
      end
    end
  end

  defp field_deserializer(%Union{} = union, field, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, union)

    quote do
      defp unquote(name)(
             <<unquote(Type.struct()), unquote(field.id)::16-signed, rest::binary>>,
             acc
           ) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(name)(rest, %{acc | unquote(field.name) => value})

          :error ->
            :error
        end
      end
    end
  end

  defp field_deserializer(%Exception{} = struct, field, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(name)(
             <<unquote(Type.struct()), unquote(field.id)::16-signed, rest::binary>>,
             acc
           ) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(name)(rest, %{acc | unquote(field.name) => value})

          :error ->
            :error
        end
      end
    end
  end

  defp field_deserializer({:map, {key_type, value_type}}, field, name, file_group) do
    key_name = :"#{name}__#{field.name}__key"
    value_name = :"#{name}__#{field.name}__value"

    quote do
      defp unquote(name)(
             <<unquote(Type.map()), unquote(field.id)::16-signed,
               unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)),
               map_size::32-signed, rest::binary>>,
             struct
           ) do
        unquote(key_name)(rest, [%{}, map_size, struct])
      end

      defp unquote(key_name)(<<rest::binary>>, [map, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => map})
      end

      unquote(map_key_deserializer(key_type, key_name, value_name, file_group))
      unquote(map_value_deserializer(value_type, key_name, value_name, file_group))
      defp unquote(key_name)(_, _), do: :error
      defp unquote(value_name)(_, _, _), do: :error
    end
  end

  defp field_deserializer({:set, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"

    quote do
      defp unquote(name)(
             <<unquote(Type.set()), unquote(field.id)::16-signed,
               unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             struct
           ) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end

      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => MapSet.new(list)})
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp field_deserializer({:list, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"

    quote do
      defp unquote(name)(
             <<unquote(Type.list()), unquote(field.id)::16-signed,
               unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             struct
           ) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end

      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => Enum.reverse(list)})
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp field_deserializer(%TypeRef{referenced_type: type}, field, name, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    field_deserializer(resolved, field, name, file_group)
  end

  defp map_key_deserializer(:bool, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<0, rest::binary>>, stack) do
        unquote(value_name)(rest, false, stack)
      end

      defp unquote(key_name)(<<1, rest::binary>>, stack) do
        unquote(value_name)(rest, true, stack)
      end
    end
  end

  defp map_key_deserializer(:byte, key_name, value_name, file_group) do
    map_key_deserializer(:i8, key_name, value_name, file_group)
  end

  defp map_key_deserializer(:double, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<0::1, 2047::11, 0::52, rest::binary>>, stack) do
        unquote(value_name)(rest, :inf, stack)
      end

      defp unquote(key_name)(<<1::1, 2047::11, 0::52, rest::binary>>, stack) do
        unquote(value_name)(rest, :"-inf", stack)
      end

      defp unquote(key_name)(<<sign::1, 2047::11, frac::52, rest::binary>>, stack) do
        unquote(value_name)(rest, %Thrift.NaN{sign: sign, fraction: frac}, stack)
      end

      defp unquote(key_name)(<<key::float-signed, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::8-signed, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::16-signed, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::32-signed, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_key_deserializer(:i32, key_name, value_name, file_group)
  end

  defp map_key_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::64-signed, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(:binary, key_name, value_name, file_group) do
    map_key_deserializer(:string, key_name, value_name, file_group)
  end

  defp map_key_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(
             <<string_size::32-signed, key::binary-size(string_size), rest::binary>>,
             stack
           ) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end

  defp map_key_deserializer(%Struct{} = struct, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(key_name)(<<rest::binary>>, stack) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {key, rest} ->
            unquote(value_name)(rest, key, stack)

          :error ->
            :error
        end
      end
    end
  end

  defp map_key_deserializer(%Union{} = union, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, union)

    quote do
      defp unquote(key_name)(<<rest::binary>>, stack) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {key, rest} ->
            unquote(value_name)(rest, key, stack)

          :error ->
            :error
        end
      end
    end
  end

  defp map_key_deserializer(%Exception{} = struct, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(key_name)(<<rest::binary>>, stack) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {key, rest} ->
            unquote(value_name)(rest, key, stack)

          :error ->
            :error
        end
      end
    end
  end

  defp map_key_deserializer({:map, {key_type, value_type}}, key_name, value_name, file_group) do
    child_key_name = :"#{key_name}__key"
    child_value_name = :"#{key_name}__value"

    quote do
      defp unquote(key_name)(
             <<unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)),
               remaining::32-signed, rest::binary>>,
             stack
           ) do
        unquote(child_key_name)(rest, [%{}, remaining | stack])
      end

      defp unquote(child_key_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, key, stack)
      end

      unquote(map_key_deserializer(key_type, child_key_name, child_value_name, file_group))
      unquote(map_value_deserializer(value_type, child_key_name, child_value_name, file_group))
      defp unquote(child_key_name)(_, _), do: :error
      defp unquote(child_value_name)(_, _, _), do: :error
    end
  end

  defp map_key_deserializer({:set, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{key_name}__element"

    quote do
      defp unquote(key_name)(
             <<unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             stack
           ) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, MapSet.new(key), stack)
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp map_key_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{key_name}__element"

    quote do
      defp unquote(key_name)(
             <<unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             stack
           ) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, Enum.reverse(key), stack)
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp map_key_deserializer(%TypeRef{referenced_type: type}, key_name, value_name, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    map_key_deserializer(resolved, key_name, value_name, file_group)
  end

  defp map_value_deserializer(:bool, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<0, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, false), remaining - 1 | stack])
      end

      defp unquote(value_name)(<<1, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, true), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(:byte, key_name, value_name, file_group) do
    map_value_deserializer(:i8, key_name, value_name, file_group)
  end

  defp map_value_deserializer(:double, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<0::1, 2047::11, 0::52, rest::binary>>, key, [
             map,
             remaining | stack
           ]) do
        unquote(key_name)(rest, [Map.put(map, key, :inf), remaining - 1 | stack])
      end

      defp unquote(value_name)(<<1::1, 2047::11, 0::52, rest::binary>>, key, [
             map,
             remaining | stack
           ]) do
        unquote(key_name)(rest, [Map.put(map, key, :"-inf"), remaining - 1 | stack])
      end

      defp unquote(value_name)(<<sign::1, 2047::11, frac::52, rest::binary>>, key, [
             map,
             remaining | stack
           ]) do
        unquote(key_name)(rest, [
          Map.put(map, key, %Thrift.NaN{sign: sign, fraction: frac}),
          remaining - 1 | stack
        ])
      end

      defp unquote(value_name)(<<value::float-signed, rest::binary>>, key, [
             map,
             remaining | stack
           ]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::8-signed, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::16-signed, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::32-signed, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_value_deserializer(:i32, key_name, value_name, file_group)
  end

  defp map_value_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::64-signed, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(:binary, key_name, value_name, file_group) do
    map_value_deserializer(:string, key_name, value_name, file_group)
  end

  defp map_value_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(
             <<string_size::32-signed, value::binary-size(string_size), rest::binary>>,
             key,
             [map, remaining | stack]
           ) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end

  defp map_value_deserializer(%Struct{} = struct, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(value_name)(<<rest::binary>>, key, [map, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp map_value_deserializer(%Union{} = union, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, union)

    quote do
      defp unquote(value_name)(<<rest::binary>>, key, [map, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp map_value_deserializer(%Exception{} = struct, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(value_name)(<<rest::binary>>, key, [map, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {value, rest} ->
            unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp map_value_deserializer({:map, {key_type, value_type}}, key_name, value_name, file_group) do
    child_key_name = :"#{value_name}__key"
    child_value_name = :"#{value_name}__value"

    quote do
      defp unquote(value_name)(
             <<unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)),
               remaining::32-signed, rest::binary>>,
             key,
             stack
           ) do
        unquote(child_key_name)(rest, [%{}, remaining, key | stack])
      end

      defp unquote(child_key_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end

      unquote(map_key_deserializer(key_type, child_key_name, child_value_name, file_group))
      unquote(map_value_deserializer(value_type, child_key_name, child_value_name, file_group))
      defp unquote(child_key_name)(_, _), do: :error
      defp unquote(child_value_name)(_, _, _), do: :error
    end
  end

  defp map_value_deserializer({:set, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{value_name}__element"

    quote do
      defp unquote(value_name)(
             <<unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             key,
             stack
           ) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, MapSet.new(value)), remaining - 1 | stack])
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp map_value_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{value_name}__element"

    quote do
      defp unquote(value_name)(
             <<unquote(type_id(element_type, file_group)), remaining::32-signed, rest::binary>>,
             key,
             stack
           ) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, Enum.reverse(value)), remaining - 1 | stack])
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp map_value_deserializer(%TypeRef{referenced_type: type}, key_name, value_name, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    map_value_deserializer(resolved, key_name, value_name, file_group)
  end

  defp list_deserializer(:bool, name, _file_group) do
    quote do
      defp unquote(name)(<<0, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[false | list], remaining - 1 | stack])
      end

      defp unquote(name)(<<1, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[true | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(:byte, name, file_group) do
    list_deserializer(:i8, name, file_group)
  end

  defp list_deserializer(:double, name, _file_group) do
    quote do
      defp unquote(name)(<<0::1, 2047::11, 0::52, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[:inf | list], remaining - 1 | stack])
      end

      defp unquote(name)(<<1::1, 2047::11, 0::52, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[:"-inf" | list], remaining - 1 | stack])
      end

      defp unquote(name)(<<sign::1, 2047::11, frac::52, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [
          [%Thrift.NaN{sign: sign, fraction: frac} | list],
          remaining - 1 | stack
        ])
      end

      defp unquote(name)(<<element::signed-float, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(:i8, name, _file_group) do
    quote do
      defp unquote(name)(<<element::8-signed, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(:i16, name, _file_group) do
    quote do
      defp unquote(name)(<<element::16-signed, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(:i32, name, _file_group) do
    quote do
      defp unquote(name)(<<element::32-signed, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(%TEnum{}, name, file_group) do
    list_deserializer(:i32, name, file_group)
  end

  defp list_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<element::64-signed, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(:binary, name, file_group) do
    list_deserializer(:string, name, file_group)
  end

  defp list_deserializer(:string, name, _file_group) do
    quote do
      defp unquote(name)(
             <<string_size::32-signed, element::binary-size(string_size), rest::binary>>,
             [list, remaining | stack]
           ) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  defp list_deserializer(%Struct{} = struct, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(name)(<<rest::binary>>, [list, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {element, rest} ->
            unquote(name)(rest, [[element | list], remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp list_deserializer(%Union{} = union, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, union)

    quote do
      defp unquote(name)(<<rest::binary>>, [list, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {element, rest} ->
            unquote(name)(rest, [[element | list], remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp list_deserializer(%Exception{} = struct, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      defp unquote(name)(<<rest::binary>>, [list, remaining | stack]) do
        case unquote(dest_module).BinaryProtocol.deserialize(rest) do
          {element, rest} ->
            unquote(name)(rest, [[element | list], remaining - 1 | stack])

          :error ->
            :error
        end
      end
    end
  end

  defp list_deserializer({:map, {key_type, value_type}}, name, file_group) do
    key_name = :"#{name}__key"
    value_name = :"#{name}__value"

    quote do
      defp unquote(name)(
             <<unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)),
               inner_remaining::32-signed, rest::binary>>,
             [list, remaining | stack]
           ) do
        unquote(key_name)(rest, [%{}, inner_remaining, list, remaining | stack])
      end

      defp unquote(key_name)(<<rest::binary>>, [map, 0, list, remaining | stack]) do
        unquote(name)(rest, [[map | list], remaining - 1 | stack])
      end

      unquote(map_key_deserializer(key_type, key_name, value_name, file_group))
      unquote(map_value_deserializer(value_type, key_name, value_name, file_group))
      defp unquote(key_name)(_, _), do: :error
      defp unquote(value_name)(_, _, _), do: :error
    end
  end

  defp list_deserializer({:set, element_type}, name, file_group) do
    sub_name = :"#{name}__element"

    quote do
      defp unquote(name)(
             <<unquote(type_id(element_type, file_group)), inner_remaining::32-signed,
               rest::binary>>,
             [list, remaining | stack]
           ) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[MapSet.new(inner_list) | list], remaining - 1 | stack])
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp list_deserializer({:list, element_type}, name, file_group) do
    sub_name = :"#{name}__element"

    quote do
      defp unquote(name)(
             <<unquote(type_id(element_type, file_group)), inner_remaining::32-signed,
               rest::binary>>,
             [list, remaining | stack]
           ) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end

      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[Enum.reverse(inner_list) | list], remaining - 1 | stack])
      end

      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end

  defp list_deserializer(%TypeRef{referenced_type: type}, name, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    list_deserializer(resolved, name, file_group)
  end

  @doc """
  Generate a serializer for a Thrift struct, union or exception.
  """
  def struct_serializer(%Union{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    all_fields_nil =
      Enum.map(fields, fn
        %Field{name: nil_name} -> {nil_name, nil}
      end)

    single_field_serializers =
      Enum.map(fields, fn
        %Field{name: match_name} = match_field ->
          field_matchers =
            Enum.map(all_fields_nil, fn
              {^match_name, nil} -> {match_name, Macro.var(match_name, nil)}
              {other_name, nil} -> {other_name, nil}
            end)

          field_serializers = [required_field_serializer(match_field, file_group)]

          quote do
            def serialize(%unquote(name){unquote_splicing(field_matchers)}) do
              unquote(Utils.optimize_iolist([field_serializers, <<0>>]))
            end
          end
      end)

    quote do
      def serialize(%unquote(name){unquote_splicing(all_fields_nil)}) do
        <<0>>
      end

      unquote_splicing(single_field_serializers)

      def serialize(%unquote(name){} = value) do
        set_fields =
          value
          |> Map.from_struct()
          |> Enum.flat_map(fn
            {_, nil} -> []
            {key, _} -> [key]
          end)

        raise %Thrift.Union.TooManyFieldsSetError{
          message: "Thrift union has more than one field set",
          set_fields: set_fields
        }
      end
    end
  end

  def struct_serializer(%{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    field_matchers =
      Enum.map(fields, fn %Field{name: name} ->
        {name, Macro.var(name, nil)}
      end)

    field_serializers = Enum.map(fields, &field_serializer(&1, name, file_group))

    quote do
      def serialize(%unquote(name){unquote_splicing(field_matchers)}) do
        unquote(Utils.optimize_iolist([field_serializers, <<0>>]))
      end
    end
  end

  defp field_serializer(
         %Field{name: name, type: :bool, id: id, required: true},
         struct_name,
         _file_group
       ) do
    quote do
      case unquote(Macro.var(name, nil)) do
        false ->
          <<unquote(Type.bool()), unquote(id)::16-signed, 0>>

        true ->
          <<unquote(Type.bool()), unquote(id)::16-signed, 1>>

        _ ->
          raise Thrift.InvalidValueError,
                unquote(
                  "Required boolean field #{inspect(name)} on #{inspect(struct_name)} must be true or false"
                )
      end
    end
  end

  defp field_serializer(%Field{name: name, type: :bool, id: id}, struct_name, _file_group) do
    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          <<>>

        false ->
          <<unquote(Type.bool()), unquote(id)::16-signed, 0>>

        true ->
          <<unquote(Type.bool()), unquote(id)::16-signed, 1>>

        _ ->
          raise Thrift.InvalidValueError,
                unquote(
                  "Optional boolean field #{inspect(name)} on #{inspect(struct_name)} must be true, false, or nil"
                )
      end
    end
  end

  defp field_serializer(%Field{name: name, required: true} = field, struct_name, file_group) do
    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          raise Thrift.InvalidValueError,
                unquote(
                  "Required field #{inspect(name)} on #{inspect(struct_name)} must not be nil"
                )

        _ ->
          unquote(required_field_serializer(field, file_group))
      end
    end
  end

  defp field_serializer(%Field{name: name} = field, _struct_name, file_group) do
    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          <<>>

        _ ->
          unquote(required_field_serializer(field, file_group))
      end
    end
  end

  defp required_field_serializer(%Field{name: name, type: type, id: id}, file_group) do
    var = Macro.var(name, nil)

    Utils.optimize_iolist([
      quote do
        <<unquote(type_id(type, file_group)), unquote(id)::16-signed>>
      end,
      value_serializer(type, var, file_group)
    ])
  end

  defp value_serializer(:bool, var, _file_group) do
    quote do
      case unquote(var) do
        nil -> <<0>>
        false -> <<0>>
        _ -> <<1>>
      end
    end
  end

  defp value_serializer(:byte, var, _file_group), do: quote(do: <<unquote(var)::8-signed>>)
  defp value_serializer(:i8, var, _file_group), do: quote(do: <<unquote(var)::8-signed>>)

  defp value_serializer(:double, var, _file_group) do
    quote do
      case unquote(var) do
        :inf -> <<0::1, 2047::11, 0::52>>
        :"-inf" -> <<1::1, 2047::11, 0::52>>
        %Thrift.NaN{sign: sign, fraction: frac} -> <<sign::1, 2047::11, frac::52>>
        _ -> <<unquote(var)::float-signed>>
      end
    end
  end

  defp value_serializer(:i16, var, _file_group), do: quote(do: <<unquote(var)::16-signed>>)
  defp value_serializer(:i32, var, _file_group), do: quote(do: <<unquote(var)::32-signed>>)
  defp value_serializer(%TEnum{}, var, _file_group), do: quote(do: <<unquote(var)::32-signed>>)
  defp value_serializer(:i64, var, _file_group), do: quote(do: <<unquote(var)::64-signed>>)

  defp value_serializer(:binary, var, _file_group),
    do: quote(do: [<<byte_size(unquote(var))::32-signed>>, unquote(var)])

  defp value_serializer(:string, var, _file_group),
    do: quote(do: [<<byte_size(unquote(var))::32-signed>>, unquote(var)])

  defp value_serializer({:map, {key_type, val_type}}, var, file_group) do
    quote do
      [
        <<unquote(type_id(key_type, file_group)), unquote(type_id(val_type, file_group)),
          Enum.count(unquote(var))::32-signed>>,
        for {unquote(Macro.var(:k, nil)), unquote(Macro.var(:v, nil))} <- unquote(var) do
          unquote(
            Utils.optimize_iolist([
              value_serializer(key_type, Macro.var(:k, nil), file_group),
              value_serializer(val_type, Macro.var(:v, nil), file_group)
            ])
          )
        end
      ]
    end
  end

  defp value_serializer({:set, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), Enum.count(unquote(var))::32-signed>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(Utils.optimize_iolist(value_serializer(type, Macro.var(:e, nil), file_group)))
        end
      ]
    end
  end

  defp value_serializer({:list, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), length(unquote(var))::32-signed>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(Utils.optimize_iolist(value_serializer(type, Macro.var(:e, nil), file_group)))
        end
      ]
    end
  end

  defp value_serializer(%Struct{} = struct, var, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      unquote(dest_module).serialize(unquote(var))
    end
  end

  defp value_serializer(%Union{} = union, var, file_group) do
    dest_module = FileGroup.dest_module(file_group, union)

    quote do
      unquote(dest_module).serialize(unquote(var))
    end
  end

  defp value_serializer(%Exception{} = struct, var, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      unquote(dest_module).serialize(unquote(var))
    end
  end

  defp value_serializer(%TypeRef{referenced_type: type}, var, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    value_serializer(resolved, var, file_group)
  end

  defp type_id(%TypeRef{referenced_type: type}, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    type_id(resolved, file_group)
  end

  defp type_id(%TEnum{}, _), do: Type.i32()
  defp type_id(%Struct{}, _), do: Type.struct()
  defp type_id(%Exception{}, _), do: Type.struct()
  defp type_id(%Union{}, _), do: Type.struct()
  defp type_id(type, _), do: Type.of(type)
end
