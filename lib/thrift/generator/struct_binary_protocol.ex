defmodule Thrift.Generator.StructBinaryProtocol do
  @moduledoc """
  This module implements code generation of binary protocol deserialization.

  Consider a Thrift struct.

    struct MyStruct {
      1: i32 num;
      2: list<i32> nums;
      3: map<string, OtherStruct> structs;
    }

  You could visual this as a tree of types like the following.

    struct (MyStruct)
    ├ i32
    ├ list
    │ └ i32
    └ map
      ├ key
      │ └ string
      └ value
        └ struct (OtherStruct)

  We care about the edges of this graph. This module needs to know how to
  implement deserializers for each transition from one type to another.

  - struct -> i32
  - struct -> list
  - list -> i32
  - struct -> map
  - map key -> string
  - map value -> struct

  For the struct at the root of the graph, we generate a deserializer for each
  field. Other structs are leaf nodes in the graph. Rather than generating the
  deserialization logic inline, we make a call to the module we expect to have
  been generated for that struct.
  """

  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    Exception,
    Field,
    Struct,
    StructRef,
    TEnum,
    Union,
  }

  @bool 2
  @byte 3
  @double 4
  @i16 6
  @i32 8
  @i64 10
  @string 11
  @struct 12
  @union 12
  @map 13
  @set 14
  @list 15

  @doc """
  Generate a deserializer for a Thrift struct, union or exception.
  """
  def struct_deserializer(%{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    field_deserializers = fields
    |> Enum.map(&field_deserializer(&1.type, &1, :deserialize, file_group))
    |> Utils.merge_blocks

    quote do
      def deserialize(binary) do
        deserialize(binary, %unquote(name){})
      end
      defp deserialize(<<0, rest::binary>>, %unquote(name){} = acc) do
        {acc, rest}
      end

      unquote_splicing(field_deserializers)

      defp deserialize(<<field_type, _id::16-signed, rest::binary>>, acc) do
        # this is for unknown fields. This can happen if you have an
        # outdated schema
        rest
        |> skip_field(field_type)
        |> deserialize(acc)
      end

      unquote(field_skippers)

      defp deserialize(_, _), do: :error
    end
  end

  defp field_skippers do
    quote do
      defp skip_field(<<_value, rest::binary>>, unquote(@bool)), do: rest
      defp skip_field(<<_value, rest::binary>>, unquote(@byte)), do: rest
      defp skip_field(<<_::signed-float, rest::binary>>, unquote(@double)), do: rest
      defp skip_field(<<_::size(16), rest::binary>>, unquote(@i16)), do: rest
      defp skip_field(<<_::size(32), rest::binary>>, unquote(@i32)), do: rest
      defp skip_field(<<_::size(64), rest::binary>>, unquote(@i64)), do: rest
      defp skip_field(<<string_size::32-signed, _::binary-size(string_size), rest::binary>>, unquote(@string)) do
        rest
      end

      defp skip_field(<<rest::binary>>, unquote(@struct)) do
        skip_struct(rest)
      end

      # maps
      defp skip_field(<<key_type, val_type, length::size(32), rest::binary>>, unquote(@map)) do
        skip_map_entry(rest, key_type, val_type, length)
      end

      # sets
      defp skip_field(<<elem_type, length::size(32), rest::binary>>, unquote(@set)) do
        skip_list_element(rest, elem_type, length)
      end

      # lists
      defp skip_field(<<elem_type, length::size(32), rest::binary>>, unquote(@list)) do
        skip_list_element(rest, elem_type, length)
      end

      defp skip_field(_, _) do
        :error
      end

      defp skip_list_element(<<rest::binary>>, _, 0) do
        rest
      end
      defp skip_list_element(<<rest::binary>>, elem_type, remaining) do
        rest
        |> skip_field(elem_type)
        |> skip_list_element(elem_type, remaining - 1)
      end
      defp skip_list_element(:error, _, _) do
        :error
      end

      defp skip_map_entry(<<rest::binary>>, _, _, 0) do
        rest
      end
      defp skip_map_entry(<<rest::binary>>, key_type, val_type, remaining) do
        rest
        |> skip_field(key_type)
        |> skip_field(val_type)
        |> skip_map_entry(key_type, val_type, remaining - 1)
      end
      defp skip_map_entry(:error, _, _, _) do
        :error
      end

      defp skip_struct(<<type, _id::16-signed, rest::binary>>) do
        case skip_field(rest, type) do
          <<0::size(8), rest::binary>> ->
            # we have a struct stop
            rest

          <<remaining::binary>> ->
            skip_struct(remaining)

          :error ->
            :error
        end
      end
      defp skip_struct(_) do
        :error
      end
    end
  end

  defp field_deserializer(:bool, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@bool), unquote(field.id)::size(16), 1, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => true})
      end
      defp unquote(name)(<<unquote(@bool), unquote(field.id)::size(16), 0, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => false})
      end
    end
  end
  defp field_deserializer(:byte, field, name, file_group) do
    field_deserializer(:i8, field, name, file_group)
  end
  defp field_deserializer(:i8, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@byte), unquote(field.id)::size(16), value, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(:double, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@double), unquote(field.id)::size(16), value::signed-float, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(:i16, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@i16), unquote(field.id)::size(16), value::size(16), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(:i32, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@i32), unquote(field.id)::size(16), value::size(32), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(%TEnum{}, field, name, file_group) do
    field_deserializer(:i32, field, name, file_group)
  end
  defp field_deserializer(:i64, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@i64), unquote(field.id)::size(16), value::size(64), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(:binary, field, name, file_group) do
    field_deserializer(:string, field, name, file_group)
  end
  defp field_deserializer(:string, field, name, _file_group) do
    quote do
      defp unquote(name)(<<unquote(@string),
                         unquote(field.id)::16-signed,
                         string_size::32-signed,
                         value::binary-size(string_size),
                         rest::binary>>, acc) do

        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  defp field_deserializer(%Struct{} = struct, field, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      defp unquote(name)(<<unquote(@struct), unquote(field.id)::16-signed, rest::binary>>, acc) do
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
      defp unquote(name)(<<unquote(@union), unquote(field.id)::16-signed, rest::binary>>, acc) do
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
      defp unquote(name)(<<unquote(@struct), unquote(field.id)::16-signed, rest::binary>>, acc) do
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
      defp unquote(name)(<<unquote(@map),
                           unquote(field.id)::size(16),
                           unquote(type_id(key_type, file_group)),
                           unquote(type_id(value_type, file_group)),
                           map_size::size(32),
                           rest::binary>>, struct) do
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
      defp unquote(name)(<<unquote(@set),
                           unquote(field.id)::size(16),
                           unquote(type_id(element_type, file_group)),
                           remaining::size(32),
                           rest::binary>>, struct) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end
      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => MapSet.new(Enum.reverse(list))})
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp field_deserializer({:list, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"
    quote do
      defp unquote(name)(<<unquote(@list),
                           unquote(field.id)::size(16),
                           unquote(type_id(element_type, file_group)),
                           remaining::size(32),
                           rest::binary>>, struct) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end
      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => Enum.reverse(list)})
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp field_deserializer(%StructRef{referenced_type: type}, field, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> field_deserializer(field, name, file_group)
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
      defp unquote(key_name)(<<key::signed-float, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  defp map_key_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  defp map_key_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(16), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  defp map_key_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(32), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  defp map_key_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_key_deserializer(:i32, key_name, value_name, file_group)
  end
  defp map_key_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(64), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  defp map_key_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<string_size::32-signed, key::binary-size(string_size), rest::binary>>, stack) do
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
      defp unquote(key_name)(<<unquote(type_id(key_type, file_group)),
                               unquote(type_id(value_type, file_group)),
                               remaining::size(32),
                               rest::binary>>, stack) do
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
      defp unquote(key_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, stack) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, MapSet.new(Enum.reverse(key)), stack)
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp map_key_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{key_name}__element"
    quote do
      defp unquote(key_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, stack) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, Enum.reverse(key), stack)
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp map_key_deserializer(%StructRef{referenced_type: type}, key_name, value_name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_key_deserializer(key_name, value_name, file_group)
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
      defp unquote(value_name)(<<value::signed-float, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  defp map_value_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  defp map_value_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(16), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  defp map_value_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(32), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  defp map_value_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_value_deserializer(:i32, key_name, value_name, file_group)
  end
  defp map_value_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(64), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  defp map_value_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<string_size::32-signed,
                               value::binary-size(string_size),
                               rest::binary>>, key, [map, remaining | stack]) do
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
      defp unquote(value_name)(<<unquote(type_id(key_type, file_group)),
                                 unquote(type_id(value_type, file_group)),
                                 remaining::size(32),
                                 rest::binary>>, key, stack) do
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
      defp unquote(value_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, key, stack) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, MapSet.new(Enum.reverse(value))), remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp map_value_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{value_name}__element"
    quote do
      defp unquote(value_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, key, stack) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, Enum.reverse(value)), remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp map_value_deserializer(%StructRef{referenced_type: type}, key_name, value_name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_value_deserializer(key_name, value_name, file_group)
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
      defp unquote(name)(<<element::signed-float, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  defp list_deserializer(:i8, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(8), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  defp list_deserializer(:i16, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(16), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  defp list_deserializer(:i32, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(32), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  defp list_deserializer(%TEnum{}, name, file_group) do
    list_deserializer(:i32, name, file_group)
  end
  defp list_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(64), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  defp list_deserializer(:string, name, _file_group) do
    quote do
      defp unquote(name)(<<string_size::32-signed, element::binary-size(string_size), rest::binary>>, [list, remaining | stack]) do
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
      defp unquote(name)(<<unquote(type_id(key_type, file_group)),
                           unquote(type_id(value_type, file_group)),
                           inner_remaining::size(32),
                           rest::binary>>,
                         [list, remaining | stack]) do
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
      defp unquote(name)(<<unquote(type_id(element_type, file_group)),
                           inner_remaining::size(32),
                           rest::binary>>, [list, remaining | stack]) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[MapSet.new(Enum.reverse(inner_list)) | list], remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp list_deserializer({:list, element_type}, name, file_group) do
    sub_name = :"#{name}__element"
    quote do
      defp unquote(name)(<<unquote(type_id(element_type, file_group)),
                           inner_remaining::size(32),
                           rest::binary>>, [list, remaining | stack]) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[Enum.reverse(inner_list) | list], remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
      defp unquote(sub_name)(_, _), do: :error
    end
  end
  defp list_deserializer(%StructRef{referenced_type: type}, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> list_deserializer(name, file_group)
  end

  @doc """
  Generate a serializer for a Thrift struct, union or exception.
  """
  def struct_serializer(%Union{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    all_fields_nil = Enum.map(fields, fn
      %Field{name: nil_name} -> {nil_name, nil}
    end)

    single_field_serializers = Enum.map(fields, fn
      %Field{name: match_name} = match_field ->
        field_matchers = Enum.map(all_fields_nil, fn
          {^match_name, nil} -> {match_name, Macro.var(match_name, nil)}
          {other_name, nil}  -> {other_name, nil}
        end)

        field_serializers = [required_field_serializer(match_field, file_group)]

        quote do
          def serialize(%unquote(name){unquote_splicing(field_matchers)}) do
            unquote([field_serializers, <<0>>] |> Utils.optimize_iolist)
          end
        end
    end)

    quote do
      def serialize(%unquote(name){unquote_splicing(all_fields_nil)}) do
        <<0>>
      end
      unquote_splicing(single_field_serializers)
      def serialize(%unquote(name){} = value) do
        set_fields = value
        |> Map.from_struct
        |> Enum.flat_map(fn
          {_, nil} -> []
          {key, _} -> [key]
        end)

        raise %Thrift.Union.TooManyFieldsSetException{
          message: "Thrift union has more than one field set",
          set_fields: set_fields
        }
      end
    end
  end
  def struct_serializer(%{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    field_matchers = Enum.map(fields, fn %Field{name: name} ->
      {name, Macro.var(name, nil)}
    end)

    field_serializers = fields
    |> Enum.map(&field_serializer(&1, file_group))

    quote do
      def serialize(%unquote(name){unquote_splicing(field_matchers)}) do
        unquote([field_serializers, <<0>>] |> Utils.optimize_iolist)
      end
    end
  end

  defp field_serializer(%Field{name: name} = field, file_group) do
    var = Macro.var(name, nil)
    quote do
      case unquote(var) do
        nil ->
          <<>>
        _ ->
          unquote(required_field_serializer(field, file_group))
      end
    end
  end

  defp required_field_serializer(%Field{name: name, type: type, id: id}, file_group) do
    var = Macro.var(name, nil)
    [
      quote do <<unquote(type_id(type, file_group)), unquote(id) :: size(16)>> end,
      value_serializer(type, var, file_group)
    ] |> Utils.optimize_iolist
  end

  defp value_serializer(:bool, var, _file_group) do
    quote do
      case unquote(var) do
        nil   -> <<0>>
        false -> <<0>>
        _     -> <<1>>
      end
    end
  end
  defp value_serializer(:byte,    var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  defp value_serializer(:i8,      var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  defp value_serializer(:double,  var, _file_group), do: quote do: <<unquote(var) :: signed-float>>
  defp value_serializer(:i16,     var, _file_group), do: quote do: <<unquote(var) :: 16-signed>>
  defp value_serializer(:i32,     var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  defp value_serializer(%TEnum{}, var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  defp value_serializer(:i64,     var, _file_group), do: quote do: <<unquote(var) :: 64-signed>>
  defp value_serializer(:binary,  var, _file_group), do: quote do: [<<byte_size(unquote(var)) :: size(32)>>, unquote(var)]
  defp value_serializer(:string,  var, _file_group), do: quote do: [<<byte_size(unquote(var)) :: size(32)>>, unquote(var)]
  defp value_serializer({:map, {key_type, val_type}}, var, file_group) do
    quote do
      [
        <<unquote(type_id(key_type, file_group)),
          unquote(type_id(val_type, file_group)),
          map_size(unquote(var)) :: size(32)>>,
        for {unquote(Macro.var(:k, nil)), unquote(Macro.var(:v, nil))} <- unquote(var) do
          unquote([
            value_serializer(key_type, Macro.var(:k, nil), file_group),
            value_serializer(val_type, Macro.var(:v, nil), file_group),
          ] |> Utils.optimize_iolist)
        end
      ]
    end
  end
  defp value_serializer({:set, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), MapSet.size(unquote(var)) :: size(32)>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(value_serializer(type, Macro.var(:e, nil), file_group) |> Utils.optimize_iolist)
        end,
      ]
    end
  end
  defp value_serializer({:list, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), length(unquote(var)) :: size(32)>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(value_serializer(type, Macro.var(:e, nil), file_group) |> Utils.optimize_iolist)
        end,
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
  defp value_serializer(%StructRef{referenced_type: type}, var, file_group) do
    FileGroup.resolve(file_group, type)
    |> value_serializer(var, file_group)
  end


  defp type_id(:bool, _file_group), do: 2
  defp type_id(:byte, _file_group), do: 3
  defp type_id(:i8, _file_group), do: 3
  defp type_id(:double, _file_group), do: 4
  defp type_id(:i16, _file_group), do: 6
  defp type_id(:i32, _file_group), do: 8
  defp type_id(%TEnum{}, _file_group), do: 8
  defp type_id(:i64, _file_group), do: 10
  defp type_id(:string, _file_group), do: 11
  defp type_id(:binary, _file_group), do: 11
  defp type_id(%Struct{}, _file_group), do: 12
  defp type_id(%Exception{}, _file_group), do: 12
  defp type_id(%Union{}, _file_group), do: 12
  defp type_id({:map, _}, _file_group), do: 13
  defp type_id({:set, _}, _file_group), do: 14
  defp type_id({:list, _}, _file_group), do: 15
  defp type_id(%StructRef{referenced_type: type}, file_group) do
    FileGroup.resolve(file_group, type)
    |> type_id(file_group)
  end
end
