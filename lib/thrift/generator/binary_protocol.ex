defmodule Thrift.Generator.Models.BinaryProtocol do
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
    # Exception,
    Field,
    Struct,
    StructRef,
    TEnum,
  }

  @doc """
  Generate a deserializer for a Thrift struct or exception.

  At the moment it also generates an experimental serializer that may be faster.
  """
  def struct_deserializer(%{fields: fields}, name, file_group) do
    field_matchers = Enum.map(fields, fn %Field{name: name} ->
      {name, Macro.var(name, nil)}
    end)

    struct_matcher = {:%, [], [name, {:%{}, [], field_matchers}]}

    field_serializers = Enum.map(fields, fn %Field{name: name, type: type, id: id} ->
      var = Macro.var(name, nil)
      quote do
        case unquote(var) do
          nil ->
            <<>>
          _ ->
            unquote([
              quote do <<unquote(type_id(type, file_group)), unquote(id) :: size(16)>> end,
              value_serializer(type, var, file_group)
            ] |> Utils.merge_binaries |> simplify_iolist)
        end
      end
    end)

    field_deserializers = fields
    |> Enum.map(&field_deserializer(&1.type, &1, :deserialize, file_group))
    |> Utils.merge_blocks

    quote do
      def serialize(unquote(struct_matcher)) do
        unquote([field_serializers, <<0>>] |> Utils.merge_binaries)
      end
      def bool_to_int(false), do: 0
      def bool_to_int(nil), do: 0
      def bool_to_int(_), do: 1

      def deserialize(binary) do
        deserialize(binary, %unquote(name){})
      end
      defp deserialize(<<0, rest::binary>>, acc=%unquote(name){}) do
        {acc, rest}
      end
      unquote_splicing(field_deserializers)
    end
  end

  def field_deserializer(:bool, field, name, _file_group) do
    quote do
      defp unquote(name)(<<2, unquote(field.id)::size(16), 1, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => true})
      end
      defp unquote(name)(<<2, unquote(field.id)::size(16), 0, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => false})
      end
    end
  end
  def field_deserializer(:byte, field, name, file_group) do
    field_deserializer(:i8, field, name, file_group)
  end
  def field_deserializer(:i8, field, name, _file_group) do
    quote do
      defp unquote(name)(<<3, unquote(field.id)::size(16), value, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(:double, field, name, _file_group) do
    quote do
      defp unquote(name)(<<4, unquote(field.id)::size(16), value::signed-float, rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(:i16, field, name, _file_group) do
    quote do
      defp unquote(name)(<<6, unquote(field.id)::size(16), value::size(16), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(:i32, field, name, _file_group) do
    quote do
      defp unquote(name)(<<8, unquote(field.id)::size(16), value::size(32), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(%TEnum{}, field, name, file_group) do
    field_deserializer(:i32, field, name, file_group)
  end
  def field_deserializer(:i64, field, name, _file_group) do
    quote do
      defp unquote(name)(<<10, unquote(field.id)::size(16), value::size(64), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(:binary, field, name, file_group) do
    field_deserializer(:string, field, name, file_group)
  end
  def field_deserializer(:string, field, name, _file_group) do
    quote do
      defp unquote(name)(<<11, unquote(field.id)::16-signed, string_size::32-signed, rest::binary>>, acc) do
        <<value::binary-size(string_size), rest::binary>> = rest
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer(struct=%Struct{}, field, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      defp unquote(name)(<<12, unquote(field.id)::16-signed, rest::binary>>, acc) do
        {value, rest} = unquote(dest_module).BinaryProtocol.deserialize(rest)
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end
  def field_deserializer({:map, {key_type, value_type}}, field, name, file_group) do
    key_name = :"#{name}__#{field.name}__key"
    value_name = :"#{name}__#{field.name}__value"
    quote do
      defp unquote(name)(<<13,
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
    end
  end
  def field_deserializer({:set, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"
    quote do
      defp unquote(name)(<<14, unquote(field.id)::size(16), unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, struct) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end
      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => MapSet.new(Enum.reverse(list))})
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def field_deserializer({:list, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"
    quote do
      defp unquote(name)(<<15, unquote(field.id)::size(16), unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, struct) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end
      defp unquote(sub_name)(<<rest::binary>>, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => Enum.reverse(list)})
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def field_deserializer(%StructRef{referenced_type: type}, field, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> field_deserializer(field, name, file_group)
  end


  def map_key_deserializer(:bool, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<0, rest::binary>>, stack) do
        unquote(value_name)(rest, false, stack)
      end
      defp unquote(key_name)(<<1, rest::binary>>, stack) do
        unquote(value_name)(rest, true, stack)
      end
    end
  end
  def map_key_deserializer(:byte, key_name, value_name, file_group) do
    map_key_deserializer(:i8, key_name, value_name, file_group)
  end
  def map_key_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key, rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(16), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(32), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_key_deserializer(:i32, key_name, value_name, file_group)
  end
  def map_key_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<key::size(64), rest::binary>>, stack) do
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(key_name)(<<string_size::32-signed, rest::binary>>, stack) do
        <<key::binary-size(string_size), rest::binary>> = rest
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer(struct=%Struct{}, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      defp unquote(key_name)(<<rest::binary>>, stack) do
        {key, rest} = unquote(dest_module).BinaryProtocol.deserialize(rest)
        unquote(value_name)(rest, key, stack)
      end
    end
  end
  def map_key_deserializer({:map, {key_type, value_type}}, key_name, value_name, file_group) do
    child_key_name = :"#{key_name}__key"
    child_value_name = :"#{key_name}__value"
    quote do
      defp unquote(key_name)(<<unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)), remaining::size(32), rest::binary>>, stack) do
        unquote(child_key_name)(rest, [%{}, remaining | stack])
      end
      defp unquote(child_key_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, key, stack)
      end
      unquote(map_key_deserializer(key_type, child_key_name, child_value_name, file_group))
      unquote(map_value_deserializer(value_type, child_key_name, child_value_name, file_group))
    end
  end
  def map_key_deserializer({:set, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{key_name}__element"
    quote do
      defp unquote(key_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, stack) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, MapSet.new(Enum.reverse(key)), stack)
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def map_key_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{key_name}__element"
    quote do
      defp unquote(key_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, stack) do
        unquote(sub_name)(rest, [[], remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [key, 0 | stack]) do
        unquote(value_name)(rest, Enum.reverse(key), stack)
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def map_key_deserializer(%StructRef{referenced_type: type}, key_name, value_name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_key_deserializer(key_name, value_name, file_group)
  end


  def map_value_deserializer(:bool, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<0, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, false), remaining - 1 | stack])
      end
      defp unquote(value_name)(<<1, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, true), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(:byte, key_name, value_name, file_group) do
    map_value_deserializer(:i8, key_name, value_name, file_group)
  end
  def map_value_deserializer(:i8, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value, rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(:i16, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(16), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(:i32, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(32), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(%TEnum{}, key_name, value_name, file_group) do
    map_value_deserializer(:i32, key_name, value_name, file_group)
  end
  def map_value_deserializer(:i64, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<value::size(64), rest::binary>>, key, [map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(:string, key_name, value_name, _file_group) do
    quote do
      defp unquote(value_name)(<<string_size::32-signed, rest::binary>>, key, [map, remaining | stack]) do
        <<value::binary-size(string_size), rest::binary>> = rest
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer(struct=%Struct{}, key_name, value_name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      defp unquote(value_name)(<<rest::binary>>, key, [map, remaining | stack]) do
        {value, rest} = unquote(dest_module).BinaryProtocol.deserialize(rest)
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
    end
  end
  def map_value_deserializer({:map, {key_type, value_type}}, key_name, value_name, file_group) do
    child_key_name = :"#{value_name}__key"
    child_value_name = :"#{value_name}__value"
    quote do
      defp unquote(value_name)(<<unquote(type_id(key_type, file_group)), unquote(type_id(value_type, file_group)), remaining::size(32), rest::binary>>, key, stack) do
        unquote(child_key_name)(rest, [%{}, remaining, key | stack])
      end
      defp unquote(child_key_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, value), remaining - 1 | stack])
      end
      unquote(map_key_deserializer(key_type, child_key_name, child_value_name, file_group))
      unquote(map_value_deserializer(value_type, child_key_name, child_value_name, file_group))
    end
  end
  def map_value_deserializer({:set, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{value_name}__element"
    quote do
      defp unquote(value_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, key, stack) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, MapSet.new(Enum.reverse(value))), remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def map_value_deserializer({:list, element_type}, key_name, value_name, file_group) do
    sub_name = :"#{value_name}__element"
    quote do
      defp unquote(value_name)(<<unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, key, stack) do
        unquote(sub_name)(rest, [[], remaining, key | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [value, 0, key, map, remaining | stack]) do
        unquote(key_name)(rest, [Map.put(map, key, Enum.reverse(value)), remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def map_value_deserializer(%StructRef{referenced_type: type}, key_name, value_name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_value_deserializer(key_name, value_name, file_group)
  end


  def list_deserializer(:bool, name, _file_group) do
    quote do
      defp unquote(name)(<<0, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[false | list], remaining - 1 | stack])
      end
      defp unquote(name)(<<1, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[true | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(:byte, name, file_group) do
    list_deserializer(:i8, name, file_group)
  end
  def list_deserializer(:double, name, _file_group) do
    quote do
      defp unquote(name)(<<element::signed-float, rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(:i8, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(8), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(:i16, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(16), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(:i32, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(32), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(%TEnum{}, name, file_group) do
    list_deserializer(:i32, name, file_group)
  end
  def list_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(64), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(:string, name, _file_group) do
    quote do
      defp unquote(name)(<<string_size::32-signed, rest::binary>>, [list, remaining | stack]) do
        <<element::binary-size(string_size), rest::binary>> = rest
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer(struct=%Struct{}, name, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      defp unquote(name)(<<rest::binary>>, [list, remaining | stack]) do
        {element, rest} = unquote(dest_module).BinaryProtocol.deserialize(rest)
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end
  def list_deserializer({:map, {key_type, value_type}}, name, file_group) do
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
    end
  end
  def list_deserializer({:set, element_type}, name, file_group) do
    sub_name = :"#{name}__element"
    quote do
      defp unquote(name)(<<unquote(type_id(element_type, file_group)), inner_remaining::size(32), rest::binary>>, [list, remaining | stack]) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[MapSet.new(Enum.reverse(inner_list)) | list], remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def list_deserializer({:list, element_type}, name, file_group) do
    sub_name = :"#{name}__element"
    quote do
      defp unquote(name)(<<unquote(type_id(element_type, file_group)), inner_remaining::size(32), rest::binary>>, [list, remaining | stack]) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end
      defp unquote(sub_name)(<<rest::binary>>, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[Enum.reverse(inner_list) | list], remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end
  def list_deserializer(%StructRef{referenced_type: type}, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> list_deserializer(name, file_group)
  end


  def value_serializer(:bool,    var, _file_group), do: quote do: <<bool_to_int(unquote(var))>>
  def value_serializer(:byte,    var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  def value_serializer(:i8,      var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  def value_serializer(:double,  var, _file_group), do: quote do: <<unquote(var) :: signed-float>>
  def value_serializer(:i16,     var, _file_group), do: quote do: <<unquote(var) :: 16-signed>>
  def value_serializer(:i32,     var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  def value_serializer(%TEnum{}, var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  def value_serializer(:i64,     var, _file_group), do: quote do: <<unquote(var) :: 64-signed>>
  def value_serializer(:binary,  var, _file_group), do: quote do: [<<byte_size(unquote(var)) :: size(32)>>, unquote(var)]
  def value_serializer(:string,  var, _file_group), do: quote do: [<<byte_size(unquote(var)) :: size(32)>>, unquote(var)]
  def value_serializer({:map, {key_type, val_type}}, var, file_group) do
    quote do
      [
        <<unquote(type_id(key_type, file_group)),
          unquote(type_id(val_type, file_group)),
          map_size(unquote(var)) :: size(32)>>,
        for {unquote(Macro.var(:k, nil)), unquote(Macro.var(:v, nil))} <- unquote(var) do
          unquote([
            value_serializer(key_type, Macro.var(:k, nil), file_group),
            value_serializer(val_type, Macro.var(:v, nil), file_group),
          ] |> Utils.merge_binaries |> simplify_iolist)
        end
      ]
    end
  end
  def value_serializer({:set, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), MapSet.size(unquote(var)) :: size(32)>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(value_serializer(type, Macro.var(:e, nil), file_group) |> Utils.merge_binaries |> simplify_iolist)
        end,
      ]
    end
  end
  def value_serializer({:list, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), length(unquote(var)) :: size(32)>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(value_serializer(type, Macro.var(:e, nil), file_group) |> Utils.merge_binaries |> simplify_iolist)
        end,
      ]
    end
  end
  def value_serializer(struct=%Struct{name: _name}, var, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)
    quote do
      unquote(dest_module).serialize(unquote(var))
    end
  end
  def value_serializer(%StructRef{referenced_type: type}, var, file_group) do
    FileGroup.resolve(file_group, type)
    |> value_serializer(var, file_group)
  end


  def type_id(:bool, _file_group), do: 2
  def type_id(:byte, _file_group), do: 3
  def type_id(:i8, _file_group), do: 3
  def type_id(:double, _file_group), do: 4
  def type_id(:i16, _file_group), do: 6
  def type_id(:i32, _file_group), do: 8
  def type_id(%TEnum{}, _file_group), do: 8
  def type_id(:i64, _file_group), do: 10
  def type_id(:string, _file_group), do: 11
  def type_id(:binary, _file_group), do: 11
  def type_id(%Struct{}, _file_group), do: 12
  def type_id({:map, _}, _file_group), do: 13
  def type_id({:set, _}, _file_group), do: 14
  def type_id({:list, _}, _file_group), do: 15
  def type_id(%StructRef{referenced_type: type}, file_group) do
    FileGroup.resolve(file_group, type)
    |> type_id(file_group)
  end


  def simplify_iolist([{:<<>>, _, _}=binary]) do
    binary
  end
  def simplify_iolist(other) do
    other
  end
end
