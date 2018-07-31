defmodule Thrift.Generator.Compact.StructDeserialize do
  @moduledoc """
  Generates deserialisation functions for the compact protocol.

  See `Thrift.Generator.StructCompactProtocol`
  """

  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup

  alias Thrift.AST.{
    Exception,
    Field,
    Struct,
    TypeRef,
    TEnum,
    Union
  }

  alias Thrift.Generator.Compact.{StructSerialize, Common}
  alias Thrift.Protocol.Compact.IntegerEncoding

  def struct_deserializer(%{fields: fields}, name, file_group) do
    fields
    |> reject_void_and_sort_fields()
    |> do_struct_deserialize(name, file_group)
  end

  defp do_struct_deserialize(fields, name, file_group) do
    quote do
      def deserialize(binary) do
        deserialize(binary, 0, %unquote(name){})
      end

      defp deserialize(<<0, rest::binary>>, _, %unquote(name){} = acc) do
        {acc, rest}
      end

      unquote_splicing(field_deserializers(fields, file_group))
      unquote_splicing(container_deserializers(fields, file_group))
      unquote(field_skipping())
    end
  end

  defp field_skipping() do
    quote do
      defp deserialize(<<0::4, _type::4, contains_field_id::binary>> = rest, _previous_id, struct) do
        case Thrift.Protocol.Compact.IntegerEncoding.decode_zigzag_varint(contains_field_id) do
          {field_id, _} ->
            skip_field(rest, field_id, struct)

          _ ->
            :error
        end
      end

      defp deserialize(
             <<delta::4-unsigned, _::4-unsigned, _::binary>> = rest,
             previous_id,
             struct
           ) do
        skip_field(rest, delta + previous_id, struct)
      end

      defp deserialize(_, _, _) do
        :error
      end

      defp skip_field(rest, field_id, struct) do
        case Thrift.Protocol.Compact.skip_field(rest) do
          :error ->
            :error

          rest ->
            deserialize(rest, field_id, struct)
        end
      end
    end
  end

  defp container_deserializers(fields, file_group) do
    types = for %Field{type: type} <- fields, do: type

    %{}
    |> do_container_deserializers(types, file_group)
    |> Map.values()
    |> Utils.merge_blocks()
  end

  defp do_container_deserializers(acc, [], _), do: acc

  defp do_container_deserializers(
         acc,
         [%TypeRef{referenced_type: referenced_type} | rest],
         file_group
       ) do
    type = FileGroup.resolve(file_group, referenced_type)
    do_container_deserializers(acc, [type | rest], file_group)
  end

  defp do_container_deserializers(acc, [{:map, {key_type, value_type}} = type | rest], file_group) do
    acc
    |> add_container_deserializer(type, file_group)
    |> do_container_deserializers([key_type], file_group)
    |> do_container_deserializers([value_type], file_group)
    |> do_container_deserializers(rest, file_group)
  end

  defp do_container_deserializers(acc, [{t, element_type} = type | rest], file_group)
       when t in [:list, :set] do
    acc
    |> do_container_deserializers([element_type], file_group)
    |> add_container_deserializer({type, file_group})
    |> do_container_deserializers(rest, file_group)
  end

  defp do_container_deserializers(acc, [_ | rest], file_group) do
    quote do
      unquote(do_container_deserializers(acc, rest, file_group))
    end
  end

  defp add_container_deserializer(acc, {:map, {key_type, value_type}} = type, file_group) do
    fun_name = container_deserializer_fun_name(type, file_group)
    key_type_id = Common.contained_type_id(key_type, file_group)
    value_type_id = Common.contained_type_id(value_type, file_group)

    Map.put_new_lazy(acc, fun_name, fn ->
      quote do
        defp unquote(fun_name)(<<0, rest::binary>>) do
          {%{}, rest}
        end

        defp unquote(fun_name)(<<rest::binary>>) do
          with {size, rest} <- Thrift.Protocol.Compact.IntegerEncoding.decode_varint(rest),
               <<unquote(key_type_id)::4, unquote(value_type_id)::4, rest::binary>> <- rest do
            unquote(fun_name)(rest, {%{}, size})
          else
            _ ->
              :error
          end
        end

        defp unquote(fun_name)(<<rest::binary>>, {acc, 0}) do
          {acc, rest}
        end

        defp unquote(fun_name)(<<rest::binary>>, {acc, remaining}) do
          with {key, rest} <-
                 unquote(deserialize_value(key_type, Macro.var(:rest, nil), file_group)),
               {value, rest} <-
                 unquote(deserialize_value(value_type, Macro.var(:rest, nil), file_group)) do
            unquote(fun_name)(rest, {Map.put(acc, key, value), remaining - 1})
          end
        end
      end
    end)
  end

  defp add_container_deserializer(acc, {{container_type, element_type}, file_group})
       when container_type in [:list, :set] do
    fun_name = list_or_set_deserializer_fun_name(element_type, file_group)
    element_type_id = Common.contained_type_id(element_type, file_group)

    Map.put_new_lazy(acc, fun_name, fn ->
      quote do
        defp unquote(fun_name)(
               <<15::4-unsigned, unquote(element_type_id)::4-unsigned, rest::binary>>
             ) do
          case Thrift.Protocol.Compact.IntegerEncoding.decode_varint(rest) do
            {size, rest} ->
              unquote(fun_name)(rest, {[], size})

            _ ->
              :error
          end
        end

        defp unquote(fun_name)(
               <<size::4-unsigned, unquote(element_type_id)::4-unsigned, rest::binary>>
             )
             when size != 15 do
          unquote(fun_name)(rest, {[], size})
        end

        defp unquote(fun_name)(_) do
          :error
        end

        defp unquote(fun_name)(binary, {items, 0}) do
          {items, binary}
        end

        unquote(list_or_set_deserializer(element_type, fun_name, file_group))

        defp unquote(fun_name)(_, _), do: :error
      end
    end)
  end

  defp field_deserializers(fields, file_group) do
    fields
    |> Enum.map(&field_deserializer(&1.type, &1, file_group))
    |> Utils.merge_blocks()
  end

  defp list_or_set_deserializer_fun_name(item_type, file_group) do
    container_deserializer_fun_name({:list_or_set, item_type}, file_group)
  end

  defp container_deserializer_fun_name(type, file_group) do
    String.to_atom("deserialize_container__#{do_deserialize_fun_name(type, file_group)}")
  end

  defp do_deserialize_fun_name({:map, {key_type, value_type}}, file_group) do
    "map_#{do_deserialize_fun_name(key_type, file_group)}_#{
      do_deserialize_fun_name(value_type, file_group)
    }"
  end

  defp do_deserialize_fun_name({type, element_type}, file_group) do
    "#{type}_#{do_deserialize_fun_name(element_type, file_group)}"
  end

  defp do_deserialize_fun_name(%TypeRef{referenced_type: type}, file_group) do
    file_group
    |> FileGroup.resolve(type)
    |> do_deserialize_fun_name(file_group)
  end

  defp do_deserialize_fun_name(%{name: name, __struct__: mod}, _file_group)
       when mod in [Struct, Exception, Union, TEnum] do
    name
    |> Atom.to_string()
    |> String.downcase()
    |> String.replace(".", "_")
  end

  defp do_deserialize_fun_name(type, _file_group), do: "#{type}"

  defp short_header_field_deserializer(type, %Field{id: id} = field, file_group) do
    quote do
      defp deserialize(
             <<delta::4-unsigned, unquote(Common.type_id(type, file_group))::4-unsigned,
               rest::binary>>,
             previous_id,
             struct
           )
           when delta > 0 and delta + previous_id == unquote(id) do
        unquote(body_deserializer(type, field, :rest, file_group))
      end
    end
  end

  defp long_header_field_deserializer(type, %Field{id: id} = field, file_group) do
    header_match =
      Utils.optimize_iolist([
        <<0::4, Common.type_id(type, file_group)::4-unsigned>>,
        IntegerEncoding.encode_zigzag_varint(id),
        quote do
          <<rest::binary>>
        end
      ])

    quote do
      defp deserialize(unquote(header_match), _previous_id, struct) do
        unquote(body_deserializer(type, field, :rest, file_group))
      end
    end
  end

  defp field_deserializer(:bool, field, file_group) do
    quote do
      unquote(field_deserializer({:bool, true}, field, file_group))
      unquote(field_deserializer({:bool, false}, field, file_group))
    end
  end

  defp field_deserializer(type, field, file_group) do
    quote do
      unquote(short_header_field_deserializer(type, field, file_group))
      unquote(long_header_field_deserializer(type, field, file_group))
    end
  end

  defp body_deserializer(type, field, binary_var, file_group) when is_atom(binary_var) do
    body_deserializer(type, field, Macro.var(binary_var, nil), file_group)
  end

  defp body_deserializer(
         {:list, element_type},
         %Field{name: name, id: id},
         binary_var,
         file_group
       ) do
    fun_name = list_or_set_deserializer_fun_name(element_type, file_group)

    quote do
      case unquote(fun_name)(unquote(binary_var)) do
        {val, rest} ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => Enum.reverse(val)})

        _ ->
          :error
      end
    end
  end

  defp body_deserializer({:set, element_type}, %Field{name: name, id: id}, binary_var, file_group) do
    fun_name = list_or_set_deserializer_fun_name(element_type, file_group)

    quote do
      case unquote(fun_name)(unquote(binary_var)) do
        {val, rest} ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => MapSet.new(val)})

        _ ->
          :error
      end
    end
  end

  defp body_deserializer(%TypeRef{referenced_type: type}, field, binary_var, file_group) do
    file_group
    |> FileGroup.resolve(type)
    |> body_deserializer(field, binary_var, file_group)
  end

  defp body_deserializer(
         {:map, {_key_type, _value_type}} = type,
         %Field{id: id, name: name},
         binary_var,
         file_group
       ) do
    fun_name = container_deserializer_fun_name(type, file_group)

    quote do
      case unquote(fun_name)(unquote(binary_var)) do
        {val, rest} ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => val})

        _ ->
          :error
      end
    end
  end

  defp body_deserializer({:bool, false}, %Field{id: id, name: name}, binary_var, _file_group) do
    quote do
      deserialize(unquote(binary_var), unquote(id), %{struct | unquote(name) => false})
    end
  end

  defp body_deserializer({:bool, true}, %Field{id: id, name: name}, binary_var, _file_group) do
    quote do
      deserialize(unquote(binary_var), unquote(id), %{struct | unquote(name) => true})
    end
  end

  defp body_deserializer(:double, %Field{id: id, name: name}, binary_var, _file_group) do
    quote do
      case unquote(binary_var) do
        <<val::64-float-little, rest::binary>> ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => val})

        _ ->
          :error
      end
    end
  end

  defp body_deserializer(:i8, %Field{id: id, name: name}, binary_var, _file_group) do
    quote do
      case unquote(binary_var) do
        <<val::8-signed, rest::binary>> ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => val})

        _ ->
          :error
      end
    end
  end

  defp body_deserializer(type, %Field{id: id, name: name}, binary_var, file_group) do
    quote do
      case unquote(deserialize_value(type, binary_var, file_group)) do
        {val, rest} ->
          deserialize(rest, unquote(id), %{struct | unquote(name) => val})

        _ ->
          :error
      end
    end
  end

  defp deserialize_value(type, binary_var, file_group) when is_atom(binary_var) do
    deserialize_value(type, Macro.var(binary_var, nil), file_group)
  end

  defp deserialize_value({type, element_type}, binary_var, file_group)
       when type in [:list, :set] do
    fun_name = list_or_set_deserializer_fun_name(element_type, file_group)

    accumulator_to_container =
      case type do
        :set ->
          quote do
            MapSet.new(acc)
          end

        :list ->
          quote do
            Enum.reverse(acc)
          end
      end

    quote do
      case unquote(fun_name)(unquote(binary_var)) do
        {acc, rest} ->
          {unquote(accumulator_to_container), rest}

        _ ->
          :error
      end
    end
  end

  defp deserialize_value(:bool, binary_var, _file_group) do
    quote do
      case unquote(binary_var) do
        <<unquote(Common.contained_false()), rest::binary>> ->
          {false, rest}

        <<unquote(Common.contained_true()), rest::binary>> ->
          {true, rest}

        _ ->
          :error
      end
    end
  end

  defp deserialize_value(:i8, binary_var, _file_group) do
    quote do
      case unquote(binary_var) do
        <<val::8-signed, rest::binary>> ->
          {val, rest}

        _ ->
          :error
      end
    end
  end

  defp deserialize_value(:double, binary_var, _file_group) do
    quote do
      case unquote(binary_var) do
        <<val::64-float-little, rest::binary>> ->
          {val, rest}

        _ ->
          :error
      end
    end
  end

  defp deserialize_value({:map, _} = type, binary_var, file_group) do
    fun_name = container_deserializer_fun_name(type, file_group)

    quote do
      unquote(fun_name)(unquote(binary_var))
    end
  end

  defp deserialize_value(%TypeRef{referenced_type: referenced_type}, binary_var, file_group) do
    resolved = FileGroup.resolve(file_group, referenced_type)
    deserialize_value(resolved, binary_var, file_group)
  end

  defp deserialize_value(%{__struct__: mod} = struct_def, binary_var, file_group)
       when mod in [Struct, Exception, Union] do
    dest_module = FileGroup.dest_module(file_group, struct_def)

    quote do
      unquote(dest_module).deserialize(unquote(binary_var), :compact)
    end
  end

  defp deserialize_value(type, binary_var, _file_group) when type in [:i16, :i32, :i64] do
    quote do
      Thrift.Protocol.Compact.IntegerEncoding.decode_zigzag_varint(unquote(binary_var))
    end
  end

  defp deserialize_value(%TEnum{}, binary_var, file_group) do
    deserialize_value(:i32, binary_var, file_group)
  end

  defp deserialize_value(type, binary_var, _file_group) when type in [:string, :binary] do
    quote do
      Thrift.Protocol.Compact.deserialize_binary(unquote(binary_var))
    end
  end

  defp list_or_set_deserializer(%TypeRef{referenced_type: type}, deserialize_item_fun, file_group) do
    file_group
    |> FileGroup.resolve(type)
    |> list_or_set_deserializer(deserialize_item_fun, file_group)
  end

  defp list_or_set_deserializer(:bool, deserialize_item, _file_group) do
    quote do
      defp unquote(deserialize_item)(
             <<unquote(Common.contained_false()), rest::binary>>,
             {items, countdown}
           ) do
        unquote(deserialize_item)(rest, {[false | items], countdown - 1})
      end

      defp unquote(deserialize_item)(
             <<unquote(Common.contained_true()), rest::binary>>,
             {items, countdown}
           ) do
        unquote(deserialize_item)(rest, {[true | items], countdown - 1})
      end
    end
  end

  defp list_or_set_deserializer(:i8, deserialize_item_fun, _file_group) do
    quote do
      defp unquote(deserialize_item_fun)(
             <<val::8-signed, rest::binary>>,
             {items, countdown}
           ) do
        unquote(deserialize_item_fun)(rest, {[val | items], countdown - 1})
      end
    end
  end

  defp list_or_set_deserializer(:double, deserialize_item_fun, _file_group) do
    quote do
      defp unquote(deserialize_item_fun)(
             <<val::64-float-little, rest::binary>>,
             {items, remaining}
           ) do
        unquote(deserialize_item_fun)(rest, {[val | items], remaining - 1})
      end
    end
  end

  defp list_or_set_deserializer(type, deserialize_item_fun, file_group) do
    quote do
      defp unquote(deserialize_item_fun)(rest, {items, remaining}) do
        case unquote(deserialize_value(type, :rest, file_group)) do
          {val, rest} ->
            unquote(deserialize_item_fun)(rest, {[val | items], remaining - 1})

          _ ->
            :error
        end
      end
    end
  end

  defp reject_void_and_sort_fields(fields) do
    fields
    |> Enum.reject(&(&1.type == :void))
    |> Enum.sort_by(& &1.id)
  end

  defdelegate struct_serializer(struct_def, name, file_group), to: StructSerialize
end
