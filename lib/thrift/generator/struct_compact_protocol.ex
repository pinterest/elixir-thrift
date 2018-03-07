defmodule Thrift.Generator.StructCompactProtocol do
  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup

  alias Thrift.Parser.Models.{
    Exception,
    Field,
    Struct,
    TypeRef,
    TEnum,
    Union
  }

  alias Thrift.Protocol.Compact.IntegerEncoding
  alias Thrift.Protocol.Compact
  require Thrift.Protocol.Compact.Type, as: Type

  def struct_deserializer(%{fields: fields}, name, file_group) do
    fields = Enum.reject(fields, &(&1.type == :void))

    container_deserializers =
      fields
      |> container_deserializers(file_group)
      |> Map.values()
      |> Utils.merge_blocks()

    field_deserializers =
      fields
      |> Enum.map(&field_deserializer(&1.type, &1, file_group))
      |> Utils.merge_blocks()

    quote do
      def deserialize(binary) do
        deserialize(binary, 0, %unquote(name){})
      end

      defp deserialize(<<0, rest::binary>>, _, %unquote(name){} = acc) do
        {acc, rest}
      end

      unquote_splicing(field_deserializers)
      unquote_splicing(container_deserializers)

      # No tests so far to skip over unknown fields

      defp deserialize(rest, previous, struct) do
        {:error, {rest, previous, struct}}
      end
    end
  end

  defp container_deserializers(fields, file_group) do
    types = for %Field{type: type} <- fields, do: type
    container_deserializers(%{}, types, file_group)
  end

  defp container_deserializers(acc, [], _), do: acc

  defp container_deserializers(
         acc,
         [%TypeRef{referenced_type: referenced_type} | rest],
         file_group
       ) do
    type = FileGroup.resolve(file_group, referenced_type)
    container_deserializers(acc, [type | rest], file_group)
  end

  defp container_deserializers(acc, [{:map, {key_type, value_type}} = type | rest], file_group) do
    acc
    |> add_container_deserializer(type, file_group)
    |> container_deserializers([key_type], file_group)
    |> container_deserializers([value_type], file_group)
    |> container_deserializers(rest, file_group)
  end

  defp container_deserializers(acc, [{t, element_type} = type | rest], file_group)
       when t in [:list, :set] do
    acc
    |> container_deserializers([element_type], file_group)
    |> add_container_deserializer({type, file_group})
    |> container_deserializers(rest, file_group)
  end

  defp container_deserializers(acc, [_ | rest], file_group) do
    quote do
      unquote(container_deserializers(acc, rest, file_group))
    end
  end

  defp add_container_deserializer(acc, {:map, {key_type, value_type}} = type, file_group) do
    fun_name = container_deserializer_fun_name(type, file_group)
    key_type_id = contained_type_id(key_type, file_group)
    value_type_id = contained_type_id(value_type, file_group)

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
            err ->
              # todo - just :error
              {:error, err}
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
    element_type_id = contained_type_id(element_type, file_group)

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
             ) do
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

  defp short_header_field_deserializer(type, field = %Field{id: id}, file_group) do
    quote do
      defp deserialize(
             <<delta::4-unsigned, unquote(type_id(type, file_group))::4-unsigned, rest::binary>>,
             previous_id,
             struct
           )
           when delta + previous_id == unquote(id) do
        unquote(body_deserializer(type, field, :rest, file_group))
      end
    end
  end

  defp long_header_field_deserializer(type, field = %Field{id: id}, file_group) do
    header_match =
      Utils.optimize_iolist([
        <<0::4, type_id(type, file_group)::4-unsigned>>,
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
        # Todo: why is this little endian. It specifies big-endian in the document but
        # the ruby serialisation comes out as little.
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
        <<unquote(contained_false()), rest::binary>> ->
          {false, rest}

        <<unquote(contained_true()), rest::binary>> ->
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
             <<unquote(contained_false()), rest::binary>>,
             {items, countdown}
           ) do
        unquote(deserialize_item)(rest, {[false | items], countdown - 1})
      end

      defp unquote(deserialize_item)(
             <<unquote(contained_true()), rest::binary>>,
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

  defp reject_void_fields(fields) do
    Enum.reject(fields, &(&1.type == :void))
  end

  def struct_serializer(%{fields: fields} = struct_def, name, file_group) do
    fields
    |> reject_void_fields()
    |> do_struct_serializer(struct_def, name, file_group)
  end

  defp do_struct_serializer(fields, %Union{}, name, file_group) do
    all_fields_nil = Enum.map(fields, fn %Field{name: nil_name} -> {nil_name, nil} end)

    field_serializers =
      for field <- fields, do: union_field_serializer(all_fields_nil, field, name, file_group)

    quote do
      def serialize(%unquote(name){unquote_splicing(all_fields_nil)}) do
        <<0>>
      end

      unquote_splicing(field_serializers)

      def serialize(%unquote(name){} = union) do
        set_fields =
          union
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

  defp do_struct_serializer([], _, _name, _file_group) do
    quote do
      def serialize(_) do
        <<0>>
      end
    end
  end

  defp do_struct_serializer(fields, _, name, file_group) do
    [%Field{name: first_field_name} | _] = fields

    first_serialize_fun = serialize_field_method_name(first_field_name)

    serializer_funs =
      fields
      |> field_serializer_funs(name, file_group, [])
      |> Utils.merge_blocks()

    quote do
      def serialize(struct) do
        unquote(first_serialize_fun)(struct, {0, []})
      end

      unquote_splicing(serializer_funs)
    end
  end

  defp union_field_serializer(
         nil_fields,
         field = %Field{name: name},
         struct,
         file_group
       ) do
    var = Macro.var(:val, nil)
    single_field_not_nil = Keyword.put(nil_fields, name, var)

    quote do
      def serialize(%unquote(struct){unquote_splicing(single_field_not_nil)}) do
        unquote(do_union_field_serializer(field, var, {struct, file_group}))
      end
    end
  end

  defp do_union_field_serializer(%Field{type: :bool, id: id, name: name}, var, {struct_name, _}) do
    quote do
      case unquote(var) do
        false ->
          unquote(
            Utils.optimize_iolist([
              Compact.field_header({0, id}, type_id({:bool, false})),
              <<0>>
            ])
          )

        true ->
          unquote(
            Utils.optimize_iolist([
              Compact.field_header({0, id}, type_id({:bool, true})),
              <<0>>
            ])
          )

        _ ->
          raise Thrift.InvalidValueError,
                unquote(
                  "Boolean field #{inspect(name)} on #{inspect(struct_name)} must be true, false, or nil"
                )
      end
    end
  end

  defp do_union_field_serializer(%Field{id: id, type: {:map, _} = type}, var, {_, file_group}) do
    field_header = Compact.field_header({0, id}, type_id(type, file_group))

    quote do
      if map_size(unquote(var)) == 0 do
        unquote(Utils.optimize_iolist([field_header, <<0, 0>>]))
      else
        [unquote(field_header), unquote(value_serializer(type, var, file_group)), <<0>>]
      end
    end
  end

  defp do_union_field_serializer(%Field{id: id, type: type}, var, {_, file_group}) do
    [
      quote do
        unquote(Compact.field_header({0, id}, type_id(type, file_group)))
      end,
      value_serializer(type, var, file_group),
      <<0>>
    ]
  end

  defp field_serializer_funs(
         [field = %Field{name: name, id: id} | [%Field{name: next_name} | _] = rest],
         struct_name,
         file_group,
         functions
       ) do
    fun =
      quote do
        defp unquote(serialize_field_method_name(name))(
               %unquote(struct_name){unquote(name) => unquote(Macro.var(name, nil))} = struct,
               {previous_id, acc}
             ) do
          serialized_field = unquote(field_serializer(field, struct_name, file_group))

          subsequent_id =
            case serialized_field do
              <<>> -> previous_id
              _ -> unquote(id)
            end

          unquote(serialize_field_method_name(next_name))(
            struct,
            {subsequent_id, [serialized_field | acc]}
          )
        end
      end

    field_serializer_funs(rest, struct_name, file_group, [fun | functions])
  end

  defp field_serializer_funs([field = %Field{name: name}], struct_name, file_group, functions) do
    fun =
      quote do
        defp unquote(serialize_field_method_name(name))(
               %unquote(struct_name){unquote(name) => unquote(Macro.var(name, nil))},
               {previous_id, acc}
             ) do
          serialized_field = unquote(field_serializer(field, struct_name, file_group))
          Enum.reverse([<<0>>, serialized_field | acc])
        end
      end

    [fun | functions]
  end

  defp serialize_field_method_name(field_name), do: :"serialize__#{field_name}"

  defp field_serializer(%Field{required: true, name: name} = field, struct_name, file_group) do
    raise_error =
      quote do
        raise(
          Thrift.InvalidValueError,
          unquote("Required field #{inspect(name)} on #{inspect(struct_name)} must not be nil")
        )
      end

    do_field_serializer(field, {struct_name, raise_error}, file_group)
  end

  defp field_serializer(field, struct_name, file_group) do
    do_field_serializer(
      field,
      {struct_name,
       quote do
         <<>>
       end},
      file_group
    )
  end

  defp do_field_serializer(
         %Field{name: name, type: :bool, id: id},
         {struct_name, nil_val},
         _file_group
       ) do
    previous_id = Macro.var(:previous_id, nil)

    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          unquote(nil_val)

        false ->
          Thrift.Protocol.Compact.field_header(
            {unquote(previous_id), unquote(id)},
            unquote(type_id({:bool, false}))
          )

        true ->
          Thrift.Protocol.Compact.field_header(
            {unquote(previous_id), unquote(id)},
            unquote(type_id({:bool, true}))
          )

        _ ->
          raise Thrift.InvalidValueError,
                unquote(
                  "Optional boolean field #{inspect(name)} on #{inspect(struct_name)} must be true, false, or nil"
                )
      end
    end
  end

  defp do_field_serializer(
         %Field{name: name, id: id, type: type = {:map, _}},
         {_struct_name, nil_val},
         file_group
       ) do
    field_header =
      quote do
        Thrift.Protocol.Compact.field_header(
          {unquote(Macro.var(:previous_id, nil)), unquote(id)},
          unquote(type_id({:map, nil}))
        )
      end

    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          unquote(nil_val)

        val when map_size(val) == 0 ->
          [unquote(field_header), <<0>>]

        var ->
          [
            unquote(field_header),
            unquote(value_serializer(type, Macro.var(:var, nil), file_group))
          ]
      end
    end
  end

  defp do_field_serializer(field = %Field{name: name}, {_struct_name, nil_val}, file_group) do
    quote do
      case unquote(Macro.var(name, nil)) do
        nil ->
          unquote(nil_val)

        _ ->
          unquote(do_field_serialiser(field, file_group))
      end
    end
  end

  def do_field_serialiser(%Field{name: name, id: id, type: type}, file_group) do
    var = Macro.var(name, nil)
    previous_id = Macro.var(:previous_id, nil)

    [
      quote do
        Thrift.Protocol.Compact.field_header(
          {unquote(previous_id), unquote(id)},
          unquote(type_id(type, file_group))
        )
      end,
      value_serializer(type, var, file_group)
    ]
  end

  defp value_serializer(:bool, var, _file_group) do
    quote do
      case unquote(var) do
        nil -> <<unquote(contained_false())>>
        false -> <<unquote(contained_false())>>
        _ -> <<unquote(contained_true())>>
      end
    end
  end

  defp value_serializer(:i8, var, _file_group), do: quote(do: <<unquote(var)::8-signed>>)

  defp value_serializer(:i16, var, _file_group) do
    quote do
      Thrift.Protocol.Compact.IntegerEncoding.encode_zigzag_varint(unquote(var), 16)
    end
  end

  defp value_serializer(:i32, var, _file_group), do: var_int_serliazer(var, 32)
  defp value_serializer(%TEnum{}, var, file_group), do: value_serializer(:i32, var, file_group)
  defp value_serializer(:i64, var, _file_group), do: var_int_serliazer(var, 64)

  defp value_serializer(type, var, _file_group) when type in [:string, :binary] do
    quote do
      [
        Thrift.Protocol.Compact.IntegerEncoding.encode_varint(byte_size(unquote(var))),
        unquote(var)
      ]
    end
  end

  defp value_serializer(:double, var, _file_group) do
    quote do
      # Todo: why is this little endian. It specifies big-endian in the document but
      # the ruby serialisation comes out as little.
      <<unquote(var)::64-float-little>>
    end
  end

  defp value_serializer({:map, {key_type, val_type}}, var, file_group) do
    type_header =
      <<contained_type_id(key_type, file_group)::4-unsigned,
        contained_type_id(val_type, file_group)::4-unsigned>>

    quote do
      unquote(var) =
        if is_list(unquote(var)) do
          Enum.into(unquote(var), %{})
        else
          unquote(var)
        end

      [
        Thrift.Protocol.Compact.IntegerEncoding.encode_varint(map_size(unquote(var))),
        unquote(type_header),
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

  defp value_serializer({type, val_type}, var, file_group) when type in [:list, :set] do
    quote do
      serialised_elements =
        for item <- unquote(var) do
          unquote(contained_value_serializer(val_type, Macro.var(:item, nil), file_group))
        end

      size = Enum.count(unquote(var))

      if size < 15 do
        [
          <<size::4-unsigned, unquote(contained_type_id(val_type, file_group))::4-unsigned>>,
          serialised_elements
        ]
      else
        [
          <<15::4-unsigned, unquote(contained_type_id(val_type, file_group))::4-unsigned>>,
          Thrift.Protocol.Compact.IntegerEncoding.encode_varint(size),
          serialised_elements
        ]
      end
    end
  end

  defp value_serializer(%TypeRef{referenced_type: type}, var, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    value_serializer(resolved, var, file_group)
  end

  defp value_serializer(%Struct{} = struct, var, file_group) do
    struct_value_serializer(struct, var, file_group)
  end

  defp value_serializer(%Union{} = union, var, file_group) do
    struct_value_serializer(union, var, file_group)
  end

  defp value_serializer(%Exception{} = ex, var, file_group) do
    struct_value_serializer(ex, var, file_group)
  end

  defp struct_value_serializer(struct, var, file_group) do
    dest_module = FileGroup.dest_module(file_group, struct)

    quote do
      unquote(dest_module).serialize(unquote(var), :compact)
    end
  end

  defp contained_value_serializer(type, var, file_group) do
    value_serializer(type, var, file_group)
  end

  defp var_int_serliazer(var, size) do
    quote do
      Thrift.Protocol.Compact.IntegerEncoding.encode_zigzag_varint(unquote(var), unquote(size))
    end
  end

  defp contained_type_id(%TypeRef{referenced_type: type}, file_group) do
    file_group
    |> FileGroup.resolve(type)
    |> contained_type_id(file_group)
  end

  defp contained_type_id(:bool, _file_group), do: 1
  defp contained_type_id(type, file_group), do: type_id(type, file_group)

  defp type_id(type) do
    type_id(type, nil)
  end

  defp type_id(%TypeRef{referenced_type: type}, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    type_id(resolved, file_group)
  end

  defp type_id(%TEnum{}, _), do: Type.i32()
  defp type_id(%Struct{}, _), do: Type.struct()
  defp type_id(%Exception{}, _), do: Type.struct()
  defp type_id(%Union{}, _), do: Type.struct()
  defp type_id(type, _file_group), do: Type.of(type)

  defp contained_true, do: 1
  defp contained_false, do: 2
end
