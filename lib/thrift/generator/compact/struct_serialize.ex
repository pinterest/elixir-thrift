defmodule Thrift.Generator.Compact.StructSerialize do
  @moduledoc """
  Generates serialisation functions for the compact protocol.

  See `Thrift.Generator.StructCompactProtocol`
  """
  alias Thrift.Generator.Compact.Common
  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup

  alias Thrift.AST.{
    Exception,
    Field,
    Struct,
    TEnum,
    TypeRef,
    Union
  }

  alias Thrift.Protocol.Compact

  def struct_serializer(%{fields: fields} = struct_def, name, file_group) do
    fields
    |> Common.reject_void_and_sort_fields()
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
         %Field{name: name} = field,
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
              Compact.field_header({0, id}, Common.type_id({:bool, false})),
              <<0>>
            ])
          )

        true ->
          unquote(
            Utils.optimize_iolist([
              Compact.field_header({0, id}, Common.type_id({:bool, true})),
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
    field_header = Compact.field_header({0, id}, Common.type_id(type, file_group))

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
        unquote(Compact.field_header({0, id}, Common.type_id(type, file_group)))
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
            unquote(Common.type_id({:bool, false}))
          )

        true ->
          Thrift.Protocol.Compact.field_header(
            {unquote(previous_id), unquote(id)},
            unquote(Common.type_id({:bool, true}))
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
          unquote(Common.type_id({:map, nil}))
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

  defp do_field_serializer(%Field{name: name} = field, {_struct_name, nil_val}, file_group) do
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
          unquote(Common.type_id(type, file_group))
        )
      end,
      value_serializer(type, var, file_group)
    ]
  end

  defp value_serializer(:bool, var, _file_group) do
    quote do
      case unquote(var) do
        nil -> <<unquote(Common.contained_false())>>
        false -> <<unquote(Common.contained_false())>>
        _ -> <<unquote(Common.contained_true())>>
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
      <<unquote(var)::64-float-little>>
    end
  end

  defp value_serializer({:map, {key_type, val_type}}, var, file_group) do
    type_header =
      <<Common.contained_type_id(key_type, file_group)::4-unsigned,
        Common.contained_type_id(val_type, file_group)::4-unsigned>>

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
          <<size::4-unsigned,
            unquote(Common.contained_type_id(val_type, file_group))::4-unsigned>>,
          serialised_elements
        ]
      else
        [
          <<15::4-unsigned, unquote(Common.contained_type_id(val_type, file_group))::4-unsigned>>,
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
end
