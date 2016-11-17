defmodule Thrift.Protocols.Binary do
  alias Thrift.Parser.FileGroup

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

  def primitive_serializers do
    type_converters = for {atom_type, int_type} <- @types do
      quote do
        def int_type(unquote(atom_type)) do
          unquote(int_type)
        end
      end
    end

    quote location: :keep do
      unquote_splicing(type_converters)
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
        unquote(integer_serializer(8))
      end
      def serialize(:i8, value) do
        unquote(integer_serializer(8))
      end
      def serialize(:i16, value) do
        unquote(integer_serializer(16))
      end
      def serialize(:i32, value) do
        unquote(integer_serializer(32))
      end
      def serialize(:i64, value) do
        unquote(integer_serializer(64))
      end
      def serialize(:double, value) do
        <<value::signed-float>>
      end
      def serialize(:string, value) do
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

      def serialize(:message_begin, {sequence_id, message_type, name}) do
        # Taken from https://erikvanoosten.github.io/thrift-missing-specification/#_message_encoding

        <<1::size(1), 1::size(15), 0::size(8),
        # ^^ Strange, I know. We could integrate the 8-bit zero here with the 5 bit zero below.
        0::size(5), to_message_type(message_type)::size(3),
        byte_size(name)::32-signed, sequence_id::32-signed>>
      end
    end
  end

  def build(file_group, struct) do
    alias Thrift.Generator.Models.BinaryProtocol, as: Deserializer
    name = FileGroup.dest_module(file_group, struct.name)

    quote do
      defmodule BinaryProtocol do
        unquote(primitive_serializers)
        unquote(generate_serializer(file_group, struct))
        unquote(Deserializer.struct_deserializer(struct, name, file_group))
      end
    end
  end

  def generate_serializer(file_group, %Struct{}=struct) do
    generate_generic_serializer(file_group, struct, :struct)
  end

  def generate_serializer(file_group, %Exception{}=ex) do
    generate_generic_serializer(file_group, ex, :exception)
  end

  defp generate_generic_serializer(file_group, %{fields: fields, name: name}, match_type) do
    serializers = fields
    |> Enum.sort_by(&(&1.id))
    |> Enum.map(&FileGroup.resolve(file_group, &1))
    |> Enum.map(&generate_field_call(file_group, &1))
    |> append_struct_stop

    dest_module = FileGroup.dest_module(file_group, name)
    field_match = generate_field_match(dest_module, fields)

    quote do
      def serialize(unquote(match_type), unquote(field_match)) do
        unquote(serializers)
      end
    end
  end

  defp to_generic_type(type) do
    case type do
      {:map, {key_type, val_type}} ->
        {:map, {to_generic_type(key_type), to_generic_type(val_type)}}

      {:list, elem_type} ->
        {:list, to_generic_type(elem_type)}

      {:set, elem_type} ->
        {:set, to_generic_type(elem_type)}

      val when is_map(val) ->
        :struct

      val ->
        val
    end
  end

  defp generate_field_call(file_group, %Field{type: %Struct{}}=field) do
    dest_module = file_group
    |> FileGroup.dest_module(field.type)
    |> Module.concat(BinaryProtocol)
    |> field_serializer_stanza(field)
  end

  defp generate_field_call(file_group, %Field{}=field) do
    field = FileGroup.resolve(file_group, field)
    generic_type = to_generic_type(field.type)

    self = quote do: __MODULE__
    field_serializer_stanza(self, field)
  end

  def header_for(%Field{type: type}=field) do
    type_name = case type do
                  primitive when is_atom(primitive) ->
                    primitive

                  {composite, _} ->
                    composite

                  %TEnum{} ->
                    :i8

                  %Struct{} ->
                    :struct
                end
    @types
    |> Map.get(type_name)
    |> build_struct_field_header(field)
  end

  def build_struct_field_header(type, field) do
    quote do
      <<unquote(type)::size(8), unquote(field.id)::size(16)>>
    end
  end

  defp integer_serializer(bit_size) do
    quote do
      <<value::unquote(bit_size)-signed>>
    end
  end

  def append_struct_stop(serializers) do
    quoted_stop = quote do
      <<0::size(8)>>
    end

    serializers ++ [quoted_stop]
  end

  defp generate_field_match(dest_module, field_list) do
    match_kw = field_list
    |> Enum.map(fn(field) ->
      {field.name, Macro.var(field.name, Elixir)}
    end)

    {:%, [],
     [{:__aliases__, [alias: false], [dest_module]},
      {:%{}, [], match_kw}]}
  end

  def field_serializer_stanza(serializer_module, %Field{}=field) do
    field_var = Macro.var(field.name, Elixir)
    field_type = to_generic_type(field.type)
    quote do
      case unquote(field_var) do
        nil ->
          []
        field_value ->
          [unquote(header_for(field)), unquote(serializer_module).serialize(unquote(field_type), field_value)]
      end
    end
  end
end
