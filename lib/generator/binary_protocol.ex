defmodule Thrift.Generator.Models.BinaryProtocol do
  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    # Exception,
    # Field,
    Struct,
    StructRef,
    TEnum,
  }

  def struct_deserializer(%{fields: fields}, name, file_group) do
    # field_matchers = Enum.map(fields, fn %Field{name: name} ->
    #   {name, Macro.var(name, nil)}
    # end)

    # struct_matcher = {:%, [], [name, {:%{}, [], field_matchers}]}

    # field_serializers = Enum.map(fields, fn %Field{name: name, type: type, id: id} ->
    #   var = Macro.var(name, nil)
    #   quote do
    #     case unquote(var) do
    #       nil ->
    #         []
    #       _ ->
    #         unquote([
    #           quote do <<unquote(type_id(type, file_group)), unquote(id) :: size(16)>> end,
    #           value_serializer(type, var, file_group)
    #         ] |> merge_binaries)
    #     end
    #   end
    # end)

    field_deserializers = fields
    |> Enum.map(&field_deserializer(&1.type, &1, :deserialize, file_group))
    |> merge_blocks

    quote do
      # def serialize(unquote(struct_matcher)) do
      #   unquote([field_serializers, <<0>>] |> merge_binaries |> merge_binaries)
      # end
      def deserialize(binary) do
        deserialize(binary, %unquote(name){})
      end
      defp deserialize(<<0, rest::binary>>, acc=%unquote(name){}) do
        {acc, rest}
      end
      unquote_splicing(field_deserializers)
      # defp bool_to_int(nil), do: 0
      # defp bool_to_int(false), do: 0
      # defp bool_to_int(_), do: 1
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

  def field_deserializer(:i64, field, name, _file_group) do
    quote do
      defp unquote(name)(<<10, unquote(field.id)::size(16), value::size(64), rest::binary>>, acc) do
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

  def field_deserializer(:string, field, name, _file_group) do
    quote do
      defp unquote(name)(<<11, unquote(field.id)::16-signed, string_size::size(32), rest::binary>>, acc) do
        <<value::binary-size(string_size), rest::binary>> = rest
        unquote(name)(rest, %{acc | unquote(field.name) => value})
      end
    end
  end

    # defp(deserialize_optional_integers(<<value::size(32), rest::binary>>, acc, elements, remaining)) when remaining > 0 do
    #   deserialize_optional_integers(rest, acc, [value | elements], remaining - 1)
    # end

  def field_deserializer({:list, element_type}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"
    quote do
      defp unquote(name)(<<15, unquote(field.id)::size(16), unquote(type_id(element_type, file_group)), remaining::size(32), rest::binary>>, struct) do
        unquote(sub_name)(rest, [[], remaining, struct])
      end
      defp unquote(sub_name)(rest, [list, 0, struct]) do
        unquote(name)(rest, %{struct | unquote(field.name) => Enum.reverse(list)})
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end

  # def field_deserializer({:set, element_type}, field, name, file_group) do
  #   sub_name = :"#{name}__#{field.name}"
  #   quote do
  #     defp unquote(name)(<<14, unquote(field.id)::size(16), unquote(type_id(element_type, file_group)), list_size::size(32), rest::binary>>, struct) do
  #       unquote(sub_name)(rest, [[], struct], list_size)
  #     end
  #     defp unquote(sub_name)(rest, [elements, struct], 0) do
  #       unquote(name)(rest, %{struct | unquote(field.name) => Enum.reverse(elements)})
  #     end
  #     unquote(list_deserializer(element_type, sub_name, file_group))
  #   end
  # end

  def field_deserializer({:map, {key_type, value_type}}, field, name, file_group) do
    sub_name = :"#{name}__#{field.name}"
    quote do
      defp unquote(name)(<<13,
                           unquote(field.id)::size(16),
                           unquote(type_id(key_type, file_group)),
                           unquote(type_id(value_type, file_group)),
                           map_size::size(32),
                           rest::binary>>, struct) do
        unquote(sub_name)(rest, [%{}, struct], map_size)
      end
      defp unquote(sub_name)(rest, [map, struct], 0) do
        unquote(name)(rest, %{struct | unquote(field.name) => map})
      end
      unquote(map_key_deserializer(key_type, sub_name, file_group))
      unquote(map_value_deserializer(value_type, sub_name, file_group))
    end
  end

  def field_deserializer(type, field, _name, _file_group) do
    quote do
      _ = unquote("field deserializer not implemented for #{field.id}=#{field.name} #{inspect type}")
    end
  end



  def map_key_deserializer(:byte, name, file_group) do
    map_key_deserializer(:i8, name, file_group)
  end

  def map_key_deserializer(:i8, name, _file_group) do
    quote do
      defp unquote(name)(<<key, rest::binary>>, stack, remaining) do
        unquote(name)(rest, key, stack, remaining)
      end
    end
  end

  def map_key_deserializer(:i16, name, _file_group) do
    quote do
      defp unquote(name)(<<key::size(16), rest::binary>>, stack, remaining) do
        unquote(name)(rest, key, stack, remaining)
      end
    end
  end

  def map_key_deserializer(:i32, name, _file_group) do
    quote do
      defp unquote(name)(<<key::size(32), rest::binary>>, stack, remaining) do
        unquote(name)(rest, key, stack, remaining)
      end
    end
  end

  def map_key_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<key::size(64), rest::binary>>, stack, remaining) do
        unquote(name)(rest, key, stack, remaining)
      end
    end
  end

  def map_key_deserializer(%StructRef{referenced_type: type}, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_key_deserializer(name, file_group)
  end




  def map_value_deserializer(:i8, name, _file_group) do
    quote do
      defp unquote(name)(<<value, rest::binary>>, key, [map | stack], remaining) do
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(:i16, name, _file_group) do
    quote do
      defp unquote(name)(<<value::size(16), rest::binary>>, key, [map | stack], remaining) do
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(:i32, name, _file_group) do
    quote do
      defp unquote(name)(<<value::size(32), rest::binary>>, key, [map | stack], remaining) do
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<value::size(64), rest::binary>>, key, [map | stack], remaining) do
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(:string, name, _file_group) do
    quote do
      defp unquote(name)(<<string_size::size(64), rest::binary>>, key, [map | stack], remaining) do
        <<value::binary-size(string_size), rest::binary>> = rest
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(struct=%Struct{}, name, _file_group) do
    quote do
      defp unquote(name)(rest, key, [map | stack], remaining) do
        {value, rest} = unquote(struct.name).BinaryProtocol.deserialize(rest)
        unquote(name)(rest, [Map.put(map, key, value) | stack], remaining - 1)
      end
    end
  end

  def map_value_deserializer(%StructRef{referenced_type: type}, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> map_value_deserializer(name, file_group)
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

  def list_deserializer(:i64, name, _file_group) do
    quote do
      defp unquote(name)(<<element::size(64), rest::binary>>, [list, remaining | stack]) do
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  def list_deserializer(:string, name, _file_group) do
    quote do
      defp unquote(name)(rest, [list, remaining | stack]) do
        <<element::binary-size(string_size), rest::binary>> = rest
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  def list_deserializer({:list, element_type}, name, file_group) do
    sub_name = :"#{name}__element"
    quote do
      defp unquote(name)(<<unquote(type_id(element_type, file_group)), inner_remaining::size(32), rest::binary>>, [list, remaining | stack]) do
        unquote(sub_name)(rest, [[], inner_remaining, list, remaining | stack])
      end
      defp unquote(sub_name)(rest, [inner_list, 0, list, remaining | stack]) do
        unquote(name)(rest, [[Enum.reverse(inner_list) | list], remaining - 1 | stack])
      end
      unquote(list_deserializer(element_type, sub_name, file_group))
    end
  end

  def list_deserializer(struct=%Struct{}, name, _file_group) do
    quote do
      defp unquote(name)(rest, [list, remaining | stack]) do
        {element, rest} = unquote(struct.name).BinaryProtocol.deserialize(rest)
        unquote(name)(rest, [[element | list], remaining - 1 | stack])
      end
    end
  end

  def list_deserializer(%StructRef{referenced_type: type}, name, file_group) do
    FileGroup.resolve(file_group, type)
    |> list_deserializer(name, file_group)
  end

  # def field_deserializer(%{name: name, type: {:list, element_type}, id: id}, file_group) do
  #   quote do
  #     defp deserialize(<<15, unquote(id)::size(16), unquote(type_id(element_type), file_group), list_size::size(32), rest::binary>>, acc) do
  #       unquote(:"deserialize_#{name}")(rest, acc, [], list_size)
  #     end
  #     defp unquote(:"deserialize_#{name}")(rest, acc, elements, 0) do
  #       deserialize(rest, %{acc | unquote(name) => Enum.reverse(elements)})
  #     end
  #   end
  # end

    # defp(deserialize(<<15, 8::size(16), 12, count::size(32), rest::binary>>, user)) do
    #   deserialize_friends(rest, user, [], count)
    # end
    # defp deserialize_friends(rest, user, friends, count) when count > 0 do
    #   {friend, rest} = User.BinaryProtocol.deserialize(rest)
    #   deserialize_friends(rest, user, [friend | friends], count - 1)
    # end
    # defp deserialize_friends(rest, user, friends, 0) do
    #   deserialize(rest, %{user | friends: Enum.reverse(friends)})
    # end









  def value_serializer(:bool,    var, _file_group), do: quote do: <<bool_to_int(unquote(var))>>
  def value_serializer(:byte,    var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  def value_serializer(:i8,      var, _file_group), do: quote do: <<unquote(var) :: 8-signed>>
  def value_serializer(:double,  var, _file_group), do: quote do: <<unquote(var) :: signed-float>>
  def value_serializer(:i16,     var, _file_group), do: quote do: <<unquote(var) :: 16-signed>>
  def value_serializer(:i32,     var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  def value_serializer(%TEnum{}, var, _file_group), do: quote do: <<unquote(var) :: 32-signed>>
  def value_serializer(:i64,     var, _file_group), do: quote do: <<unquote(var) :: 64-signed>>
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
          ] |> merge_binaries)
        end
      ]
    end
  end

  def value_serializer({:set, type}, var, file_group) do
    value_serializer({:list, type}, var, file_group)
  end

  def value_serializer({:list, type}, var, file_group) do
    quote do
      [
        <<unquote(type_id(type, file_group)), length(unquote(var)) :: size(32)>>,
        for unquote(Macro.var(:e, nil)) <- unquote(var) do
          unquote(value_serializer(type, Macro.var(:e, nil), file_group) |> merge_binaries)
        end,
      ]
    end
  end

  def value_serializer(_struct=%Struct{name: name}, var, _file_group) do
    IO.puts "Can't resolve ref #{inspect name}"
    # IO.inspect struct
    # IO.inspect file_group
    quote do
      unquote(name).BinaryProtocol.serialize(unquote(var))
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
  def type_id(%Struct{}, _file_group), do: 12
  def type_id({:map, _}, _file_group), do: 13
  def type_id({:set, _}, _file_group), do: 14
  def type_id({:list, _}, _file_group), do: 15

  def type_id(%StructRef{referenced_type: type}, file_group) do
    FileGroup.resolve(file_group, type)
    |> type_id(file_group)
  end



  def merge_binaries([a | rest]) when is_list(a) do
    merge_binaries(a ++ rest)
  end

  def merge_binaries([a, b | rest]) when is_list(b) do
    merge_binaries([a] ++ b ++ rest)
  end

  def merge_binaries([{:<<>>, [], a}, {:<<>>, [], b} | rest]) do
    merge_binaries([{:<<>>, [], a ++ b} | rest])
  end

  def merge_binaries([a | rest]) do
    [a] ++ merge_binaries(rest)
  end

  def merge_binaries(a) do
    a
  end



  def merge_blocks([{:__block__, [], a} | rest]) do
    merge_blocks(a) ++ merge_blocks(rest)
  end

  def merge_blocks([a | rest]) do
    # IO.inspect a
    [a] ++ merge_blocks(rest)
  end

  def merge_blocks([]) do
    []
  end
end
