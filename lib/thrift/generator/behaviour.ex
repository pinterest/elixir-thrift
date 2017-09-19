defmodule Thrift.Generator.Behaviour do
  @moduledoc """
  A generator for a handler module's behaviour.

  Takes a thrift service definition and creates a behavoiur module for users
  to implement. Thrift types are converted into Elixir typespecs that are
  equivalent to their thrift counterparts.
  """
  alias Thrift.Generator.Utils
  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    Exception,
    Field,
    Struct,
    TypeRef,
    TEnum,
    Union,
  }

  require Logger

  def generate(schema, service) do
    file_group = schema.file_group
    dest_module = Module.concat(FileGroup.dest_module(file_group, service), Handler)

    callbacks = service.functions
    |> Map.values
    |> Enum.map(&create_callback(file_group, &1))

    behaviour_module = quote do
      defmodule unquote(dest_module) do
        unquote_splicing(callbacks)
      end
    end

    {dest_module, behaviour_module}
  end

  defp create_callback(file_group, function) do
    callback_name = Utils.underscore(function.name)

    return_type = typespec(function.return_type, file_group)
    params = function.params
    |> Enum.map(&FileGroup.resolve(file_group, &1))
    |> Enum.map(&to_arg_spec(&1, file_group))

    quote do
      @callback unquote(callback_name)(unquote_splicing(params)) :: unquote(return_type)
    end
  end

  def to_arg_spec(%Field{name: name, type: type}, file_group) do
    quote do
      unquote(Macro.var(name, nil)) :: unquote(typespec(type, file_group))
    end
  end

  defp typespec(:void, _), do: quote do: no_return()
  defp typespec(:bool, _), do: quote do: boolean()
  defp typespec(:string, _), do: quote do: String.t
  defp typespec(:binary, _), do: quote do: binary
  defp typespec(:i8, _), do: quote do: Thrift.i8
  defp typespec(:i16, _), do: quote do: Thrift.i16
  defp typespec(:i32, _), do: quote do: Thrift.i32
  defp typespec(:i64, _), do: quote do: Thrift.i64
  defp typespec(:double, _), do: quote do: Thrift.double
  defp typespec(%TypeRef{} = ref, file_group) do
    file_group
    |> FileGroup.resolve(ref)
    |> typespec(file_group)
  end
  defp typespec(%TEnum{}, _) do
    quote do
      non_neg_integer
    end
  end
  defp typespec(%Union{name: name}, file_group) do
    dest_module = FileGroup.dest_module(file_group, name)
    quote do
      %unquote(dest_module){}
    end
  end
  defp typespec(%Exception{name: name}, file_group) do
    dest_module = FileGroup.dest_module(file_group, name)
    quote do
      %unquote(dest_module){}
    end
  end
  defp typespec(%Struct{name: name}, file_group) do
    dest_module = FileGroup.dest_module(file_group, name)
    quote do
      %unquote(dest_module){}
    end
  end
  defp typespec({:set, _t}, _) do
    quote do
      %MapSet{}
    end
  end
  defp typespec({:list, t}, file_group) do
    quote do
      [unquote(typespec(t, file_group))]
    end
  end
  defp typespec({:map, {k, v}}, file_group) do
    key_type = typespec(k, file_group)
    val_type = typespec(v, file_group)
    quote do
      %{unquote(key_type) => unquote(val_type)}
    end
  end
  defp typespec(unknown_typespec, _) do
    Logger.error("Unknown type: #{inspect unknown_typespec}. Falling back to any()")
    quote do
      any
    end
  end
end
