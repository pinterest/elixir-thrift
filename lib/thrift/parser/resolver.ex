defmodule Thrift.Parser.Resolver do
  @moduledoc false

  # A resolver for references. During file parsing, all new generated thrift
  # concepts flow through this resolver and are added to its global database
  # of names. At the end, the database is dumped into the FileGroup so it can
  # resolve references.

  alias Thrift.AST.TEnum

  def add(state, name, schema) do
    state
    |> update(name, schema.constants)
    |> update(name, schema.services)
    |> update(name, schema.structs)
    |> update(name, schema.exceptions)
    |> update(name, schema.unions)
    |> update(name, schema.enums)
    |> update(name, schema.typedefs)
  end

  defp update(%{} = resolutions, include_name, %{} = local_mappings) do
    new_type_mappings =
      Map.new(local_mappings, fn
        {name, val} when is_atom(val) or is_tuple(val) ->
          {:"#{include_name}.#{name}", val}

        {name, val} when is_map(val) ->
          {:"#{include_name}.#{name}", Map.put(val, :name, :"#{include_name}.#{name}")}
      end)

    new_value_mappings =
      Enum.reduce(local_mappings, %{}, fn
        {_, %TEnum{name: enum_name, values: values}}, acc ->
          Enum.reduce(values, acc, fn
            {value_name, value}, acc ->
              Map.put(acc, :"#{enum_name}.#{value_name}", value)
          end)

        _, acc ->
          acc
      end)

    resolutions
    |> Map.merge(new_type_mappings)
    |> Map.merge(new_value_mappings)
  end
end
