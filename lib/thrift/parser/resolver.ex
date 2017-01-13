defmodule Thrift.Parser.Resolver do
  @moduledoc false

  # A resolver for references. During file parsing, all new generated thrift
  # concepts flow through this resolver and are added to its global database
  # of names. At the end, the database is dumped into the FileGroup so it can
  # resolve references.

  alias Thrift.Parser.ParsedFile
  alias Thrift.Parser.Models.TEnum

  def add(state, %ParsedFile{} = f) do
    state
    |> update(f.name, f.schema.constants)
    |> update(f.name, f.schema.services)
    |> update(f.name, f.schema.structs)
    |> update(f.name, f.schema.exceptions)
    |> update(f.name, f.schema.unions)
    |> update(f.name, f.schema.enums)
    |> update(f.name, f.schema.typedefs)
  end

  defp update(%{} = resolutions, include_name, %{} = local_mappings) do
    new_type_mappings = Map.new(local_mappings, fn
      {name, val} when is_atom(val) or is_tuple(val) ->
        {:"#{include_name}.#{name}", val}
      {name, val} when is_map(val) ->
        {:"#{include_name}.#{name}", Map.put(val, :name, :"#{include_name}.#{name}")}
    end)

    new_value_mappings = Enum.reduce(local_mappings, %{}, fn
      {_, %TEnum{name: enum_name, values: values}}, acc ->
        Enum.reduce(values, acc, fn
          {value_name, value}, acc ->
            Map.put(acc, :"#{enum_name}.#{value_name}", value)
        end)
      _, acc ->
        acc
    end)

    resolutions
    |> merge_new(new_type_mappings)
    |> merge_new(new_value_mappings)
  end

  # Similar to Map.merge but raises an error if there are any duplicate keys.
  defp merge_new(existing_map, new_map) do
    existing_keys = Map.keys(existing_map) |> MapSet.new
    new_keys = Map.keys(new_map) |> MapSet.new

    unless MapSet.disjoint?(existing_keys, new_keys) do
      shared_keys = new_keys
      |> MapSet.intersection(existing_keys)
      |> Enum.map_join(", ", &to_string/1)
      raise "Name collision: #{shared_keys}"
    end

    Map.merge(existing_map, new_map)
  end
end
