defmodule Thrift.Parser.Resolver do
  @moduledoc """
  A resolver for references.
  During file parsing, all new generated thrift concepts flow through this resolver
  and are added to its global database of names. At the end, the database is dumped into
  the FileGroup so it can resolve references.
  """
  alias Thrift.Parser.ParsedFile

  def start_link do
    Agent.start_link(&Map.new/0)
  end

  def stop(pid) do
    Agent.stop(pid)
  end

  def add(pid, f=%ParsedFile{}) do
    Agent.update(pid, fn(state) ->
      state
      |> update(f.name, f.schema.services)
      |> update(f.name, f.schema.structs)
      |> update(f.name, f.schema.exceptions)
      |> update(f.name, f.schema.unions)
      |> update(f.name, f.schema.enums)
      |> update(f.name, f.schema.typedefs)
    end)
  end

  def get(pid) do
    Agent.get(pid, &(&1))
  end

  defp update(%{}=state, include_name, %{}=local_mappings) do
    new_mappings = local_mappings
    |> Map.new(fn {name, val} ->
      case val do
        val when is_atom(val) ->
          {:"#{include_name}.#{name}", val}
        val when is_map(val) ->
          {:"#{include_name}.#{name}", Map.put(val, :name, :"#{include_name}.#{name}")}
      end
    end)

    Map.merge(state, new_mappings)
  end
end
