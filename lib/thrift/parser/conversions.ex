defmodule Thrift.Parser.Conversions do
  @moduledoc """
  Conversion utilities useful for parsing Thrift.
  """

  @doc """
  Ensures that the argument is an atom.
  """
  def atomify(nil), do: nil
  def atomify(l) when is_list(l) do
    List.to_atom(l)
  end

  def cast(_, nil) do
    nil
  end

  def cast(type, %{} = val) do
    # this is for TEnumValues
    %{val | type: type}
  end

  def cast(:double, val) do
    val
  end

  def cast(:string, val) do
    List.to_string(val)
  end

  def cast({:set, type}, val) do
    MapSet.new(val, &cast(type, &1))
  end

  def cast({:map, {key_type, val_type}}, val) do
    Enum.into(val, %{}, fn {k, v} ->
      {cast(key_type, k), cast(val_type, v)}
    end)
  end

  def cast({:list, elem_type}, val) do
    Enum.map(val, fn elem ->
      cast(elem_type, elem)
    end)
  end

  def cast(_, val) do
    val
  end
end
