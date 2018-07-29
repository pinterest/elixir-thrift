defmodule Thrift.Parser.Conversions do
  @moduledoc false

  @spec atomify(charlist | nil) :: atom
  def atomify(nil), do: nil

  def atomify(l) when is_list(l) do
    List.to_atom(l)
  end

  # convert a charlist to a snake_case atom
  #   e.g., 'FooBar', 'foo_bar', 'fooBar', and 'FOO_BAR'
  #   should all produce :foo_bar
  @spec atomic_snake(charlist | nil) :: atom
  def atomic_snake(nil), do: nil

  def atomic_snake(l) when is_list(l) do
    l
    |> List.to_string()
    |> String.split("_")
    |> Enum.map(&Macro.underscore/1)
    |> Enum.join("_")
    |> String.to_atom()
  end

  @spec cast(Thrift.data_type(), any) :: any
  def cast(_, nil) do
    nil
  end

  # We can't match a TypeRef because it would create a circular dependency.
  def cast(_, %{referenced_type: _} = ref) do
    ref
  end

  def cast(_, %{referenced_value: _} = ref) do
    ref
  end

  def cast(:double, val) do
    val
  end

  def cast(:bool, 0), do: false
  def cast(:bool, 1), do: true

  def cast(:string, val) when is_list(val) do
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
