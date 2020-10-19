defmodule(Calculator.Generated.VectorProductType) do
  @moduledoc false
  defmacro(unquote(:dot_product)()) do
    1
  end

  defmacro(unquote(:cross_product)()) do
    2
  end

  def(value_to_name(v)) do
    case(v) do
      1 ->
        {:ok, :dot_product}

      2 ->
        {:ok, :cross_product}

      _ ->
        {:error, {:invalid_enum_value, v}}
    end
  end

  def(name_to_value(k)) do
    case(k) do
      :dot_product ->
        {:ok, 1}

      :cross_product ->
        {:ok, 2}

      _ ->
        {:error, {:invalid_enum_name, k}}
    end
  end

  def(value_to_name!(value)) do
    {:ok, name} = value_to_name(value)
    name
  end

  def(name_to_value!(name)) do
    {:ok, value} = name_to_value(name)
    value
  end

  def(meta(:names)) do
    [:dot_product, :cross_product]
  end

  def(meta(:values)) do
    [1, 2]
  end

  def(member?(v)) do
    case(v) do
      1 ->
        true

      2 ->
        true

      _ ->
        false
    end
  end

  def(name?(k)) do
    case(k) do
      :dot_product ->
        true

      :cross_product ->
        true

      _ ->
        false
    end
  end
end
