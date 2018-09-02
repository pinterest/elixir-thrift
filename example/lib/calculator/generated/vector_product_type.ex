defmodule(Calculator.Generated.VectorProductType) do
  @moduledoc(false)
  defmacro(unquote(:dot_product)()) do
    1
  end
  defmacro(unquote(:cross_product)()) do
    2
  end
  def(value_to_name(1)) do
    {:ok, :dot_product}
  end
  def(value_to_name(2)) do
    {:ok, :cross_product}
  end
  def(value_to_name(v)) do
    {:error, {:invalid_enum_value, v}}
  end
  def(name_to_value(:dot_product)) do
    {:ok, 1}
  end
  def(name_to_value(:cross_product)) do
    {:ok, 2}
  end
  def(name_to_value(k)) do
    {:error, {:invalid_enum_name, k}}
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
  def(member?(1)) do
    true
  end
  def(member?(2)) do
    true
  end
  def(member?(_)) do
    false
  end
  def(name?(:dot_product)) do
    true
  end
  def(name?(:cross_product)) do
    true
  end
  def(name?(_)) do
    false
  end
end