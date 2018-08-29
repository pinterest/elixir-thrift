defmodule Calculator.ServiceHandler do
  @moduledoc false
  @behaviour Calculator.Generated.Service.Handler

  alias Calculator.Generated.Vector
  alias Calculator.Generated.VectorProductResult
  alias Calculator.Generated.VectorProductType

  @impl true
  def add(left, right) do
    left + right
  end

  @impl true
  def subtract(left, right) do
    left - right
  end

  @impl true
  def multiply(left, right) do
    left * right
  end

  @impl true
  def divide(_left, 0) do
    raise Calculator.Generated.DivideByZeroError, message: "Cannot divide by zero"
  end

  def divide(left, right) do
    div(left, right)
  end

  @impl true
  def vector_product(left, right, type) do
    case VectorProductType.value_to_name!(type) do
      :dot_product ->
        %VectorProductResult{scalar: dot_product(left, right)}

      :cross_product ->
        %VectorProductResult{vector: cross_product(left, right)}
    end
  end

  defp dot_product(%Vector{} = left, %Vector{} = right) do
    left.x * right.x + left.y * right.y + left.z * right.z
  end

  defp cross_product(%Vector{} = left, %Vector{} = right) do
    %Vector{
      x: left.y * right.z - left.z * right.y,
      y: left.z * right.x - left.x * right.z,
      z: left.x * right.y - left.y * right.x
    }
  end
end
