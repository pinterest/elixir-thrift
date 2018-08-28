defmodule Calculator.ServiceHandler do
  @moduledoc false
  @behaviour Calculator.Generated.Service.Handler

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
  def divide(left, right) do
    case right do
      0 ->
        raise Calculator.Generated.DivideByZeroError, message: "Cannot divide by zero"

      _ ->
        div(left, right)
    end
  end
end
