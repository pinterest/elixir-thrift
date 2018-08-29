defmodule CalculatorTest do
  @moduledoc false
  use ExUnit.Case

  require Calculator.Generated.VectorProductType

  alias Calculator.Generated.DivideByZeroError
  alias Calculator.Generated.Service.Binary.Framed.Client
  alias Calculator.Generated.Vector
  alias Calculator.Generated.VectorProductResult
  alias Calculator.Generated.VectorProductType

  setup do
    port = Application.get_env(:calculator, :port, 9090)
    {:ok, client} = Client.start_link("localhost", port)
    %{client: client}
  end

  test "add", ctx do
    assert Client.add(ctx[:client], 10, 11) == {:ok, 21}
    assert Client.add!(ctx[:client], 10, 11) == 21

    assert_raise ArgumentError, fn ->
      Client.add(ctx[:client], 10, 1.23)
    end
  end

  test "subtract", ctx do
    assert Client.subtract(ctx[:client], 10, 11) == {:ok, -1}
    assert Client.subtract!(ctx[:client], 10, 11) == -1

    assert_raise ArgumentError, fn ->
      Client.subtract(ctx[:client], 10, 1.23)
    end
  end

  test "multiply", ctx do
    assert Client.multiply(ctx[:client], 10, 11) == {:ok, 110}
    assert Client.multiply!(ctx[:client], 10, 11) == 110

    assert_raise ArgumentError, fn ->
      Client.multiply(ctx[:client], 10, 1.23)
    end
  end

  test "divide", ctx do
    assert Client.divide(ctx[:client], 22, 7) == {:ok, 3}
    assert Client.divide!(ctx[:client], 22, 7) == 3

    assert_raise ArgumentError, fn ->
      Client.divide(ctx[:client], 22, 7.0)
    end

    assert {:error, {:exception, %DivideByZeroError{}}} = Client.divide(ctx[:client], 22, 0)

    assert_raise DivideByZeroError, fn ->
      Client.divide!(ctx[:client], 22, 0)
    end
  end

  test "dot product", ctx do
    left = %Vector{x: 1.0, y: 2.0, z: 5.0}
    right = %Vector{x: 3.0, y: 1.0, z: -1.0}
    type = VectorProductType.dot_product()

    assert Client.vector_product(ctx[:client], left, left, type) ==
             {:ok, %VectorProductResult{scalar: 30.0}}

    assert Client.vector_product(ctx[:client], left, right, type) ==
             {:ok, %VectorProductResult{scalar: 0.0}}

    assert Client.vector_product(ctx[:client], right, right, type) ==
             {:ok, %VectorProductResult{scalar: 11.0}}
  end

  test "cross product", ctx do
    i = %Vector{x: 1.0}
    j = %Vector{y: 1.0}
    k = %Vector{z: 1.0}
    type = VectorProductType.cross_product()

    assert Client.vector_product(ctx[:client], i, j, type) ==
             {:ok, %VectorProductResult{vector: k}}

    assert Client.vector_product(ctx[:client], j, k, type) ==
             {:ok, %VectorProductResult{vector: i}}

    assert Client.vector_product(ctx[:client], k, i, type) ==
             {:ok, %VectorProductResult{vector: j}}
  end
end
