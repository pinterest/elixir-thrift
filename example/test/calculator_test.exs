defmodule CalculatorTest do
  use ExUnit.Case

  alias Calculator.Generated.DivideByZeroError
  alias Calculator.Generated.Service.Binary.Framed.Client

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
end
