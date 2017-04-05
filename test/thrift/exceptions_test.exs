defmodule Thrift.ExceptionsTest do
  use ExUnit.Case, async: true

  describe "TApplicationException" do
    alias Thrift.TApplicationException

    test "accepts integer type values" do
      assert %TApplicationException{type: :invalid_protocol} =
        TApplicationException.exception(type: 9)
    end

    test "accepts atom type values" do
      assert %TApplicationException{type: :unknown_method} =
        TApplicationException.exception(type: :unknown_method)
    end

    test "defaults to :unknown for unknown integer type values" do
      assert %TApplicationException{type: :unknown} =
        TApplicationException.exception(type: 1000)
    end

    test "raises for unknown atom type values" do
      assert_raise FunctionClauseError, fn ->
        TApplicationException.exception(type: :bogus)
      end
    end

    test "raises for missing atom type values" do
      assert_raise FunctionClauseError, fn ->
        TApplicationException.exception(Keyword.new())
      end
    end

    test "accepts a :message argument" do
      exception = TApplicationException.exception(type: :unknown, message: "Message Text")
      assert exception.message == "Message Text"
      assert Exception.message(exception) == "Message Text"
    end

    test "can convert valid atom type values to integer type values" do
      assert 9 == TApplicationException.type_id(:invalid_protocol)
      assert_raise FunctionClauseError, fn ->
        TApplicationException.type_id(:bogus)
      end
    end
  end
end
