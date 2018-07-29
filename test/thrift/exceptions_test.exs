defmodule Thrift.ExceptionsTest do
  use ExUnit.Case, async: true

  describe "ConnectionError" do
    alias Thrift.ConnectionError

    test "formats :closed" do
      exception = ConnectionError.exception(reason: :closed)
      assert Exception.message(exception) == "Connection error: closed"
    end

    test "formats :timeout" do
      exception = ConnectionError.exception(reason: :timeout)
      assert Exception.message(exception) == "Connection error: timeout"
    end

    test "formats POSIX errors" do
      exception = ConnectionError.exception(reason: :econnrefused)
      assert Exception.message(exception) == "Connection error: connection refused (econnrefused)"
    end
  end

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
      assert %TApplicationException{type: :unknown} = TApplicationException.exception(type: 1000)
    end

    test "raises for unknown atom type values" do
      assert_raise FunctionClauseError, fn ->
        TApplicationException.exception(type: :bogus)
      end
    end

    test "raises for missing atom type values" do
      assert_raise KeyError, fn ->
        TApplicationException.exception(Keyword.new())
      end
    end

    test "accepts a :message argument" do
      exception = TApplicationException.exception(type: :unknown, message: "Message Text")
      assert exception.message == "Message Text"
      assert Exception.message(exception) == "Message Text"
    end

    test "message defaults to the :type string" do
      exception = TApplicationException.exception(type: :invalid_protocol)
      assert exception.message == "invalid_protocol"
      assert Exception.message(exception) == "invalid_protocol"
    end

    test "can convert valid atom type values to integer type values" do
      assert 9 == TApplicationException.type_id(:invalid_protocol)

      assert_raise FunctionClauseError, fn ->
        TApplicationException.type_id(:bogus)
      end
    end
  end
end
