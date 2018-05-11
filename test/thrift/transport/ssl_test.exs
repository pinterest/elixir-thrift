defmodule Thrift.Transport.SSLTest do
  use ExUnit.Case, async: true

  alias Thrift.Transport.SSL

  describe "configuration/1" do
    test "handles configure/0 errors" do
      error = RuntimeError.exception("test")
      assert {:error, error} == SSL.configuration([enabled: true, configure: fn -> {:error, error} end])
    end
  end

  describe "optional?/1" do
    test "does the right thing" do
      assert {true, [certfile: "/some/path"]} = SSL.optional?([certfile: "/some/path", optional: true])
      assert {false, [certfile: "/some/path"]} = SSL.optional?([certfile: "/some/path", optional: false])
    end
  end
end
