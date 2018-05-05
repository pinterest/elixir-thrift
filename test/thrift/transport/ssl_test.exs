defmodule Thrift.Transport.SSLTest do
  use ExUnit.Case, async: true

  alias Thrift.Transport.SSL

  describe "configuration/1" do
    test "handles configure/0 errors" do
      error = RuntimeError.exception("test")
      assert {:error, error} == SSL.configuration([enabled: true, configure: fn -> {:error, error} end])
    end
  end
end
