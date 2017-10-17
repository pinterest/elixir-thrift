defmodule Thrift.Parser.FileGroupTest do
  use ExUnit.Case
  use ThriftTestHelpers

  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.Constant

  test "constant module uses suitable existing name" do
    with_thrift_files([
      "myservice.thrift": """
      const double PI = 3.14
      service MyService {}
      """, as: :file_group, parse: "myservice.thrift"]) do

      assert :"Elixir.MyService" == FileGroup.dest_module(file_group, Constant)
    end
  end
end
