defmodule Thrift.Parser.FileGroupTest do
  use ExUnit.Case
  use ThriftTestHelpers

  alias Thrift.AST.Constant
  alias Thrift.Parser.FileGroup

  test "constant module uses suitable existing name" do
    with_thrift_files([
      "myservice.thrift": """
      const double PI = 3.14
      service MyService {}
      """, as: :file_group, parse: "myservice.thrift"]) do

      assert :"Elixir.MyService" == FileGroup.dest_module(file_group, Constant)
    end
  end

  test "destination module supports input names in various casings" do
    file_group = FileGroup.new("casing.thrift")
    assert :"Elixir.UPPERCASE" == FileGroup.dest_module(file_group, :"module.UPPERCASE")
    assert :"Elixir.Lowercase" == FileGroup.dest_module(file_group, :"module.lowercase")
    assert :"Elixir.CamelCase" == FileGroup.dest_module(file_group, :"module.CamelCase")
  end
end
