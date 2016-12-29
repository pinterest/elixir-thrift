defmodule Thrift do
  @moduledoc ~S"""
  Thrift provides [Apache Thrift](http://thrift.apache.org/) support:

    * `Mix.Tasks.Compile.Thrift` - mix task for generate Erlang source files
      from `.thrift` schema files

    * `Thrift.Parser` - functions for parsing `.thift` schema files
  """

  @type i8   :: (-128..127)
  @type double :: float()
  @type i16 :: (-32_768..32_767)
  @type i32 :: (-2_147_483_648..2_147_483_647)
  @type i64 :: (-9_223_372_036_854_775_808..9_223_372_036_854_775_807)
end
