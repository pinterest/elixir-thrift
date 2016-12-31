defmodule Thrift do
  @moduledoc ~S"""
  Thrift provides [Apache Thrift](http://thrift.apache.org/) support:

    * `Mix.Tasks.Compile.Thrift` - mix task for generate Erlang source files
      from `.thrift` schema files

    * `Thrift.Parser` - functions for parsing `.thift` schema files
  """

  @typedoc "Thrift data types"
  @type data_type ::
    :bool | :byte | :i16 | :i32 | :i64 | :double |
    :string | :struct | :union | :map | :set | :list

  @type i8   :: (-128..127)
  @type i16 :: (-32_768..32_767)
  @type i32 :: (-2_147_483_648..2_147_483_647)
  @type i64 :: (-9_223_372_036_854_775_808..9_223_372_036_854_775_807)
  @type double :: float()

  @typedoc "Thrift message types"
  @type message_type :: :call | :reply | :exception | :oneway
end
