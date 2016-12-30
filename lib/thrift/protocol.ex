defmodule Thrift.Protocol do
  @moduledoc """
  This module contains high-level Thrift protocol types.
  """

  @typedoc "Thrift data types"
  @type data_type ::
    :bool | :byte | :i16 | :i32 | :i64 | :double |
    :string | :struct | :map | :set | :list

  @typedoc "Thrift message types"
  @type message_type :: :call | :reply | :exception | :oneway
end
