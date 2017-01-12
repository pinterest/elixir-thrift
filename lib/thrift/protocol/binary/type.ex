defmodule Thrift.Protocol.Binary.Type do
  @moduledoc false

  @typedoc "Binary protocol field type identifier"
  @type t :: 2 | 3 | 4 | 6 | 8 | 10 | 11 | 12 | 13 | 14 | 15

  defmacro bool,    do: 2
  defmacro byte,    do: 3
  defmacro double,  do: 4
  defmacro i8,      do: 3
  defmacro i16,     do: 6
  defmacro i32,     do: 8
  defmacro i64,     do: 10
  defmacro string,  do: 11
  defmacro struct,  do: 12
  defmacro map,     do: 13
  defmacro set,     do: 14
  defmacro list,    do: 15
end

