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

  @spec of(Thrift.data_type) :: t
  def of(:bool),      do: bool()
  def of(:byte),      do: byte()
  def of(:i16),       do: i16()
  def of(:i32),       do: i32()
  def of(:i64),       do: i64()
  def of(:double),    do: double()
  def of(:string),    do: string()
  def of(:struct),    do: struct()
  def of({:map, _}),  do: map()
  def of({:set, _}),  do: set()
  def of({:list, _}), do: list()
end

