defmodule Thrift.Protocol.Compact.Type do
  @moduledoc false

  @typedoc "Compact protocol field type identifier"
  @type t :: 1 | 2 | 3 | 4 | 6 | 7 | 8 | 10 | 11 | 12

  defmacro true_bool, do: 1
  defmacro contained_bool, do: 1
  defmacro false_bool, do: 2
  defmacro byte, do: 3
  defmacro i16, do: 4
  defmacro i32, do: 5
  defmacro i64, do: 6
  defmacro double, do: 7
  defmacro string, do: 8
  defmacro list, do: 9
  defmacro set, do: 10
  defmacro map, do: 11
  defmacro struct, do: 12

  @type data_type :: Thrift.data_type() | {:bool, true} | {:bool, false} | :contained_bool

  @spec of(data_type()) :: t
  def of({:bool, true}), do: true_bool()
  def of({:bool, false}), do: false_bool()
  def of(:contained_bool), do: contained_bool()
  def of(:byte), do: byte()
  def of(:i8), do: byte()
  def of(:i16), do: i16()
  def of(:i32), do: i32()
  def of(:i64), do: i64()
  def of(:double), do: double()
  def of(:binary), do: string()
  def of(:string), do: string()
  def of({:map, _}), do: map()
  def of({:set, _}), do: set()
  def of({:list, _}), do: list()
end
