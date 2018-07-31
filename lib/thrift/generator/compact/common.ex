defmodule Thrift.Generator.Compact.Common do
  @moduledoc """
  Functions used by `Thrift.Generator.Compact.StructSerialize` and
  `Thrift.Generator.Compact.StructDeserialize`

  """

  alias Thrift.AST.{
    Exception,
    Struct,
    TEnum,
    TypeRef,
    Union
  }

  alias Thrift.Parser.FileGroup

  require Thrift.Protocol.Compact.Type, as: Type

  @doc """
  Rejects all void fields, and sorts in order of
  field id
  """
  @spec reject_void_and_sort_fields(list()) :: list()
  def reject_void_and_sort_fields(fields) do
    fields
    |> Enum.reject(&(&1.type == :void))
    |> Enum.sort_by(& &1.id)
  end

  @doc """
  Type id of a map, set, or list element.
  """
  @spec contained_type_id(TypeRef.t() | Type.data_type(), FileGroup.t()) :: Type.t()
  def contained_type_id(%TypeRef{referenced_type: type}, file_group) do
    file_group
    |> FileGroup.resolve(type)
    |> contained_type_id(file_group)
  end

  def contained_type_id(:bool, _file_group), do: 1
  def contained_type_id(type, file_group), do: type_id(type, file_group)

  @doc """
  Type id of a struct field.
  """
  @spec type_id(any) :: Type.t()
  def type_id(type) do
    type_id(type, nil)
  end

  @spec type_id(any) :: Type.t()
  def type_id(%TypeRef{referenced_type: type}, file_group) do
    resolved = FileGroup.resolve(file_group, type)
    type_id(resolved, file_group)
  end

  def type_id(%TEnum{}, _), do: Type.i32()
  def type_id(%Struct{}, _), do: Type.struct()
  def type_id(%Exception{}, _), do: Type.struct()
  def type_id(%Union{}, _), do: Type.struct()
  def type_id(type, _file_group), do: Type.of(type)

  @doc """
  Boolean true (as a list, set, or map element)
  """
  def contained_true, do: 1

  @doc """
  Boolean false (as a list, set, or map element)
  """
  def contained_false, do: 2
end
