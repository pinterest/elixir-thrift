defmodule Thrift.Protocol do
  @moduledoc false

  @doc """
  Generate quoted expression for implementing SerDe protocol for a struct.
  """
  @callback serde_impl(module, Thrift.AST.Struct.t, Thrift.FileGroup.t) :: Macro.expr
end
