defmodule Thrift.Parser.ParsedFile do
  @moduledoc false

  alias Thrift.AST.Schema
  alias Thrift.Parser
  alias Thrift.Parser.FileRef

  @type t :: %__MODULE__{file_ref: %FileRef{}, schema: %Schema{}, name: String.t()}
  defstruct file_ref: nil, schema: nil, name: nil

  def new(%FileRef{} = file_ref) do
    case Parser.parse(file_ref.contents) do
      {:ok, schema} ->
        %__MODULE__{file_ref: file_ref, schema: schema, name: FileRef.include_name(file_ref.path)}

      {:error, error} ->
        raise Thrift.FileParseError, {file_ref, error}
    end
  end
end
