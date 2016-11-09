defmodule Thrift.Parser.ParsedFile do
  alias Thrift.Parser.Models.Schema
  alias Thrift.Parser
  alias Thrift.Parser.FileRef

  @type t :: %__MODULE__{file_ref: %FileRef{}, schema: %Schema{}, name: String.t}
  defstruct file_ref: nil, schema: nil, name: nil

  def new(file_ref=%FileRef{}) do
    %__MODULE__{file_ref: file_ref,
                schema: Parser.parse(file_ref.contents),
                name: FileRef.include_name(file_ref.path)
               }
  end
end
