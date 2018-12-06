defmodule Thrift.Parser.ParsedFile do
  @moduledoc false

  alias Thrift.AST.Schema
  alias Thrift.Parser

  @type t :: %__MODULE__{path: Path.t(), schema: %Schema{}, name: String.t()}
  defstruct path: nil, schema: nil, name: nil

  @spec new(Path.t()) :: t()
  def new(path) do
    with {:ok, contents} <- read(path),
         {:ok, schema} <- Parser.parse(contents) do
      %__MODULE__{
        path: path,
        schema: schema,
        name: Path.basename(path, ".thrift")
      }
    else
      {:error, error} ->
        raise Thrift.FileParseError, {path, error}
    end
  end

  defp read(path) do
    case File.read(path) do
      {:ok, contents} ->
        # We include the __file__ here to hack around the fact that leex and
        # yecc don't operate on files and lose the file info. This is relevant
        # because the filename is turned into the thrift module, and is
        # necessary for resolution.
        {:ok, contents <> "\n__file__ \"#{path}\""}

      {:error, reason} ->
        {:error, :file.format_error(reason)}
    end
  end
end
