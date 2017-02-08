defmodule Thrift.Parser do
  @moduledoc """
  This module provides functions for parsing Thrift IDL files (`.thrift`).
  """

  @typedoc "A schema path element"
  @type path_element :: String.t | atom

  alias Thrift.Parser.{FileGroup, FileRef, Models, ParsedFile}
  alias Thrift.Parser.Models.Schema

  @doc """
  Parses a Thrift document and returns the schema that it represents.
  """
  @spec parse(String.t) :: {:ok, Schema.t} | {:error, term}
  def parse(doc) do
    doc = String.to_char_list(doc)

    case :thrift_lexer.string(doc) do
      {:ok, tokens, _} -> :thrift_parser.parse(tokens)
      {:error, lexer_error1, lexer_error2} ->
        {:error, {lexer_error1, lexer_error2}}
    end
  end

  @doc """
  Parses a Thrift document and returns a component to the caller.

  The part of the Thrift document that's returned is determined by the `path`
  parameter. It works a lot like the `Kernel.get_in/2` function, which takes a
  map and can pull out nested pieces.

  For example, this makes it easy to get to a service definition:

      parse(doc, [:services, :MyService])

  Will return the "MyService" service.
  """
  @spec parse(String.t, [path_element, ...]) :: Models.all
  def parse(doc, path) do
    {:ok, schema} = parse(doc)

    Enum.reduce(path, schema, fn
      (_part, nil) ->
        nil
      (part, %{} = next) ->
        Map.get(next, part)
    end)
  end

  @doc """
  Parses a Thrift IDL file.

  In addition to the given file, all included files are also parsed and
  returned as part of the resulting `Thrift.Parser.FileGroup`.
  """
  @spec parse_file(Path.t, [Path.t]) :: FileGroup.t
  def parse_file(file_path, include_paths \\ []) do
    parsed_file = file_path
    |> FileRef.new
    |> ParsedFile.new

    FileGroup.new(file_path, include_paths)
    |> FileGroup.add(parsed_file)
    |> FileGroup.update_resolutions
  end
end
