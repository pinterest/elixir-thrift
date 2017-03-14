defmodule Thrift.Parser do
  @moduledoc """
  This module provides functions for parsing Thrift IDL files (`.thrift`).
  """

  alias Thrift.Parser.{FileGroup, FileRef, Models, ParsedFile}
  alias Thrift.Parser.Models.Schema

  @typedoc "A Thrift IDL line number"
  @type line :: pos_integer | nil

  @typedoc "A schema path element"
  @type path_element :: String.t | atom

  @typedoc "Available parser options"
  @type opt ::
    {:include_paths, [Path.t]} |
    {:namespace, String.t}
  @type opts :: [opt]

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
  @spec parse_file(Path.t, opts) :: FileGroup.t
  def parse_file(file_path, opts \\ []) do
    normalized_opts = normalize_opts(opts)

    parsed_file = file_path
    |> FileRef.new
    |> ParsedFile.new

    mod_name = file_path
    |> Path.basename
    |> Path.rootname
    |> String.to_atom

    FileGroup.new(file_path, normalized_opts)
    |> FileGroup.add(parsed_file)
    |> FileGroup.set_current_module(mod_name)
  end

  # normalize various type permutations that we could get options as
  defp normalize_opts(opts) do
    opts
    |> Keyword.update(:namespace, nil, &namespace_string/1)
  end

  # namespace can be an atom or a binary
  #   - convert an atom to a binary and remove the "Elixir." we get from atoms
  #      like `Foo`
  #   - make sure values are valid module names (CamelCase)
  defp namespace_string(nil), do: nil
  defp namespace_string(b) when is_binary(b), do: Macro.camelize(b)
  defp namespace_string(a) when is_atom(a) do
    a
    |> Atom.to_string
    |> String.replace_leading("Elixir.", "")
    |> namespace_string
  end
end
