defmodule Thrift.Parser do
  @moduledoc """
  This module provides functions for parsing [Thrift IDL][idl] files
  (`.thrift`).

  [idl]: https://thrift.apache.org/docs/idl
  """

  alias Thrift.Parser.{FileGroup, FileRef, ParsedFile}

  @typedoc "A Thrift IDL line number"
  @type line :: pos_integer | nil

  @typedoc "A map of Thrift annotation keys to values"
  @type annotations :: %{required(String.t()) => String.t()}

  @typedoc "Available parser options"
  @type opt ::
          {:include_paths, [Path.t()]}
          | {:namespace, module | String.t()}
  @type opts :: [opt]

  @doc """
  Parses a Thrift IDL string into its AST representation.
  """
  @spec parse(String.t()) ::
          {:ok, Thrift.AST.Schema.t()} | {:error, {line(), message :: String.t()}}
  def parse(doc) do
    doc = String.to_charlist(doc)

    with {:ok, tokens, _} <- :thrift_lexer.string(doc),
         {:ok, _} = result <- :thrift_parser.parse(tokens) do
      result
    else
      {:error, {line, :thrift_lexer, error}, _} ->
        {:error, {line, List.to_string(:thrift_lexer.format_error(error))}}

      {:error, {line, :thrift_parser, error}} ->
        {:error, {line, List.to_string(:thrift_parser.format_error(error))}}
    end
  end

  @doc """
  Parses a Thrift IDL file.

  In addition to the given file, all included files are also parsed and
  returned as part of the resulting `Thrift.Parser.FileGroup`.
  """
  @spec parse_file(Path.t(), opts) :: FileGroup.t()
  def parse_file(file_path, opts \\ []) do
    normalized_opts = normalize_opts(opts)

    parsed_file =
      file_path
      |> FileRef.new()
      |> ParsedFile.new()

    mod_name =
      file_path
      |> Path.basename()
      |> Path.rootname()
      |> String.to_atom()

    FileGroup.new(file_path, normalized_opts)
    |> FileGroup.add(parsed_file)
    |> FileGroup.set_current_module(mod_name)
  end

  # normalize various type permutations that we could get options as
  defp normalize_opts(opts) do
    Keyword.update(opts, :namespace, nil, &namespace_string/1)
  end

  # namespace can be an atom or a binary
  #   - convert an atom to a binary and remove the "Elixir." we get from atoms
  #      like `Foo`
  #   - make sure values are valid module names (CamelCase)
  defp namespace_string(""), do: nil
  defp namespace_string(nil), do: nil
  defp namespace_string(b) when is_binary(b), do: Macro.camelize(b)

  defp namespace_string(a) when is_atom(a) do
    a
    |> Atom.to_string()
    |> String.trim_leading("Elixir.")
    |> namespace_string
  end
end
