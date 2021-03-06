defmodule Thrift.Parser do
  @moduledoc """
  This module provides functions for parsing [Thrift IDL][idl] (`.thrift`)
  files.

  [idl]: https://thrift.apache.org/docs/idl
  """

  alias Thrift.Parser.FileGroup

  @typedoc "A Thrift IDL line number"
  @type line :: pos_integer | nil

  @typedoc "A map of Thrift annotation keys to values"
  @type annotations :: %{required(String.t()) => String.t()}

  @typedoc "Available parser options"
  @type opt ::
          {:include_paths, [Path.t()]}
          | {:namespace, module | String.t()}
  @type opts :: [opt]

  @typedoc "Parse error (path, line, message)"
  @type error :: {Path.t() | nil, line(), message :: String.t()}

  @doc """
  Parses a Thrift IDL string into its AST representation.
  """
  @spec parse_string(String.t()) :: {:ok, Thrift.AST.Schema.t()} | {:error, error}
  def parse_string(doc) do
    doc = String.to_charlist(doc)

    with {:ok, tokens, _} <- :thrift_lexer.string(doc),
         {:ok, _} = result <- :thrift_parser.parse(tokens) do
      result
    else
      {:error, {line, :thrift_lexer, error}, _} ->
        {:error, {nil, line, List.to_string(:thrift_lexer.format_error(error))}}

      {:error, {line, :thrift_parser, error}} ->
        {:error, {nil, line, List.to_string(:thrift_parser.format_error(error))}}
    end
  end

  @doc """
  Parses a Thrift IDL file into its AST representation.
  """
  @spec parse_file(Path.t()) :: {:ok, Thrift.AST.Schema.t()} | {:error, error}
  def parse_file(path) do
    with {:ok, contents} <- read_file(path),
         {:ok, _schema} = result <- parse_string(contents) do
      result
    else
      {:error, {nil, line, message}} ->
        {:error, {path, line, message}}

      {:error, message} ->
        {:error, {path, nil, message}}
    end
  end

  @doc """
  Parses a Thrift IDL file and its included files into a file group.
  """
  @spec parse_file_group(Path.t(), opts) :: {:ok, FileGroup.t()} | {:error, [error, ...]}
  def parse_file_group(path, opts \\ []) do
    group = FileGroup.new(path, normalize_opts(opts))

    with {:ok, schema} <- parse_file(path),
         {group, [] = _errors} <- FileGroup.add(group, path, schema) do
      {:ok, FileGroup.set_current_module(group, module_name(path))}
    else
      {:error, error} ->
        {:error, [error]}

      {%FileGroup{}, errors} ->
        {:error, Enum.reverse(errors)}
    end
  end

  @doc """
  Parses a Thrift IDL file and its included files into a file group.

  A `Thrift.FileParseError` will be raised if an error occurs.
  """
  @spec parse_file_group!(Path.t(), opts) :: FileGroup.t()
  def parse_file_group!(path, opts \\ []) do
    case parse_file_group(path, opts) do
      {:ok, group} ->
        group

      {:error, [first_error | _errors]} ->
        raise Thrift.FileParseError, first_error
    end
  end

  defp read_file(path) do
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

  defp module_name(path) do
    path
    |> Path.basename()
    |> Path.rootname()
    |> String.to_atom()
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
