defmodule Thrift.Generator.Utils do
  @moduledoc """
  Collection of utilities for working with generated code.
  """

  @doc """
  When nesting a quote with multiple defs into another quote, the defs end up
  wrapped in blocks. Take the following code, for example.

    foo_bar = quote do
      def foo, do: 1
      def bar, do: 2
    end
    quote do
      unquote(foo_bar)
      def baz, do: 3
    end

  This generates code like the following.

    (
      def foo, do: 1
      def bar, do: 2
    )
    def baz, do: 3

  Running it through merge_blocks turns it into this:

    def foo, do: 1
    def bar, do: 2
    def baz, do: 3
  """
  def merge_blocks([{:__block__, _, contents} | rest]) do
    merge_blocks(contents) ++ merge_blocks(rest)
  end
  def merge_blocks([statement | rest]) do
    [statement | merge_blocks(rest)]
  end
  def merge_blocks([]) do
    []
  end

  @doc """
  Sort a list of quoted def/defp function clauses by name and arity. When
  similar clauses are not grouped together, Elixir prints a warning.
  """
  def sort_defs(statements) do
    Enum.sort_by(statements, fn
      {:def, _, [{:when, _, [{name, _, args} | _]} | _]} ->
        {name, length(args)}
      {:defp, _, [{:when, _, [{name, _, args} | _]} | _]} ->
        {name, length(args)}
      {:def, _, [{name, _, args} | _]} ->
        {name, length(args)}
      {:defp, _, [{name, _, args} | _]} ->
        {name, length(args)}
      {:=, _, [{:_, _, _} | _]} ->
        nil
    end)
  end

  @doc """
  Merge the binaries in an iolist.

    ["a", "b", ["c", [var]]] => ["abc", var]
  """
  def merge_binaries([a | rest]) when is_list(a) do
    merge_binaries(a ++ rest)
  end
  def merge_binaries([a, b | rest]) when is_list(b) do
    merge_binaries([a] ++ b ++ rest)
  end
  def merge_binaries([{:<<>>, [], a}, {:<<>>, [], b} | rest]) do
    merge_binaries([{:<<>>, [], a ++ b} | rest])
  end
  def merge_binaries([a | rest]) do
    [a] ++ merge_binaries(rest)
  end
  def merge_binaries(a) do
    a
  end

  def simplify_iolist([{:<<>>, _, _}=binary]) do
    binary
  end
  def simplify_iolist(other) do
    other
  end
end
