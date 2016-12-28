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
  Optimize an expression that returns an iolist.

    [<<1>>, <<2>>] => <<1, 2>>
  """
  def optimize_iolist([a | rest]) when is_list(a) do
    optimize_iolist(a ++ rest)
  end
  def optimize_iolist([a, b | rest]) when is_list(b) do
    optimize_iolist([a] ++ b ++ rest)
  end
  def optimize_iolist([{:<<>>, opts, a}, {:<<>>, _, b} | rest]) do
    optimize_iolist([{:<<>>, opts, a ++ b} | rest])
  end
  def optimize_iolist([a, {:<<>>, opts, b} | rest]) when is_binary(a) do
    optimize_iolist([{:<<>>, opts, [a | b]} | rest])
  end
  def optimize_iolist([{:<<>>, opts, a}, b | rest]) when is_binary(b) do
    optimize_iolist([{:<<>>, opts, a ++ [b]} | rest])
  end
  def optimize_iolist([{:<<>>, opts, a}, <<0>> | rest]) do
    optimize_iolist([{:<<>>, opts, a ++ [0]} | rest])
  end
  def optimize_iolist([{:|, _, [a, b]} | rest]) do
    optimize_iolist([a, b | rest])
  end
  def optimize_iolist([a, b]) do
    [{:|, [], [a, b]}]
  end
  def optimize_iolist([binary]) do
    binary
  end
  def optimize_iolist([a | rest]) do
    [optimize_iolist(a) | optimize_iolist(rest)]
  end
  def optimize_iolist({:<<>>, _, _} = binary) do
    binary
  end
  def optimize_iolist(binary) when is_binary(binary) do
    binary
  end
  def optimize_iolist(other) do
    other
  end
end
