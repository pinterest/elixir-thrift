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
  @spec merge_blocks(list) :: list
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
  @spec sort_defs(list) :: list
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

  @spec underscore(atom) :: atom
  def underscore(a) when is_atom(a) do
    a |> Atom.to_string |> underscore |> String.to_atom
  end

  @spec underscore(bitstring) :: bitstring
  def underscore(s) when is_bitstring(s), do: Macro.underscore(s)

  # Change this to true to see iolist optimizations as they are applied.
  @debug_optimization false
  defmacrop debug_optimization(expr, label) do
    if @debug_optimization do
      quote do
        unquote(expr) |> Macro.to_string |> IO.puts
        IO.puts unquote(label)
      end
    else
      quote do
        _ = unquote(expr)
      end
    end
  end

  @doc false
  #
  # Optimize a quoted expression that returns an iolist. The high level strategy
  # is to flatten lists and combine adjacent binaries.
  #
  @spec optimize_iolist(list) :: any
  def optimize_iolist([[a | b] | c] = expr) do
    debug_optimization(expr, "flatten list")
    optimize_iolist([a, b | c])
  end
  def optimize_iolist([a, [b | c] | d] = expr) do
    debug_optimization(expr, "flatten list")
    optimize_iolist([a, b, c | d])
  end
  def optimize_iolist([[] | a] = expr) do
    debug_optimization(expr, "discard empty list")
    optimize_iolist(a)
  end
  def optimize_iolist([a, [] | b] = expr) do
    debug_optimization(expr, "discard empty list")
    optimize_iolist([a | b])
  end
  def optimize_iolist([a, b, [] | c] = expr) do
    debug_optimization(expr, "discard empty list")
    optimize_iolist([a, b | c])
  end
  def optimize_iolist([{:|, _, a} | b] = expr) do
    debug_optimization(expr, "extract final iolist cell")
    optimize_iolist([a | b])
  end
  def optimize_iolist([a, {:|, _, b} | c] = expr) do
    debug_optimization(expr, "extract final iolist cell")
    optimize_iolist([a, b | c])
  end
  def optimize_iolist([{:<<>>, opts, a}, {:<<>>, _, b} | rest] = expr) do
    debug_optimization(expr, "merge binary expressions")
    optimize_iolist([{:<<>>, opts, a ++ b} | rest])
  end
  def optimize_iolist([a, {:<<>>, opts, b} | rest] = expr) when is_binary(a) do
    debug_optimization(expr, "merge binary and binary expression")
    optimize_iolist([{:<<>>, opts, [a | b]} | rest])
  end
  def optimize_iolist([{:<<>>, opts, a}, b | rest] = expr) when is_binary(b) do
    debug_optimization(expr, "merge binary expression and binary")
    optimize_iolist([{:<<>>, opts, a ++ [b]} | rest])
  end
  def optimize_iolist([a, b | rest] = expr) when is_binary(a) and is_binary(b) do
    debug_optimization(expr, "merge binaries")
    optimize_iolist([a <> b | rest])
  end
  def optimize_iolist([a, b] = expr) do
    debug_optimization(expr, "final cons cell")
    [{:|, [], [a, b]}]
  end
  def optimize_iolist([a] = expr) do
    debug_optimization(expr, "unwrap single element")
    a
  end
  def optimize_iolist([a | rest] = expr) do
    debug_optimization(expr, "skip element")
    expr = case optimize_iolist(rest) do
      b when is_list(b) -> [a | b]
      b -> [a, b]
    end
    debug_optimization(expr, "recombined skipped element")
    expr
  end
  def optimize_iolist(expr) do
    debug_optimization(expr, "done")
    expr
  end
end
