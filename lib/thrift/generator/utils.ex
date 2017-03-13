defmodule Thrift.Generator.Utils do
  @moduledoc """
  Collection of utilities for working with generated code.
  """

  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    Constant,
    Field,
    Struct,
    Schema,
    TypeRef,
    TEnum,
    ValueRef,
  }

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
  @spec merge_blocks(Macro.t) :: Macro.t
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
  @spec sort_defs(Macro.t) :: Macro.t
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

  # NOTE
  # this is basically the same as Macro.underscore/1, but this version properly
  # handles SCREAMING_SNAKE_CASE strings
  # Macro.underscore/1 has been fixed upstream in the Elixir source
  @spec underscore(binary) :: binary
  def underscore(s) when is_binary(s) do
    s
    |> String.split("_")
    |> Enum.map(&Macro.underscore/1)
    |> Enum.join("_")
  end

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
  @spec optimize_iolist(list) :: iodata
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

  @doc """
  Returns a quoted value, resolving it in the schema if necessary

  The values returned by this function are suitable for unquoting.
  """
  @spec quote_value(term, term, Schema.t) :: Macro.t
  def quote_value(%ValueRef{} = ref, type, schema) do
    value = resolve!(schema.file_group, ref)
    quote_value(value, type, schema)
  end
  def quote_value(value, %TypeRef{} = ref, schema) do
    type = resolve!(schema.file_group, ref)
    quote_value(value, type, schema)
  end
  def quote_value(%Constant{type: %TypeRef{} = ref} = constant, type, schema) do
    constant = %Constant{constant | type: resolve!(schema.file_group, ref)}
    quote_value(constant, type, schema)
  end
  def quote_value(%Constant{type: type, value: value}, type, schema) do
    quote_value(value, type, schema)
  end
  def quote_value(value, %TEnum{} = enum, _schema) when is_integer(value) do
    value_found = Enum.any?(enum.values, fn
      {_, ^value} -> true
      {_, _value} -> false
    end)
    unless value_found do
      raise RuntimeError,
        message: "Default value #{inspect value} is not a member of enum #{enum.name}"
    end
    value
  end
  def quote_value(nil, _type, _schema) do
    nil
  end
  def quote_value(value, :bool, _schema) when is_boolean(value), do: value
  def quote_value(value, :byte, _schema) when is_integer(value), do: value
  def quote_value(value, :double, _schema) when is_number(value), do: value
  def quote_value(value, :i8, _schema) when is_integer(value), do: value
  def quote_value(value, :i16, _schema) when is_integer(value), do: value
  def quote_value(value, :i32, _schema) when is_integer(value), do: value
  def quote_value(value, :i64, _schema) when is_integer(value), do: value
  def quote_value(value, :string, _schema) when is_binary(value), do: value
  def quote_value(value, :string, _schema) when is_list(value) do
    List.to_string(value)
  end
  def quote_value(value, :binary, schema) do
    quote_value(value, :string, schema)
  end
  def quote_value(values, %Struct{fields: fields} = struct, schema) when is_list(values) do
    struct_module = FileGroup.dest_module(schema.file_group, struct)
    types = Map.new(fields, fn %Field{name: name, type: type} ->
      {name, type}
    end)
    defaults = Enum.map(fields, fn %Field{name: name, type: type, default: default} ->
      {name, quote_value(default, type, schema)}
    end)
    values = Enum.map(values, fn {name_charlist, value} ->
      name = List.to_string(name_charlist) |> String.to_existing_atom
      type = Map.fetch!(types, name)
      {name, quote_value(value, type, schema)}
    end)
    quote do
      %{unquote_splicing([{:__struct__, struct_module} | Keyword.new(defaults ++ values)])}
    end
  end
  def quote_value(value, {:map, {key_type, value_type}}, schema) when is_list(value) do
    quote_value(Enum.into(value, %{}), {:map, {key_type, value_type}}, schema)
  end
  def quote_value(%{} = value, {:map, {key_type, value_type}}, schema) do
    Map.new(value, fn {key, value} ->
      {
        quote_value(key, key_type, schema),
        quote_value(value, value_type, schema),
      }
    end)
  end
  def quote_value(%MapSet{} = set, {:set, type}, schema) do
    values = Enum.map(set, &quote_value(&1, type, schema))
    quote do
      MapSet.new(unquote(values))
    end
  end
  def quote_value(list, {:list, type}, schema) when is_list(list) do
    Enum.map(list, &quote_value(&1, type, schema))
  end

  defp resolve!(file_group, ref) do
    case FileGroup.resolve(file_group, ref) do
      nil ->
        resolution_failed(ref)
      other ->
        other
    end
  end

  defp resolution_failed(%TypeRef{} = ref) do
    raise "Fatal error: Could not find type: #{ref.referenced_type}"
  end
  defp resolution_failed(%ValueRef{} = ref) do
    raise "Fatal error: Could not find value: #{ref.referenced_value}"
  end
end
