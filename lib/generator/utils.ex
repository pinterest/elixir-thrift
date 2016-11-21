defmodule Thrift.Generator.Utils do
  def merge_blocks([{:__block__, _, contents} | rest]) do
    merge_blocks(contents) ++ merge_blocks(rest)
  end

  def merge_blocks([statement | rest]) do
    [statement | merge_blocks(rest)]
  end

  def merge_blocks([]) do
    []
  end

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
end
