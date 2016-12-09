defmodule Thrift.Parser.Shell do
  @moduledoc """
  Utility for writing output to the shell.
  """

  @yellow IO.ANSI.yellow
  @red IO.ANSI.red
  @reset IO.ANSI.reset

  @doc """
  Emits a warning to the shell in a yellow color
  """
  def warn(msg) do
    IO.puts "#{@yellow}#{msg}#{@reset}"
  end

  @doc """
  Emits an error to the shell in a red color
  """
  def error(msg) do
    IO.puts "#{@red}#{msg}#{@reset}"
  end
end
