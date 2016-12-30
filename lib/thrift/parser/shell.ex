defmodule Thrift.Parser.Shell do
  @moduledoc false

  @yellow IO.ANSI.yellow
  @red IO.ANSI.red
  @reset IO.ANSI.reset

  def warn(msg) do
    IO.puts "#{@yellow}#{msg}#{@reset}"
  end

  def error(msg) do
    IO.puts "#{@red}#{msg}#{@reset}"
  end
end
