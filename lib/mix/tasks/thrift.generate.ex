defmodule Mix.Tasks.Thrift.Generate do
  use Mix.Task

  @moduledoc """
  Generate Elixir modules from Thrift schema definitions.

  Syntax:
    mix thrift.generate [options] myfile.thrift
    mix thrift.generate [options] dir_with_thrift_files

  ## Command line options
    * `--output-dir` - Directory under which to place generated .ex files. (Default: ./lib)
  """

  def run(args) do
    {opts, args, _} = OptionParser.parse(args)
    if args == [] do
      print_help
      exit :normal
    end
    [input | _] = args
    output_dir = Keyword.get(opts, :output_dir, "lib")

    thrift_files = cond do
      File.dir?(input) ->
        Mix.Utils.extract_files([input], "*.thrift")
      File.regular?(input) ->
        [input]
    end

    for thrift_file <- thrift_files do
      for output_file <- Thrift.Generator.generate!(thrift_file, output_dir) do
        Mix.shell.info "Generated #{output_file}"
      end
    end
  end

  defp print_help do
    Mix.Task.moduledoc(__MODULE__)
    |> Mix.shell.info
  end
end
