defmodule Mix.Tasks.Thrift.Generate do
  use Mix.Task

  @moduledoc """
  Generate Elixir modules from Thrift schema definitions.

  ## Command line options

    * `--thrift-dir` - Directory to scan for .thrift files. (Default: ./thrift)
    * `--lib-dir` - Directory under which to place generated .ex files. (Default: ./lib)
  """

  def run(args) do
    {opts, _, _} = OptionParser.parse(args)

    thrift_dir = Keyword.get(opts, :thrift_dir, "thrift")
    lib_dir = Keyword.get(opts, :lib_dir, "lib")

    for thrift_file <- Mix.Utils.extract_files([thrift_dir], "*.thrift") do
      for output_file <- Thrift.Generator.Models.generate!(thrift_file, lib_dir) do
        Mix.shell.info "Generated #{output_file}"
      end
    end
  end
end
