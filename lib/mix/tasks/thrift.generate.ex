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
      thrift_file
      |> Thrift.Generator.Models.generate
      |> Enum.each(fn {model_file, source} ->
        path = Path.join(lib_dir, model_file)
        path |> Path.dirname |> File.mkdir_p!
        path |> File.write!(source)
        Mix.shell.info "Generated #{path}"
      end)
    end
  end
end
