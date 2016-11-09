defmodule Mix.Tasks.Generate.Thrift do
  use Mix.Task

  def run(args) do
    {opts, _, _} = OptionParser.parse(args)

    thrift_dir = Keyword.get(opts, :thrift_dir, "thrift")
    lib_dir = Keyword.get(opts, :lib_dir, "lib")

    for thrift_file <- list_thrift_files(thrift_dir) do
      thrift_file
      |> Thrift.Generator.Models.generate
      |> Enum.each(fn {model_file, source} ->
        path = Path.join(lib_dir, model_file)
        :ok = path |> Path.dirname |> File.mkdir_p!
        :ok = path |> File.write!(source)
        Mix.shell.info "Generated #{path}"
      end)
    end
  end

  defp list_thrift_files(dir) do
    dir
    |> File.ls!
    |> Enum.filter(fn file -> Path.extname(file) == ".thrift" end)
    |> Enum.map(&Path.join(dir, &1))
  end
end
