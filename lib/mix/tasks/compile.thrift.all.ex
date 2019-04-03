defmodule Mix.Tasks.Compile.Thrift.All do
  use Mix.Task
  require Logger

  @impl Mix.Task
  @spec run(OptionParser.argv()) :: :ok
  def run(command_line_args) do
    {opts, _, _} = OptionParser.parse(command_line_args, switches: [verbose: :boolean])

    verbose = Keyword.get(opts, :verbose, false)

    input_files =
      Keyword.get(Mix.Project.config(), :thrift, [])
      |> Keyword.get(:files, [])

    for file <- input_files do
      if(verbose, do: Logger.info("Compiling thrift file: #{file}"))
      Mix.Tasks.Compile.Thrift.run([file])
      if(verbose, do: Logger.info("Generating thrift HTTP client for file: #{file}"))
      Mix.Tasks.Thrift.HttpClient.Generate.run([file])
    end

    :ok
  end
end
