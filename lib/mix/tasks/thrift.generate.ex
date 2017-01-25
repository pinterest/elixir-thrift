defmodule Mix.Tasks.Thrift.Generate do
  use Mix.Task

  @shortdoc "Generate Elixir source files from Thrift schema files"

  @moduledoc """
  Generate Elixir source files from Thrift schema files (`.thrift`).

  A list of files can be given after the task name in order to select the
  specific Thrift schema files to parse:

      mix thrift.generate file1.thrift file2.thrift

  ## Command line options

    * `-o` `--out` - set the output directory, overriding the `:output_path`
      configuration value
    * `-v` `--verbose` - enable verbose task logging

  ## Configuration

    * `:output_path` - output directory into which the generated Elixir
      source files will be generated. Defaults to `"lib"`.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args,
      aliases: [o: :out, v: :verbose],
      switches: [out: :string, verbose: :boolean])

    config      = Keyword.get(Mix.Project.config, :thrift, [])
    output_path = opts[:out] || Keyword.get(config, :output_path, "lib")

    unless Enum.empty?(files) do
      File.mkdir_p!(output_path)
      Enum.each(files, &generate!(&1, output_path, opts))
    end
  end

  defp parse!(thrift_file) do
    try do
      Thrift.Parser.parse_file(thrift_file)
    rescue
      e ->
        Mix.raise "#{thrift_file}: #{Exception.message(e)}"
    end
  end

  defp generate!(thrift_file, output_path, opts) do
    Mix.shell.info "Parsing #{thrift_file}"

    generated_files =
      thrift_file
      |> parse!
      |> Thrift.Generator.generate!(output_path)

    if opts[:verbose] do
      generated_files
      |> Enum.uniq
      |> Enum.sort
      |> Enum.each(fn file ->
        Mix.shell.info "Wrote #{file}"
      end)
    end
  end
end
