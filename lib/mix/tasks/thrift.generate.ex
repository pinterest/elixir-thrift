defmodule Mix.Tasks.Thrift.Generate do
  use Mix.Task

  @shortdoc "Generate Elixir source files from Thrift schema files"

  @moduledoc """
  Generate Elixir source files from Thrift schema files (`.thrift`).

  A list of files can be given after the task name in order to select the
  specific Thrift schema files to parse:

      mix thrift.generate file1.thrift file2.thrift

  ## Command line options

    * `-o` `--out` - set the output directory, overriding the `:thrift_output`
      configuration value
    * `-v` `--verbose` - enable verbose task logging

  ## Configuration

    * `:thrift_output` - output directory into which the generated Elixir
      source file will be generated. Defaults to `"lib"`.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args,
      aliases: [o: :out, v: :verbose],
      switches: [out: :string, verbose: :boolean])

    config     = Mix.Project.config
    output_dir = opts[:out] || Keyword.get(config, :thrift_output, "lib")

    unless Enum.empty?(files) do
      File.mkdir_p!(output_dir)
      Enum.each(files, &generate!(&1, output_dir, opts))
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

  defp generate!(thrift_file, output_dir, opts) do
    if opts[:verbose] do
      Mix.shell.info "Reading #{thrift_file}"
    end

    generated_files =
      thrift_file
      |> parse!
      |> Thrift.Generator.generate!(output_dir)

    if opts[:verbose] do
      files =
        generated_files
        |> Enum.uniq
        |> Enum.sort
        |> Enum.join(" ")
      Mix.shell.info "Wrote #{files}"
    end
  end
end
