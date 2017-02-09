defmodule Mix.Tasks.Thrift.Generate do
  use Mix.Task

  @shortdoc "Generate Elixir source files from Thrift schema files"

  @moduledoc """
  Generate Elixir source files from Thrift schema files (`.thrift`).

  A list of files should be given after the task name in order to select
  the specific Thrift schema files to parse:

      mix thrift.generate file1.thrift file2.thrift

  ## Command line options

    * `-I dir` / `--include dir` - add a directory to the list of
      directory paths in which to search for included files, overriding
      the `:include_paths` configuration value. This option can be repeated
      in order to add multiple directories to the search list.
    * `-o dir` / `--out dir` - set the output directory, overriding the
      `:output_path` configuration value
    * `-v` / `--verbose` - enable verbose task logging

  ## Configuration

    * `:include_paths` - list of additional directory paths in which to
      search for included files. Defaults to `[]`.
    * `:output_path` - output directory into which the generated Elixir
      source files will be generated. Defaults to `"lib"`.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args,
      aliases: [I: :include, o: :out, v: :verbose],
      switches: [include: :keep, out: :string, verbose: :boolean])

    config        = Keyword.get(Mix.Project.config, :thrift, [])
    output_path   = opts[:out] || Keyword.get(config, :output_path, "lib")
    include_paths =
      (opts[:include] && Keyword.get_values(opts, :include))
      || Keyword.get(config, :include_paths, [])

    parser_opts =
      Keyword.new
      |> Keyword.put(:include_paths, include_paths)

    unless Enum.empty?(files) do
      File.mkdir_p!(output_path)
      Enum.each(files, &generate!(&1, output_path, parser_opts, opts))
    end
  end

  defp parse!(thrift_file, opts) do
    try do
      Thrift.Parser.parse_file(thrift_file, opts)
    rescue
      e ->
        Mix.raise "#{thrift_file}: #{Exception.message(e)}"
    end
  end

  defp generate!(thrift_file, output_path, parser_opts, opts) do
    Mix.shell.info "Parsing #{thrift_file}"

    generated_files =
      thrift_file
      |> parse!(parser_opts)
      |> Thrift.Generator.generate!(output_path)

    if opts[:verbose] do
      generated_files
      |> Enum.uniq
      |> Enum.sort
      |> Enum.each(fn file ->
        Mix.shell.info "Wrote #{Path.join(output_path, file)}"
      end)
    end
  end
end
