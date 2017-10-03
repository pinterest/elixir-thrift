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
    * `--namespace namespace` - set the default namespace for generated
      modules, overriding the `:namespace` configuration value
    * `-o dir` / `--out dir` - set the output directory, overriding the
      `:output_path` configuration value
    * `-v` / `--verbose` - enable verbose task logging

  ## Configuration

    * `:include_paths` - list of additional directory paths in which to
      search for included files. Defaults to `[]`.
    * `:namespace` - default namespace for generated modules, which will
      be used when Thrift files don't specify their own `elixir` namespace.
    * `:output_path` - output directory into which the generated Elixir
      source files will be generated. Defaults to `"lib"`.

  ```
  # example mix.exs
  defmodule MyProject.Mixfile do
    # ...

    def project do
      [
        # other srettings...
        thrift: [
          include_paths: ["./extra_thrift"],
          output_path: "lib/generated"
        ]
      ]
    end
  end
  ```
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args,
      switches: [include: :keep, namespace: :string, out: :string,
                 verbose: :boolean],
      aliases: [I: :include, o: :out, v: :verbose])

    config        = Keyword.get(Mix.Project.config, :thrift, [])
    output_path   = opts[:out] || Keyword.get(config, :output_path, "lib")
    namespace     = opts[:namespace] || Keyword.get(config, :namespace)
    include_paths =
      (opts[:include] && Keyword.get_values(opts, :include))
      || Keyword.get(config, :include_paths, [])

    parser_opts =
      Keyword.new
      |> Keyword.put(:include_paths, include_paths)
      |> Keyword.put(:namespace, namespace)

    unless Enum.empty?(files) do
      File.mkdir_p!(output_path)
      Enum.each(files, &generate!(&1, output_path, parser_opts, opts))
    end
  end

  defp parse!(thrift_file, opts) do
    Thrift.Parser.parse_file(thrift_file, opts)
  rescue
    e -> Mix.raise "#{thrift_file}: #{Exception.message(e)}"
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
