defmodule Mix.Tasks.Compile.Thrift do
  use Mix.Task
  alias Thrift.Parser.FileGroup

  @moduledoc """
  Generate Elixir source files from Thrift schema files (`.thrift`).

  When this task runs, it first checks the modification times of all source
  files that were generated by the set of .thrift files.  If the generated
  files are older than the .thrift file that generated them, this task will
  skip regenerating them.

  A list of files can be given after the task name in order to select the
  specific Thrift schema files to compile:

      mix compile.thrift file1.thrift file2.thrift

  Otherwise, the file list from the `:thrift_files` configuration value
  will be used.

  ## Command line options

    * `--force` - forces compilation regardless of modification times
    * `--out` - set the output directory, overriding the `:thrift_output`
      configuration value

  ## Configuration

    * `:thrift_files` - list of `.thrift` schema files to compile

    * `:thrift_output` - output directory into which the generated Elixir
      source file will be generated. Defaults to `"lib"`.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, files} = OptionParser.parse!(args,
      aliases: [f: :force, o: :out, v: :verbose],
      switches: [force: :boolean, output: :keep, verbose: :boolean])

    config     = Mix.Project.config
    output_dir = opts[:out] || Keyword.get(config, :thrift_output, "lib")

    thrift_files = if Enum.empty?(files) do
      Keyword.get(config, :thrift_files, [])
    else
      files
    end

    file_groups = thrift_files
    |> Enum.map(&parse/1)
    |> Enum.reject(&is_nil/1)

    stale_groups = Enum.filter(file_groups, fn file_group ->
      opts[:force] || stale?(file_group, output_dir)
    end)

    unless Enum.empty?(stale_groups) do
      File.mkdir_p!(output_dir)
      Mix.Utils.compiling_n(length(stale_groups), :thrift)
      Enum.each(stale_groups, &generate(&1, output_dir, opts))
    end
  end

  defp parse(thrift_file) do
    try do
      Thrift.Parser.parse_file(thrift_file)
    rescue
      e ->
        Mix.shell.error "Failed to parse #{thrift_file}: #{Exception.message(e)}"
        nil
    end
  end

  defp stale?(%FileGroup{initial_file: thrift_file} = group, output_dir) do
    targets = group
    |> Thrift.Generator.targets
    |> Enum.map(&Path.join(output_dir, &1))
    Enum.empty?(targets) || Mix.Utils.stale?([thrift_file], targets)
  end

  defp generate(%FileGroup{} = group, output_dir, opts) do
    Thrift.Generator.generate!(group, output_dir)
    if opts[:verbose], do: Mix.shell.info "Compiled #{group.initial_file}"
  end
end
