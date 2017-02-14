defmodule Mix.Tasks.Compile.Thrift do
  use Mix.Task
  alias Thrift.Parser.FileGroup

  @recursive true
  @manifest ".compile.thrift"
  @manifest_vsn :v1

  @moduledoc """
  Compiler task that generates Elixir source files from Thrift schema files
  (`.thrift`).

  When this task runs, it first checks the modification times of all source
  files that were generated by the set of `.thrift` files.  If any generated
  files are older than the `.thrift` file that generated them, they won't be
  regenerated.

  ## Command line options

    * `--force` - forces compilation regardless of modification times
    * `--verbose` - enable verbose compile task logging

  ## Configuration

    * `:files` - list of `.thrift` schema files to compile
    * `:include_paths` - list of additional directory paths in which to
      search for included files
    * `:namespace` - default namespace for generated modules, which will
      be used when a Thrift file doesn't define its own `elixir` namespace.
    * `:output_path` - output directory into which the generated Elixir
      source files will be generated. Defaults to `"lib"`.
  """

  @switches [force: :boolean, verbose: :boolean]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    config      = Keyword.get(Mix.Project.config, :thrift, [])
    input_files = Keyword.get(config, :files, [])
    output_path = Keyword.get(config, :output_path, "lib")
    parser_opts = Keyword.take(config, [:include_paths, :namespace])

    mappings =
      input_files
      |> Enum.map(&parse(&1, parser_opts))
      |> Enum.reject(&is_nil/1)
      |> extract_targets(output_path, opts[:force])

    generate(manifest(), mappings, output_path, opts)
  end

  @doc "Returns the Thrift compiler's manifests."
  @spec manifests :: [Path.t]
  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc "Cleans up generated files."
  @spec clean :: :ok | {:error, File.posix}
  def clean, do: clean(manifest())
  defp clean(manifest) do
    Enum.each(read_manifest(manifest), &File.rm/1)
    File.rm(manifest)
  end

  @spec parse(Path.t, OptionParser.parsed) :: FileGroup.t
  defp parse(file, opts) do
    try do
      Thrift.Parser.parse_file(file, opts)
    rescue
      e ->
        Mix.shell.error "Failed to parse #{file}: #{Exception.message(e)}"
        nil
    end
  end

  @typep mappings ::
    [{:stale, FileGroup.t, [Path.t]} | {:ok, FileGroup.t, [Path.t]}]

  @spec extract_targets([FileGroup.t], Path.t, boolean) :: mappings
  defp extract_targets(groups, output_path, force) when is_list(groups) do
    for %FileGroup{initial_file: file} = group <- groups do
      targets =
        group
        |> Thrift.Generator.targets
        |> Enum.map(&Path.join(output_path, &1))

      if force || Mix.Utils.stale?([file], targets) do
        {:stale, group, targets}
      else
        {:ok, group, targets}
      end
    end
  end

  @spec generate(Path.t, mappings, Path.t, OptionParser.parsed) :: :ok | :noop
  defp generate(manifest, mappings, output_path, opts) do
    timestamp = :calendar.universal_time()
    verbose = opts[:verbose]

    # Load the list of previously-generated files.
    previous = read_manifest(manifest)

    # Determine which of our current targets are in need of (re)generation.
    stale = for {:stale, group, targets} <- mappings, do: {group, targets}

    # Determine if there are any files that appear in our existing manifest
    # that are no longer relevant based on our current target mappings.
    removed = Enum.filter(previous, fn file ->
      not Enum.any?(mappings, fn {_, _, targets} -> file in targets end)
    end)

    if stale == [] && removed == [] do
      :noop
    else
      # Ensure we have an output directory and remove old target files.
      File.mkdir_p!(output_path)
      Enum.each(removed, &File.rm/1)

      unless Enum.empty?(stale) do
        Mix.Utils.compiling_n(length(stale), :thrift)
        Enum.each(stale, fn {group, _targets} ->
          Thrift.Generator.generate!(group, output_path)
          verbose && Mix.shell.info "Compiled #{group.initial_file}"
        end)
      end

      # Update and rewrite the manifest.
      entries = (previous -- removed) ++ Enum.flat_map(stale, &elem(&1, 1))
      write_manifest(manifest, :lists.usort(entries), timestamp)
      :ok
    end
  end

  defp thrift_vsn do
    Keyword.get(Mix.Project.config, :version)
  end

  @spec read_manifest(Path.t) :: [Path.t]
  defp read_manifest(manifest) do
    header = {@manifest_vsn, thrift_vsn()}
    try do
      manifest |> File.read! |> :erlang.binary_to_term
    rescue
      _ -> []
    else
      [^header | paths] -> paths
      _ -> []
    end
  end

  @spec write_manifest(Path.t, [Path.t], :calendar.datetime) :: :ok
  defp write_manifest(manifest, paths, timestamp) do
    data =
      [{@manifest_vsn, thrift_vsn()} | paths]
      |> :erlang.term_to_binary(compressed: 9)

    Path.dirname(manifest) |> File.mkdir_p!
    File.write!(manifest, data)
    File.touch!(manifest, timestamp)
  end
end
