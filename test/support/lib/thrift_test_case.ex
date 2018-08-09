defmodule ThriftTestCase do
  @moduledoc false
  @project_root Path.expand("../../../", __DIR__)

  use ExUnit.CaseTemplate

  using(opts) do
    dir_prefix = Path.join([@project_root, "tmp", inspect(__MODULE__)])

    quote do
      Module.register_attribute(__MODULE__, :thrift_test_opts, persist: true)
      @thrift_test_opts unquote(opts)
      Module.register_attribute(__MODULE__, :thrift_test_dir, persist: true)
      @thrift_test_dir Path.join(unquote(dir_prefix), inspect(__MODULE__))
      File.rm_rf!(@thrift_test_dir)
      File.mkdir_p!(@thrift_test_dir)
      import unquote(__MODULE__), only: [thrift_test: 2, thrift_test: 3]
      Module.register_attribute(__MODULE__, :thrift_file, accumulate: true)
      Module.register_attribute(__MODULE__, :thrift_elixir_modules, accumulate: true)
      Module.register_attribute(__MODULE__, :thrift_record_modules, accumulate: true)
    end
  end

  def implement?(module) do
    tag =
      module
      |> Module.get_attribute(:moduletag)
      |> Map.new(fn tag -> {tag, true} end)

    config = ExUnit.configuration()

    case ExUnit.Filters.eval(config[:include], config[:exclude], tag, []) do
      :ok ->
        true

      {:error, _} ->
        false
    end
  end

  def quoted_contents(module, contents) do
    compile_and_build_helpers(module)
    directives = quoted_directives(module)
    [directives, contents]
  end

  defp compile_and_build_helpers(module) do
    files = get_thrift_files(module)
    dir = Module.get_attribute(module, :thrift_test_dir)
    generate_files(files, module, dir)
  end

  defp get_thrift_files(module) do
    files = Module.get_attribute(module, :thrift_file)
    Module.delete_attribute(module, :thrift_file)
    Module.register_attribute(module, :thrift_file, accumulate: true)

    Enum.reverse(files)
  end

  defp write_thrift_file(config, namespace, dir) do
    filename =
      config
      |> Keyword.fetch!(:name)
      |> Path.expand(dir)

    contents = Keyword.fetch!(config, :contents)

    File.write!(filename, "namespace elixir #{inspect(namespace)}\n" <> contents)

    filename
  end

  defp require_file(file, dir) do
    case Code.require_file(file, dir) do
      nil ->
        []

      modules ->
        parts = Enum.map(modules, fn {module, _} -> Module.split(module) end)
        for part <- parts, alias?(part, parts), do: Module.concat(part)
    end
  end

  defp alias?(module, modules) do
    # alias when parent module in namespace does not exist
    not Enum.any?(modules, &(:lists.prefix(&1, module) and &1 != module))
  end

  defp generate_files(files, namespace, dir) do
    files
    |> Enum.map(&write_thrift_file(&1, namespace, dir))
    |> Enum.flat_map(&Thrift.Generator.generate!(&1, dir))
    |> Enum.flat_map(&require_file(&1, dir))
    |> Enum.each(&Module.put_attribute(namespace, :thrift_elixir_modules, &1))
  end

  defp quoted_directives(namespace) do
    elixir_modules = Module.get_attribute(namespace, :thrift_elixir_modules)
    record_modules = Module.get_attribute(namespace, :thrift_record_modules)

    quote do
      unquote_splicing(
        Enum.map(elixir_modules, fn module ->
          quote do: alias(unquote(module))
        end)
      )

      unquote_splicing(
        Enum.map(elixir_modules ++ record_modules, fn module ->
          quote do: require(unquote(module))
        end)
      )
    end
  end

  setup_all context do
    module = context[:module] || context[:case]
    attributes = module.__info__(:attributes)
    opts = attributes[:thrift_test_opts]
    [dir] = attributes[:thrift_test_dir]

    on_exit(fn ->
      if Keyword.get(opts, :cleanup, true) do
        File.rm_rf!(dir)
      else
        IO.puts(IO.ANSI.format([:yellow, "Leaving files in #{inspect(dir)}"]))
      end
    end)

    :ok
  end

  defmacro thrift_test(message, var \\ quote(do: _), do: block) do
    var = Macro.escape(var)
    block = Macro.escape(block, unquote: true)

    quote bind_quoted: [module: __MODULE__, message: message, var: var, block: block] do
      if module.implement?(__MODULE__) do
        contents = module.quoted_contents(__MODULE__, block)
        name = ExUnit.Case.register_test(__ENV__, :test, message, [])
        def unquote(name)(unquote(var)), do: unquote(contents)
      else
        ExUnit.Case.test message do
          flunk("not implemented")
        end
      end
    end
  end

  def inspect_quoted(block) do
    block
    |> Macro.to_string()
    |> IO.puts()

    block
  end
end
