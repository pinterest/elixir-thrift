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

    config = ExUnit.configuration
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
    opts = Module.get_attribute(module, :thrift_test_opts)
    generate_files(files, module, dir, opts)
  end

  defp get_thrift_files(module) do
    files = Module.get_attribute(module, :thrift_file)
    Module.delete_attribute(module, :thrift_file)
    Module.register_attribute(module, :thrift_file, [accumulate: true])

    Enum.reverse(files)
  end

  defp write_thrift_file(config, namespace, dir) do
    filename = config
      |> Keyword.fetch!(:name)
      |> Path.expand(dir)

    contents = Keyword.fetch!(config, :contents)

    File.write!(filename, "namespace elixir #{inspect namespace}\n" <> contents)

    filename
  end

  defp require_file(file, dir) do
    case Code.require_file(file, dir) do
      nil ->
        []
      modules ->
        parts = Enum.map(modules, fn({module, _}) -> Module.split(module) end)
        for part <- parts, alias?(part, parts), do: Module.concat(part)
      end
  end

  defp alias?(module, modules) do
    # alias when parent module in namespace does not exist
    not Enum.any?(modules, &(:lists.prefix(&1, module) and &1 != module))
  end

  defp generate_files(files, namespace, dir, opts) do
    generate_elixir_files(files, namespace, dir)

    if opts[:gen_erl] do
      generate_erlang_files(files, namespace, dir)
    else
      :ok
    end
  end

  defp generate_elixir_files(files, namespace, dir) do
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
      unquote_splicing(Enum.map(elixir_modules, fn module ->
        quote do: alias unquote(module)
      end))
      unquote_splicing(Enum.map(elixir_modules ++ record_modules, fn module ->
        quote do: require unquote(module)
      end))
    end
  end

  setup_all context do
    module = context[:module] || context[:case]
    attributes = module.__info__(:attributes)
    opts = attributes[:thrift_test_opts]
    [dir] = attributes[:thrift_test_dir]

    on_exit fn ->
      if Keyword.get(opts, :cleanup, true) do
        File.rm_rf!(dir)
      else
        IO.puts IO.ANSI.format([:yellow, "Leaving files in #{inspect dir}"])
      end
    end
    :ok
  end

  defp generate_erlang_files(list_of_files, namespace, dir) do
    erlang_source_dir = Path.join(dir, "src")

    File.mkdir(erlang_source_dir)

    outdir = Path.relative_to(erlang_source_dir, @project_root)
    reldir = Path.relative_to(dir, @project_root)

    for file <- list_of_files do
      filename = Path.join(reldir, file[:name])
      {_, 0} = System.cmd(System.get_env("THRIFT") || "thrift",
                          ["-out", outdir,
                          "--gen", "erl", "-r", filename],
                          cd: @project_root)
    end

    for source_file <- Path.wildcard("#{erlang_source_dir}/*.erl") do
      ensure_erlang_compiled(source_file)
    end

    Path.wildcard("#{erlang_source_dir}/*_types.hrl")
    |> Enum.map(&ensure_record_compiled/1)
    |> Enum.each(&Module.put_attribute(namespace, :thrift_record_modules, &1))
  end

  defp ensure_erlang_compiled(source_file) do
    module = erlang_module(source_file)
    if loaded?(source_file, module) do
      module
    else
      erlang_compile(source_file)
    end
  end

  defp loaded?(source_file \\ nil, mod) do
    source_file = source_file && String.to_charlist(source_file)
    case :code.is_loaded(mod) do
      {:file, ^source_file} ->
        true
      {:file, :in_memory} ->
        true
      _ ->
        false
    end
  end

  defp erlang_compile(source_file) do
    source_file = String.to_charlist(source_file)
    {:ok, mod_name, code} = :compile.file(source_file, [:binary])

    {:module, ^mod_name} = :code.load_binary(mod_name, source_file, code)
    mod_name
  end

  defp erlang_module(filepath) do
    filepath
    |> Path.basename
    |> Path.rootname
    |> String.to_atom
  end

  defp ensure_record_compiled(file_path) do
    erlang_module = erlang_module(file_path)

    record_module_name = erlang_module
    |> Atom.to_string
    |> String.replace("_types", "")
    |> Macro.camelize
    |> String.to_atom

    module_name = Module.concat(Erlang, record_module_name)

    if loaded?(module_name) do
      module_name
    else
      record_compile(file_path, erlang_module, module_name)
    end
  end

  defp record_compile(file_path, erlang_module, module_name) do
    records = Enum.map(Record.extract_all(from: file_path), fn {record_name, fields} ->
      underscored_record_name = record_name
      |> Atom.to_string
      |> Macro.underscore
      |> String.to_atom

      new_fn_name = :"new_#{underscored_record_name}"
      serialize_fn_name = :"serialize_#{underscored_record_name}"
      deserialize_fn_name = :"deserialize_#{underscored_record_name}"

      match = Enum.map(fields, fn _ -> Macro.var(:_, nil) end)
      kw_match = Enum.map(fields, fn {name, _} -> {name, Macro.var(name, nil)} end)
      variable_assigns = Enum.map(fields, fn {name, default} ->
        field_var = Macro.var(name, nil)
        quote do
          unquote(field_var) = Keyword.get(opts, unquote(name), unquote(default))
        end
      end)

      quote do
        Record.defrecord unquote(underscored_record_name), unquote(fields)
        def unquote(new_fn_name)(opts \\ []) do
          unquote_splicing(variable_assigns)
          record = unquote(underscored_record_name)(unquote(kw_match))
          :erlang.setelement(1, record, unquote(record_name))
        end

        def unquote(serialize_fn_name)(record, opts \\ [])
        def unquote(serialize_fn_name)({unquote(record_name), unquote_splicing(match)} = record, opts) do
          record = :erlang.setelement(1, record, unquote(underscored_record_name))
          unquote(serialize_fn_name)(record, opts)
        end
        def unquote(serialize_fn_name)({unquote(underscored_record_name), unquote_splicing(match)} = record, opts) do
          record = :erlang.setelement(1, record, unquote(record_name))
          struct_info = {:struct, {unquote(erlang_module), unquote(record_name)}}
          iolist_struct = with({:ok, tf} <- :thrift_memory_buffer.new_transport_factory(),
                               {:ok, pf} <- :thrift_binary_protocol.new_protocol_factory(tf, []),
                               {:ok, binary_protocol} <- pf.()) do

            {proto, :ok} = :thrift_protocol.write(binary_protocol, {struct_info, record})
            {_, data} = :thrift_protocol.flush_transport(proto)
            data
          end

          if Keyword.get(opts, :convert_to_binary, true) do
            :erlang.iolist_to_binary(iolist_struct)
          else
            iolist_struct
          end
        end

        def unquote(deserialize_fn_name)(binary_data) do
          struct_info = {:struct, {unquote(erlang_module), unquote(record_name)}}
          try do
            with({:ok, memory_buffer_transport} <- :thrift_memory_buffer.new(binary_data),
                 {:ok, binary_protocol} <- :thrift_binary_protocol.new(memory_buffer_transport),
                 {_, {:ok, record}} <- :thrift_protocol.read(binary_protocol, struct_info)) do

              record
            end
          rescue _ ->
              {:error, :cant_decode}
          end
        end
      end
    end)

    Code.compile_quoted(quote do
      defmodule unquote(module_name) do
        require Record
        unquote_splicing(records)
      end
    end)

    module_name
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
          flunk "not implemented"
        end
      end
    end
  end

  def inspect_quoted(block) do
    block |> Macro.to_string |> IO.puts
    block
  end
end
