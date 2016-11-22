defmodule ThriftTestCase do
  @project_root Path.expand("../../../", __DIR__)

  defmacro __using__(opts \\ []) do
    quote do
      @thrift_test_opts unquote(opts)
      import unquote(__MODULE__)
      Module.register_attribute(__MODULE__, :thrift_file, accumulate: true)
      Module.register_attribute(__MODULE__, :thrift_test, accumulate: true)
      @before_compile unquote(__MODULE__)
      use ExUnit.Case, async: true
    end
  end

  defmacro __before_compile__(env) do
    tag = Module.get_attribute(__CALLER__.module, :moduletag)
    |> Map.new(fn tag ->  {tag, true} end)

    config = ExUnit.configuration
    case ExUnit.Filters.eval(config[:include], config[:exclude], tag, []) do
      :ok ->
        compile_and_build_erlang_helpers(__CALLER__, env)
      {:error, _} ->
        nil
    end
  end

  defp compile_and_build_erlang_helpers(caller, env) do
    opts = Module.get_attribute(caller.module, :thrift_test_opts)

    namespace = inspect(env.module)

    out_root = Path.join(@project_root, "tmp")
    dir = Path.join([out_root, inspect(__MODULE__), namespace])
    File.rm_rf!(dir)
    File.mkdir_p!(dir)


    modules = caller.module
    |> Module.get_attribute(:thrift_file)
    |> Enum.reverse
    |> Enum.map(fn [name: filename, contents: contents] ->
      filename = Path.expand(filename, dir)
      File.write!(filename, "namespace elixir #{namespace}\n" <> contents)
      filename
    end)
    |> Enum.flat_map(&Thrift.Generator.Models.generate!(&1, dir))
    |> Enum.uniq
    |> Enum.map(fn output_file ->
      output_file
      |> Path.expand(dir)
      |> Code.eval_file

      namespace_module = output_file
      |> Path.dirname
      |> String.split("/")
      |> Enum.map(&Macro.camelize/1)
      |> Enum.join(".")

      basename_module = output_file
      |> Path.basename(".ex")
      |> Macro.camelize

      :"Elixir.#{namespace_module}.#{basename_module}"
    end)

    record_requires = if opts[:gen_erl] do
      caller.module
      |> Module.get_attribute(:thrift_file)
      |> Enum.reverse
      |> generate_erlang_files(dir)
    else
      []
    end

    tests = caller.module
    |> Module.get_attribute(:thrift_test)
    |> Enum.reverse
    |> Enum.map(fn {test_name, block} ->
      quote location: :keep do
        test unquote(test_name) do
          unquote(block)
        end
      end
    end)

    quote do
      unquote_splicing(Enum.map(modules, fn module ->
        quote do: alias unquote(module)
      end))
      unquote_splicing(Enum.map(modules, fn module ->
        quote do: require unquote(module)
      end))

      unquote_splicing(record_requires)
      setup_all do
        on_exit fn ->
          unquote(if Keyword.get(opts, :cleanup, true) do
            quote do: File.rm_rf!(unquote(dir))
          else
            quote do: IO.puts IO.ANSI.format([:yellow, unquote("Leaving files in #{inspect dir}")])
          end)
        end
        :ok
      end

      unquote_splicing(tests)
    end
  end

  defp generate_erlang_files(list_of_files, dir) do
    erlang_source_dir = Path.join(dir, "src")

    File.mkdir(erlang_source_dir)

    outdir = Path.relative_to(erlang_source_dir, @project_root)
    reldir = Path.relative_to(dir, @project_root)

    list_of_files
    |> Enum.map(fn file ->
      filename = Path.join(reldir, file[:name])
      System.cmd(System.get_env("THRIFT") || "thrift",
                 ["-out", outdir,
                  "--gen", "erl", "-r", filename],
                 cd: @project_root)
    end)

    Path.wildcard("#{erlang_source_dir}/*.erl")
    |> Enum.map(fn source_file ->
      {:ok, mod_name, code} = source_file
      |> String.to_char_list
      |> :compile.file([:binary])

      :code.load_binary(mod_name, [], code)
    end)

    Path.wildcard("#{erlang_source_dir}/*_types.hrl")
    |> Enum.map(&build_records/1)
  end

  defp build_records(file_path) do
    erlang_module =  file_path
    |> Path.basename
    |> Path.rootname
    |> String.to_atom

    record_module_name = erlang_module
    |> Atom.to_string
    |> String.replace("_types", "")
    |> Macro.camelize
    |> String.to_atom

    module_name = Module.concat(Erlang, record_module_name)

    records = Record.extract_all(from: file_path)
    |> Enum.map(fn {record_name, fields} ->
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

        def unquote(serialize_fn_name)({unquote(underscored_record_name), unquote_splicing(match)}=record, opts \\ []) do
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

    quote do
      defmodule unquote(module_name) do
        require Record
        unquote_splicing(records)
      end
    end
    |> Code.compile_quoted

    quote do: require unquote(module_name)
  end

  defmacro thrift_test(name, do: block) do
    quote do
      @thrift_test {unquote(name), unquote({:quote, [], [[do: block]]})}
    end
  end

  def inspect_quoted(block) do
    block |> Macro.to_string |> IO.puts
    block
  end
end
