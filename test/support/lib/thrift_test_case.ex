defmodule ThriftTestCase do
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
    opts = Module.get_attribute(__CALLER__.module, :thrift_test_opts)

    namespace = inspect(env.module)

    dir = Path.join([System.tmp_dir!, inspect(__MODULE__), namespace])

    File.rm_rf!(dir)
    File.mkdir_p!(dir)

    modules = __CALLER__.module
    |> Module.get_attribute(:thrift_file)
    |> Enum.reverse
    |> Enum.map(fn [name: filename, contents: contents] ->
      filename = Path.expand(filename, dir)
      File.write!(filename, "namespace elixir #{namespace};" <> contents)
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

    tests = __CALLER__.module
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
    |> inspect_quoted
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
