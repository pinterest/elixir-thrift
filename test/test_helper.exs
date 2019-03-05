Mix.start()
Mix.shell(Mix.Shell.Process)
ExUnit.configure(exclude: [pending: true], capture_log: true)
ExUnit.start()

defmodule ThriftTestHelpers do
  defmacro __using__(_) do
    quote do
      require ThriftTestHelpers
      import ThriftTestHelpers
    end
  end

  def build_thrift_file(base_dir, {file_name, contents}) do
    file_relative_path = Atom.to_string(file_name)
    file_path = Path.join(base_dir, file_relative_path)

    file_path
    |> Path.dirname()
    |> File.mkdir_p!()

    File.write!(file_path, contents)
    file_relative_path
  end

  def tmp_dir do
    tmp_path = Path.join(System.tmp_dir!(), Integer.to_string(System.unique_integer()))

    File.mkdir(tmp_path)
    tmp_path
  end

  def parse(_root_dir, nil) do
    nil
  end

  def parse(file_path) do
    {:ok, group} = Thrift.Parser.parse_file_group(file_path)
    group
  end

  @spec with_thrift_files(Keyword.t(), String.t()) :: nil
  defmacro with_thrift_files(opts, do: block) do
    {var_name, opts_1} = Keyword.pop(opts, :as, :file_group)
    {parsed_file, specs} = Keyword.pop(opts_1, :parse, nil)

    thrift_var = Macro.var(var_name, nil)

    quote location: :keep do
      root_dir = ThriftTestHelpers.tmp_dir()
      full_path = Path.join(root_dir, unquote(parsed_file))

      files = Enum.map(unquote(specs), &ThriftTestHelpers.build_thrift_file(root_dir, &1))
      unquote(thrift_var) = ThriftTestHelpers.parse(full_path)

      try do
        unquote(block)
      after
        File.rm_rf!(root_dir)
      end
    end
  end
end

defmodule MixTest.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      import MixTest.Case
    end
  end

  setup do
    on_exit(fn ->
      Mix.Shell.Process.flush()
      File.rm_rf!(Path.join(fixture_path(), "lib"))
    end)

    :ok
  end

  def fixture_path do
    Path.expand("fixtures/app", __DIR__)
  end

  def in_fixture(fun) do
    File.cd!(fixture_path(), fun)
  end

  def with_project_config(config, fun) do
    Mix.Project.in_project(:app, fixture_path(), config, fn _ -> fun.() end)
  end
end
