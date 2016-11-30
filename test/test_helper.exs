ExUnit.configure(exclude: [pending: true])
ExUnit.start()


defmodule ThriftTestHelpers do
  @root_dir "test/fixtures/thrift/"

  defmacro __using__(_) do
    quote do
      require ThriftTestHelpers
      import ThriftTestHelpers
    end
  end

  def build_thrift_file(base_dir, {file_name, contents}) do
    file_relative_path = file_name
    |> Atom.to_string


    file_path = Path.join(base_dir, file_relative_path)

    file_path
    |> Path.dirname
    |> File.mkdir_p!

    File.write!(file_path, contents)
    file_relative_path
  end

  def tmp_dir do
    tmp_path = System.tmp_dir!
    |> Path.join(Integer.to_string(System.unique_integer))

    File.mkdir(tmp_path)
    tmp_path
  end

  def parse(_root_dir, nil) do
    nil
  end

  def parse(file_path) do
    alias Thrift.Parser
    Parser.parse_file(file_path)
  end

  @spec with_thrift_files(Keyword.t, String.t) :: nil
  defmacro with_thrift_files(opts, do: block) do
    {var_name, opts_1} = Keyword.pop(opts, :as, :file_group)
    {parsed_file, specs} = Keyword.pop(opts_1, :parse, nil)

    thrift_var = Macro.var(var_name, nil)

    quote location: :keep do
      root_dir = ThriftTestHelpers.tmp_dir
      full_path = root_dir
      |> Path.join(unquote(parsed_file))

      files = unquote(specs)
      |> Enum.map(&ThriftTestHelpers.build_thrift_file(root_dir, &1))
      unquote(thrift_var) = ThriftTestHelpers.parse(full_path)
      try do
        unquote(block)
        File.rm_rf!(root_dir)
      rescue e ->
        File.rm_rf!(root_dir)
        raise e
      end
    end
  end

end
