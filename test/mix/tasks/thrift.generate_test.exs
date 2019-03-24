defmodule Mix.Tasks.Thrift.GenerateTest do
  use MixTest.Case

  test "not specifying any Thrift files" do
    in_fixture(fn ->
      Mix.Tasks.Thrift.Generate.run([])
      refute_received {:mix_shell, :info, [_]}
    end)
  end

  test "specifying multiple Thrift files" do
    in_fixture(fn ->
      with_project_config([], fn ->
        Mix.Tasks.Thrift.Generate.run(
          ~w[--verbose thrift/StressTest.thrift thrift/ThriftTest.thrift]
        )

        assert_received {:mix_shell, :info, ["Parsing thrift/StressTest.thrift"]}
        assert_received {:mix_shell, :info, ["Parsing thrift/ThriftTest.thrift"]}
        assert_received {:mix_shell, :info, ["Wrote lib/generated/service.ex"]}
        assert_received {:mix_shell, :info, ["Wrote lib/thrift_test/thrift_test.ex"]}

        assert File.exists?("lib/generated/service.ex")
        assert File.exists?("lib/thrift_test/thrift_test.ex")
      end)
    end)
  end

  test "specifying a non-existent Thrift file" do
    in_fixture(fn ->
      assert_raise Mix.Error, ~r/no such file or directory/, fn ->
        Mix.Tasks.Thrift.Generate.run(["missing.thrift"])
      end
    end)
  end

  test "specifying an invalid Thrift file" do
    in_fixture(fn ->
      bad_schema = """
      struct InvalidTest {
        1: i32 cannotDoArithmeticInThrift = 1 + 1,
      }
      """

      path = tempfile("invalid.thrift", bad_schema)

      assert_raise Mix.Error, ~r/Parse error/, fn ->
        Mix.Tasks.Thrift.Generate.run([path])
      end
    end)
  end

  test "specifying an alternate output directory (--out)" do
    in_fixture(fn ->
      with_project_config([], fn ->
        Mix.Tasks.Thrift.Generate.run(~w[--out lib/thrift thrift/ThriftTest.thrift])
        assert File.exists?("lib/thrift/thrift_test/thrift_test.ex")
      end)
    end)
  end

  test "specifying an include path (--include)" do
    in_fixture(fn ->
      with_project_config([], fn ->
        Mix.Tasks.Thrift.Generate.run(~w[--include thrift thrift/include/Include.thrift])
        assert File.exists?("lib/thrift_test/thrift_test.ex")
      end)
    end)
  end

  defp tempfile(filename, content) do
    temp_dir = Path.join([fixture_path(), "tmp"])
    File.mkdir_p!(temp_dir)
    on_exit(fn -> File.rm_rf!(temp_dir) end)

    path = Path.join([temp_dir, filename])
    File.write!(path, content)

    path
  end
end
