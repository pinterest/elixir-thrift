defmodule Mix.Tasks.Compile.ThriftTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  @project_root Path.expand("../../../", __DIR__)
  @fixture_project Path.join(@project_root, "test/fixtures/app")

  setup do
    in_fixture(fn -> File.rm_rf!("lib") end)
    :ok
  end

  test "compiling default :thrift_files" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ """
          Compiling 3 files (.thrift)
          Compiled thrift/shared.thrift
          Compiled thrift/simple.thrift
          Compiled thrift/tutorial.thrift
          """
        assert File.exists?("lib/shared/shared_struct.ex")
        assert File.exists?("lib/tutorial/calculator.ex")
      end
    end
  end

  test "recompiling unchanged targets" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/tutorial.thrift"
        assert run([]) == ""
      end
    end
  end

  test "recompiling stale targets" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/tutorial.thrift"
        File.rm_rf!("lib")
        assert run([]) =~ "Compiled thrift/tutorial.thrift"
      end
    end
  end

  test "forcing compilation" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/tutorial.thrift"
        assert run(["--force"]) =~ "Compiled thrift/tutorial.thrift"
      end
    end
  end

  test "specifying an empty :thrift_files list" do
    in_fixture fn ->
      with_project_config [thrift_files: []], fn ->
        assert run([]) == ""
      end
    end
  end

  test "specifying a non-existent Thrift file" do
    in_fixture fn ->
      with_project_config [thrift_files: ~w("missing.thrift")], fn ->
        capture_io fn ->
          assert run(:stderr, []) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying an invalid Thrift file" do
    in_fixture fn ->
      with_project_config [thrift_files: [__ENV__.file]], fn ->
        capture_io fn ->
          assert run(:stderr, []) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying source files on the command line" do
    in_fixture fn ->
      with_project_config [], fn ->
        output = run(["thrift/simple.thrift"])
        assert output =~ "Compiled thrift/simple.thrift"
        refute output =~ "Compiled thrift/tutorial.thrift"
      end
    end
  end

  defp run(args) when is_list(args), do: run(:stdio, args)
  defp run(device, args) when device in [:stdio, :stderr] and is_list(args) do
    args = ["--verbose" | args]
    capture_io(device, fn -> Mix.Tasks.Compile.Thrift.run(args) end)
  end

  defp in_fixture(fun) do
    File.cd!(@fixture_project, fun)
  end

  defp with_project_config(config, fun) do
    Mix.Project.in_project(:app, @fixture_project, config, fn(_) -> fun.() end)
  end
end
