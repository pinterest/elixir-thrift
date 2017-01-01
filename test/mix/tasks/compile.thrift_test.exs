defmodule Mix.Tasks.Compile.ThriftTest do
  use ExUnit.Case

  import Mix.Tasks.Compile.Thrift, only: [run: 1]
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
        assert capture_io(fn -> run([]) end) =~ """
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
        assert capture_io(fn -> run([]) end) =~ "Compiled thrift/tutorial.thrift"
        assert capture_io(fn -> run([]) end) == ""
      end
    end
  end

  test "recompiling stale targets" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert capture_io(fn -> run([]) end) =~ "Compiled thrift/tutorial.thrift"
        File.rm_rf!("lib")
        assert capture_io(fn -> run([]) end) =~ "Compiled thrift/tutorial.thrift"
      end
    end
  end

  test "forcing compilation" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert capture_io(fn -> run([]) end) =~ "Compiled thrift/tutorial.thrift"
        assert capture_io(fn -> run(["--force"]) end) =~ "Compiled thrift/tutorial.thrift"
      end
    end
  end

  test "specifying an empty :thrift_files list" do
    in_fixture fn ->
      with_project_config [thrift_files: []], fn ->
        assert capture_io(fn -> run([]) end) == ""
      end
    end
  end

  test "specifying a non-existent Thrift file" do
    in_fixture fn ->
      with_project_config [thrift_files: ~w("missing.thrift")], fn ->
        capture_io fn ->
          assert capture_io(:stderr, fn -> run([]) end) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying an invalid Thrift file" do
    in_fixture fn ->
      with_project_config [thrift_files: [__ENV__.file]], fn ->
        capture_io fn ->
          assert capture_io(:stderr, fn -> run([]) end) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying source files on the command line" do
    in_fixture fn ->
      with_project_config [], fn ->
        capture_io fn ->
          output = capture_io(fn -> run(["thrift/simple.thrift"]) end)
          assert output =~ "Compiled thrift/simple.thrift"
          refute output =~ "Compiled thrift/tutorial.thrift"
        end
      end
    end
  end

  defp in_fixture(fun) do
    File.cd!(@fixture_project, fun)
  end

  defp with_project_config(config, fun) do
    Mix.Project.in_project(:app, @fixture_project, config, fn(_) -> fun.() end)
  end
end
