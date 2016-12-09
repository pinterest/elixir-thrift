defmodule Mix.Tasks.Compile.ThriftTest do
  use ExUnit.Case

  import Mix.Tasks.Compile.Thrift, only: [run: 1]
  import ExUnit.CaptureIO

  @fixture_project Path.expand("../../fixtures/app", __DIR__)

  setup do
    in_fixture(fn -> File.rm_rf!("src") end)
    :ok
  end

  test "compiling default :thrift_files" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert capture_io(fn -> run([]) end) =~ """
          Compiled thrift/shared.thrift
          Compiled thrift/tutorial.thrift
          """
        assert File.exists?("src/shared_types.hrl")
        assert File.exists?("src/tutorial_types.hrl")
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
        File.rm_rf!("src")
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

  test "specifying a custom --gen compiler option" do
    in_fixture fn ->
      with_project_config [thrift_options: ~w[--gen erl:maps]], fn ->
        assert capture_io(fn -> run([]) end) =~ "Compiled thrift/tutorial.thrift"
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
          assert capture_io(:stderr, fn -> run([]) end) =~ "Failed to compile"
        end
      end
    end
  end

  test "attempting to use a non-existent Thrift executable" do
    in_fixture fn ->
      with_project_config [thrift_executable: "nonexistentthrift"], fn ->
        assert_raise Mix.Error, "`nonexistentthrift` not found in the current path", fn ->
          run([])
        end
      end
    end
  end

  test "attempting to use a broken Thrift executable" do
    in_fixture fn ->
      with_project_config [thrift_executable: "mix", thrift_version: "0.0.0"], fn ->
        assert_raise Mix.Error, ~r/Failed to execute/, fn ->
          run([])
        end
      end
    end
  end

  test "attempting to use an unsupported Thrift version" do
    in_fixture fn ->
      with_project_config [thrift_version: "< 0.0.0"], fn ->
        assert_raise Mix.Error, ~r/^Unsupported Thrift version/, fn ->
          run([])
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
