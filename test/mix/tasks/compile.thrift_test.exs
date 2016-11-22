defmodule Mix.Tasks.Compile.ThriftTest do
  use ExUnit.Case

  import Mix.Tasks.Compile.Thrift, only: [run: 1]
  import ExUnit.CaptureIO

  @project_root Path.expand("../../../", __DIR__)
  @fixture_project_relative "test/fixtures/app"
  @fixture_project Path.join(@project_root, @fixture_project_relative)

  setup do
    # so that the docker-based tests will work
    System.put_env("DOCKER_THRIFT_OUT_ROOT", @fixture_project_relative)
    on_exit fn -> System.delete_env("DOCKER_THRIFT_OUT_ROOT") end

    in_fixture(fn -> File.rm_rf!("src") end)
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
