defmodule Mix.Tasks.Compile.ThriftTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  @project_root Path.expand("../../../", __DIR__)
  @fixture_project Path.join(@project_root, "test/fixtures/app")

  setup do
    on_exit(fn -> File.rm_rf!(Path.join(@fixture_project, "lib")) end)
    :ok
  end

  test "compiling default :files" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ """
          Compiling 3 files (.thrift)
          Compiled thrift/StressTest.thrift
          Compiled thrift/ThriftTest.thrift
          Compiled thrift/numbers.thrift
          """
        assert File.exists?("lib/generated/service.ex")
        assert File.exists?("lib/thrift_test/thrift_test.ex")
        assert File.exists?("lib/tutorial/numbers.ex")
      end
    end
  end

  test "recompiling unchanged targets" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/ThriftTest.thrift"
        assert run([]) == ""
      end
    end
  end

  test "recompiling stale targets" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/ThriftTest.thrift"
        File.rm_rf!("lib")
        assert run([]) =~ "Compiled thrift/ThriftTest.thrift"
      end
    end
  end

  test "forcing compilation" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run([]) =~ "Compiled thrift/ThriftTest.thrift"
        assert run(["--force"]) =~ "Compiled thrift/ThriftTest.thrift"
      end
    end
  end

  test "cleaning generated files" do
    in_fixture fn ->
      with_project_config [], fn ->
        run([])
        assert File.exists?("lib/thrift_test/thrift_test.ex")
        assert Enum.all?(Mix.Tasks.Compile.Thrift.manifests, &File.exists?/1)

        Mix.Tasks.Compile.Thrift.clean()
        refute File.exists?("lib/thrift_test/thrift_test.ex")
        refute Enum.any?(Mix.Tasks.Compile.Thrift.manifests, &File.exists?/1)
      end
    end
  end

  test "specifying an empty :files list" do
    in_fixture fn ->
      with_project_config [thrift: [files: []]], fn ->
        assert run([]) == ""
      end
    end
  end

  test "specifying a non-existent Thrift file" do
    in_fixture fn ->
      with_project_config [thrift: [files: ~w("missing.thrift")]], fn ->
        capture_io fn ->
          assert run(:stderr, []) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying an invalid Thrift file" do
    in_fixture fn ->
      with_project_config [thrift: [files: [__ENV__.file]]], fn ->
        capture_io fn ->
          assert run(:stderr, []) =~ "Failed to parse"
        end
      end
    end
  end

  test "specifying an additional include path" do
    config = [
      files: ~w(thrift/include/Include.thrift),
      include_paths: ~w(thrift)
    ]
    in_fixture fn ->
      with_project_config [thrift: config], fn ->
        assert run([]) =~ "Compiled thrift/include/Include.thrift"
      end
    end
  end

  test "specifying an unknown option" do
    in_fixture fn ->
      with_project_config [], fn ->
        assert run(["--unknown-option"]) =~ "Compiling"
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
