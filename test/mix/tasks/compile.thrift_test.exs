defmodule Mix.Tasks.Compile.ThriftTest do
  use MixTest.Case

  test "compiling default :files" do
    in_fixture(fn ->
      with_project_config([], fn ->
        assert Mix.Tasks.Compile.Thrift.run(["--verbose"]) == {:ok, []}

        assert_received {:mix_shell, :info, ["Compiling 4 files (.thrift)"]}
        assert_received {:mix_shell, :info, ["Compiled thrift/AnnotationTest.thrift"]}
        assert_received {:mix_shell, :info, ["Compiled thrift/StressTest.thrift"]}
        assert_received {:mix_shell, :info, ["Compiled thrift/ThriftTest.thrift"]}
        assert_received {:mix_shell, :info, ["Compiled thrift/numbers.thrift"]}

        assert File.exists?("lib/generated/service.ex")
        assert File.exists?("lib/thrift_test/thrift_test.ex")
        assert File.exists?("lib/tutorial/numbers.ex")
      end)
    end)
  end

  test "recompiling unchanged targets" do
    in_fixture(fn ->
      with_project_config([], fn ->
        assert Mix.Tasks.Compile.Thrift.run([]) == {:ok, []}
        assert Mix.Tasks.Compile.Thrift.run([]) == {:noop, []}
      end)
    end)
  end

  test "recompiling stale targets" do
    in_fixture(fn ->
      with_project_config([], fn ->
        assert Mix.Tasks.Compile.Thrift.run([]) == {:ok, []}
        File.rm_rf!("lib")
        assert Mix.Tasks.Compile.Thrift.run([]) == {:ok, []}
      end)
    end)
  end

  test "forcing compilation" do
    in_fixture(fn ->
      with_project_config([], fn ->
        assert Mix.Tasks.Compile.Thrift.run([]) == {:ok, []}
        assert Mix.Tasks.Compile.Thrift.run(["--force"]) == {:ok, []}
      end)
    end)
  end

  test "cleaning generated files" do
    in_fixture(fn ->
      with_project_config([], fn ->
        Mix.Tasks.Compile.Thrift.run([])
        assert File.exists?("lib/thrift_test/thrift_test.ex")
        assert Enum.all?(Mix.Tasks.Compile.Thrift.manifests(), &File.exists?/1)

        Mix.Tasks.Compile.Thrift.clean()
        refute File.exists?("lib/thrift_test/thrift_test.ex")
        refute Enum.any?(Mix.Tasks.Compile.Thrift.manifests(), &File.exists?/1)
      end)
    end)
  end

  test "specifying an empty :files list" do
    in_fixture(fn ->
      with_project_config([thrift: [files: []]], fn ->
        assert Mix.Tasks.Compile.Thrift.run([]) == {:noop, []}
      end)
    end)
  end

  test "specifying a non-existent Thrift file" do
    in_fixture(fn ->
      with_project_config([thrift: [files: ~w("missing.thrift")]], fn ->
        assert {:error, [_]} = Mix.Tasks.Compile.Thrift.run([])
        assert_received {:mix_shell, :error, [_]}
      end)
    end)
  end

  test "specifying an invalid Thrift file" do
    in_fixture(fn ->
      with_project_config([thrift: [files: [__ENV__.file]]], fn ->
        assert {:error, [_]} = Mix.Tasks.Compile.Thrift.run([])
        assert_received {:mix_shell, :error, [_]}
      end)
    end)
  end

  test "specifying an additional include path" do
    config = [
      files: ~w(thrift/include/Include.thrift),
      include_paths: ~w(thrift)
    ]

    in_fixture(fn ->
      with_project_config([thrift: config], fn ->
        assert Mix.Tasks.Compile.Thrift.run(["--verbose"]) == {:ok, []}
        assert_received {:mix_shell, :info, ["Compiled thrift/include/Include.thrift"]}
      end)
    end)
  end

  test "specifying an unknown option" do
    in_fixture(fn ->
      with_project_config([], fn ->
        assert Mix.Tasks.Compile.Thrift.run(["--unknown-option"]) == {:ok, []}
        assert_received {:mix_shell, :info, ["Compiling 4 files (.thrift)"]}
      end)
    end)
  end
end
