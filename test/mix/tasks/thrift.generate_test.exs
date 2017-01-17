defmodule Mix.Tasks.Thrift.GenerateTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  @project_root Path.expand("../../../", __DIR__)
  @fixture_project Path.join(@project_root, "test/fixtures/app")

  setup do
    in_fixture(fn -> File.rm_rf!("lib") end)
    :ok
  end

  test "not specifying any Thrift files" do
    in_fixture fn ->
      assert run([]) == ""
    end
  end

  test "specifying multiple Thrift files" do
    in_fixture fn ->
      output = run(["thrift/simple.thrift", "thrift/tutorial.thrift"])
      assert output =~ "Reading thrift/simple.thrift"
      assert output =~ "Reading thrift/tutorial.thrift"
      assert File.exists?("lib/shared/shared_struct.ex")
      assert File.exists?("lib/tutorial/calculator.ex")
    end
  end

  test "specifying a non-existent Thrift file" do
    in_fixture fn ->
      assert_raise Mix.Error, ~r/could not read file/, fn ->
        run(["missing.thrift"])
      end
    end
  end

  test "specifying an invalid Thrift file" do
    in_fixture fn ->
      assert_raise Mix.Error, ~r/Error parsing/, fn ->
        run([__ENV__.file])
      end
    end
  end

  test "specifying an alternate output directory (--out)" do
    in_fixture fn ->
      run(["--out", "thrift_lib", "thrift/simple.thrift"])
      assert File.exists?("thrift_lib/shared/shared_struct.ex")
      File.rm_rf!("thrift_lib")
    end
  end

  defp run(args) when is_list(args), do: run(:stdio, args)
  defp run(device, args) when device in [:stdio, :stderr] and is_list(args) do
    args = ["--verbose" | args]
    capture_io(device, fn -> Mix.Tasks.Thrift.Generate.run(args) end)
  end

  defp in_fixture(fun) do
    File.cd!(@fixture_project, fun)
  end
end
