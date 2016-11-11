defmodule Mix.Tasks.Thrift.GenerateTest do
  use ExUnit.Case

  import Mix.Tasks.Thrift.Generate, only: [run: 1]
  import ExUnit.CaptureIO

  setup %{test: test} do
    dir = Path.join([System.tmp_dir!, to_string(__MODULE__), to_string(test)])
    File.rm_rf!(dir)
    File.mkdir_p!(dir)
    {:ok, dir: dir}
  end

  test "generates files", %{dir: dir} do
    File.write! "#{dir}/shared.thrift", """
      namespace elixir shared

      struct SharedStruct {
        1: i32 key
        2: string value
      }

      exception SharedException {
        1: string message,
        2: i32 code
      }

      service SharedService {
        SharedStruct getStruct(1: i32 key)
      }
      """

    File.write! "#{dir}/tutorial.thrift", """
      include "shared.thrift"

      namespace elixir tutorial

      typedef i32 MyInteger

      const i32 INT32CONSTANT = 9853
      const map<string,string> MAPCONSTANT = {'hello':'world', 'goodnight':'moon'}

      enum Operation {
        ADD = 1,
        SUBTRACT = 2,
        MULTIPLY = 3,
        DIVIDE = 4
      }

      struct Work {
        1: i32 num1 = 0,
        2: i32 num2,
        3: Operation op,
        4: optional string comment,
      }

      exception InvalidOperation {
        1: i32 whatOp,
        2: string why
      }
      """

    output = capture_io(fn -> run(["--thrift-dir", dir, "--output-dir", dir]) end)

    assert output == """
      ==> thrift
      Generated shared/shared_struct.ex
      Generated shared/shared_exception.ex
      Generated shared/shared_struct.ex
      Generated shared/shared_exception.ex
      Generated tutorial/operation.ex
      Generated tutorial/work.ex
      Generated tutorial/invalid_operation.ex
      """

    assert File.exists? "#{dir}/shared/shared_struct.ex"
    assert File.exists? "#{dir}/tutorial/invalid_operation.ex"
  end
end
