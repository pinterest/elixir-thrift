defmodule Thrift.Generator.BinaryProtocolTest do
  use ExUnit.Case, async: true

  import Thrift.Generator.Models

  setup %{test: test} do
    dir = [System.tmp_dir!, __MODULE__, test]
    |> Enum.map(&to_string/1)
    |> Path.join

    File.rm_rf!(dir)
    File.mkdir_p!(dir)
    on_exit fn ->
      File.rm_rf!(dir)
    end
    {:ok, dir: dir}
  end

  def load_generated_file(filename) do
    try do
      Code.eval_file(filename)
    rescue exception ->
      File.read!(filename) |> IO.puts
      flunk inspect(exception)
    end
  end

  test "generating struct", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      struct MyStruct {
        1: optional string name = "asdf";
        2: optional byte num8 = 8;
        3: optional i16 num16 = 16;
        4: optional i32 num32 = 32;
        5: optional i64 num64 = 64;
        6: optional bool b1;
      }
      """

    generate! "#{dir}/test.thrift", dir
    load_generated_file "#{dir}/my_struct.ex"

    struct = MyStruct.new
    binary = MyStruct.BinaryProtocol.serialize(:struct, struct) |> :erlang.iolist_to_binary
    assert binary == <<
      # string field 1 = "asdf"
      11, 0, 1, 0, 0, 0, 4, ?a, ?s, ?d, ?f,

      # i32 field 2 = 8
      3, 0, 2, 8,

      # i32 field 3 = 16
      6, 0, 3, 0, 16,

      # i32 field 4 = 32
      8, 0, 4, 0, 0, 0, 32,

      # i64 field 5 = 64
      10, 0, 5, 0, 0, 0, 0, 0, 0, 0, 64,

      # bool field 6 = false
      2, 0, 6, 0,

      # stop
      0>>

    assert {^struct, ""} = MyStruct.BinaryProtocol.deserialize(binary)
  end


  test "list of lists", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      namespace elixir #{__MODULE__};
      struct ListOfLists {
        1: optional list<list<i32>> outer;
      }
      """
    generate! "#{dir}/test.thrift", dir
    load_generated_file "#{dir}/thrift/generator/binary_protocol_test/list_of_lists.ex"
    alias __MODULE__.ListOfLists

    struct = %{ListOfLists.new | outer: [[1, 2, 3], [4, 5, 6]]}
    binary = ListOfLists.BinaryProtocol.serialize(:struct, struct) |> :erlang.iolist_to_binary
    assert {^struct, ""} = ListOfLists.BinaryProtocol.deserialize(binary)
  end
end
