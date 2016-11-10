defmodule Thrift.Generator.ModelsTest do
  use ExUnit.Case, async: true

  import Thrift.Generator.Models

  setup %{test: test} do
    dir = Path.join([System.tmp_dir!, to_string(__MODULE__), to_string(test)])
    File.rm_rf!(dir)
    File.mkdir_p!(dir)
    on_exit fn ->
      # IO.inspect dir
      File.rm_rf!(dir)
      :ok
    end
    {:ok, dir: dir}
  end

  defmacro assert_generated(filename, expected_contents) do
    quote do
      assert String.trim(File.read!(unquote(filename))) == String.trim(unquote(expected_contents))
    end
  end

  test "generating enum", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      enum UserStatus {
        ACTIVE,
        INACTIVE,
        BANNED = 6,
        EVIL = 0x20
      }
      """
    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/user_status.ex", """
      defmodule(UserStatus) do
        @moduledoc("Auto-generated Thrift enum UserStatus")
        defmacro(active) do
          1
        end
        defmacro(inactive) do
          2
        end
        defmacro(banned) do
          6
        end
        defmacro(evil) do
          32
        end
      end
      """
  end

  test "generating exception", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      exception ApplicationException {
        1: string message,
        2: required i32 count,
        3: optional string reason
        optional string other;
        optional string fixed = "foo"
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/application_exception.ex", """
      defmodule(ApplicationException) do
        @moduledoc("Auto-generated Thrift exception ApplicationException")
        defstruct(message: "", count: 0, reason: "", other: "", fixed: "foo")
      end
      """
  end

  test "generating struct", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      struct MyStruct {
        1: optional string name;
        2: optional i32 num1;
        3: optional i32 num2 = 5;
        4: optional bool b1;
        # Not yet supported.
        # 5: optional bool b2 = true;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my_struct.ex", """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(name: "", num1: 0, num2: 5, b1: false)
      end
      """
  end

  test "typedefs", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      typedef i32 MyInteger
      typedef string MyString
      struct MyStruct {
        1: optional MyInteger num;
        2: optional MyString str;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my_struct.ex", """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(num: 0, str: "")
      end
      """
  end

  test "transitive typedefs", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      typedef i32 MyInteger
      typedef MyInteger DefinitelyNumber
      struct MyStruct {
        1: optional DefinitelyNumber num;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my_struct.ex", """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(num: 0)
      end
      """
  end

  test "namespaces", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      namespace elixir my.project.namespace
      struct MyStruct {
        1: optional i32 num;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my/project/namespace/my_struct.ex", """
      defmodule(My.Project.Namespace.MyStruct) do
        @moduledoc("Auto-generated Thrift struct My.Project.Namespace.MyStruct")
        defstruct(num: 0)
      end
      """
  end

  test "includes", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      include "shared.thrift"
      struct MyStruct {
        1: optional shared.MyInteger num;
      }
      """

    File.write! "#{dir}/shared.thrift", """
      typedef i32 MyInteger
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my_struct.ex", """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(num: 0)
      end
      """
  end

  test "struct uses local enum", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      enum UserStatus {
        ACTIVE,
        INACTIVE,
        BANNED = 6,
        EVIL = 0x20
      }

      struct MyStruct {
        1: optional UserStatus status;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/my_struct.ex", """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(status: 1)
      end
      """
  end

  test "struct containing another struct", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      struct InnerStruct {
        1: optional i32 num;
      }
      struct OuterStruct {
        1: optional InnerStruct inner;
      }
      """

    generate! "#{dir}/test.thrift", dir

    assert_generated "#{dir}/outer_struct.ex", """
      defmodule(OuterStruct) do
        @moduledoc("Auto-generated Thrift struct OuterStruct")
        defstruct(inner: %InnerStruct{})
      end
      """
  end

  test "unknown type", %{dir: dir} do
    File.write! "#{dir}/test.thrift", """
      struct MyStruct {
        1: optional Foo num;
      }
      """

    assert_raise RuntimeError, fn ->
      generate! "#{dir}/test.thrift", dir
    end
  end
end
