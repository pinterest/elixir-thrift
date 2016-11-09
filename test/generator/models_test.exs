defmodule Thrift.Generator.ModelsTest do
  use ExUnit.Case, async: true

  import Thrift.Parser, only: [parse: 1]
  import Thrift.Generator.Models

  test "generating enum" do
    thrift = """
      enum UserStatus {
        ACTIVE,
        INACTIVE,
        BANNED = 6,
        EVIL = 0x20
      }
      """
    files = thrift |> parse |> generate
    assert [{"user_status.ex", contents}] = files
    assert contents == String.trim """
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

  test "generating exception" do
    thrift = """
      exception ApplicationException {
        1: string message,
        2: required i32 count,
        3: optional string reason
        optional string other;
        optional string fixed = "foo"
      }
      """
    files = thrift |> parse |> generate
    assert [{"application_exception.ex", contents}] = files
    assert contents == String.trim """
      defmodule(ApplicationException) do
        @moduledoc("Auto-generated Thrift exception ApplicationException")
        defstruct(message: "", count: 0, reason: "", other: "", fixed: "foo")
      end
      """
  end

  test "generating struct" do
    thrift = """
      struct MyStruct {
        1: optional string name;
        2: optional i32 num1;
        3: optional i32 num2 = 5;
        4: optional bool b1;
        # Not yet supported.
        # 5: optional bool b2 = true;
      }
      """
    files = thrift |> parse |> generate
    assert [{"my_struct.ex", contents}] = files
    assert contents == String.trim """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(name: "", num1: 0, num2: 5, b1: false)
      end
      """
  end

  test "typedefs" do
    thrift = """
      typedef i32 MyInteger
      typedef string MyString
      struct MyStruct {
        1: optional MyInteger num;
        2: optional MyString str;
      }
      """
    files = thrift |> parse |> generate
    assert [{"my_struct.ex", contents}] = files
    assert contents == String.trim """
      defmodule(MyStruct) do
        @moduledoc("Auto-generated Thrift struct MyStruct")
        defstruct(num: 0, str: "")
      end
      """
  end
end
