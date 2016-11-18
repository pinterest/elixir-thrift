defmodule Thrift.Generator.ModelsTest do
  use ThriftTestCase, cleanup: false

  @thrift_file name: "enums.thrift", contents: """
  enum Status {
    ACTIVE,
    INACTIVE,
    BANNED = 6,
    EVIL = 0x20,
  }
  struct StructWithEnum {
    1: Status status
  }
  """

  thrift_test "generating enum" do
    assert Status.active == 1
    assert Status.inactive == 2
    assert Status.banned == 6
    assert Status.evil == 32

    assert Status.member?(0) == false
    assert Status.member?(1) == true
    assert Status.member?(2) == true
    assert Status.member?(3) == false
    assert Status.member?(4) == false
    assert Status.member?(5) == false
    assert Status.member?(6) == true
    assert Status.member?(7) == false

    struct = %StructWithEnum{}
    assert struct.status == Status.active
  end

  @thrift_file name: "exceptions.thrift", contents: """
  exception ApplicationException {
    1: string message,
    2: required i32 count,
    3: optional string reason
    optional string other;
    optional string fixed = "foo"
  }
  """

  thrift_test "generating exception" do
    e = %ApplicationException{}
    assert e.message == ""
    assert e.count == 0
    assert e.reason == ""
    assert e.other == ""
    assert e.fixed == "foo"
  end

  @thrift_file name: "struct_includes.thrift", contents: """
  struct RemoteStruct {
    1: optional i32 num;
  }
  """

  @thrift_file name: "structs.thrift", contents: """
  include "struct_includes.thrift"
  struct LocalStruct {
    1: optional i32 num;
  }
  struct MyStruct {
    1: optional string name;
    2: optional i32 num1;
    3: optional i32 num2 = 5;
    4: optional bool b1;
    5: optional bool b2 = true;
    6: optional LocalStruct local_struct;
    7: optional struct_includes.RemoteStruct remote_struct;
  }
  """

  thrift_test "generating struct" do
    s = %MyStruct{}
    assert s.name == ""
    assert s.num1 == 0
    assert s.num2 == 5
    assert s.b1 == false
    assert s.b2 == true
    assert s.local_struct == %LocalStruct{}
    assert s.remote_struct == nil
  end

  @thrift_file name: "typedefs.thrift", contents: """
  typedef i32 MyInteger
  typedef string MyString
  typedef MyInteger DefinitelyNumber

  struct StructWithTypedefs {
    1: optional MyString str;
    2: optional MyInteger num1 = 1;
    3: optional DefinitelyNumber num2 = 2;
  }
  """

  thrift_test "generating typedefs" do
    s = %StructWithTypedefs{}
    assert s.str == ""
    assert s.num1 == 1
    assert s.num2 == 2
  end

  @thrift_file name: "shared.thrift", contents: """
  typedef i32 MyInteger
  """

  @thrift_file name: "includes.thrift", contents: """
  include "shared.thrift"

  struct StructWithIncludedNum {
    1: optional MyInteger num = 5;
  }
  """

  thrift_test "includes" do
    struct = %StructWithIncludedNum{}
    assert struct.num == 5
  end
end
