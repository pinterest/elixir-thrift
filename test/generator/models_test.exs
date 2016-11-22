defmodule Thrift.Generator.ModelsTest do
  use ThriftTestCase

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
    4: optional string other;
    5: optional string fixed = "foo"
  }
  """

  thrift_test "generating exception" do
    e = %ApplicationException{}
    assert e.message == nil
    assert e.count == nil
    assert e.reason == nil
    assert e.other == nil
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
    1: optional bool my_bool;
    2: optional byte my_byte;
    3: optional double my_double;
    4: optional i8 my_i8;
    5: optional i16 my_i16;
    6: optional i32 my_i32;
    7: optional i64 my_i64;
    8: optional string my_string;
    9: optional LocalStruct local_struct;
    10: optional struct_includes.RemoteStruct remote_struct;
    11: optional list<LocalStruct> local_struct_list;
    12: optional map<LocalStruct, LocalStruct> local_struct_map;
  }
  """

  thrift_test "generating struct" do
    s = %MyStruct{}
    assert s.my_bool == nil
    assert s.my_byte == nil
    assert s.my_double == nil
    assert s.my_i8 == nil
    assert s.my_i16 == nil
    assert s.my_i32 == nil
    assert s.my_i64 == nil
    assert s.my_string == nil
    assert s.local_struct == nil
    assert s.remote_struct == nil
    assert s.local_struct_list == nil
    assert s.local_struct_map == nil
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
    assert s.str == nil
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
