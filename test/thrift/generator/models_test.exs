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
    1: Status status_field,
    12: Status status_field_with_default = Status.INACTIVE,
    2: map<Status, Status> status_map,
    3: set<Status> status_set,
    4: list<Status> status_list,
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

    assert Status.name?(:active) == true
    assert Status.name?(:inactive) == true
    assert Status.name?(:banned) == true
    assert Status.name?(:evil) == true
    assert Status.name?(:bamboozled) == false

    assert Status.value_to_name(1) == {:ok, :active}
    assert Status.value_to_name(2) == {:ok, :inactive}
    assert Status.value_to_name(6) == {:ok, :banned}
    assert Status.value_to_name(32) == {:ok, :evil}
    assert Status.value_to_name(65536) == {:error, {:invalid_enum_value, 65536}}

    assert Status.value_to_name!(1) == :active
    assert Status.value_to_name!(2) == :inactive
    assert Status.value_to_name!(6) == :banned
    assert Status.value_to_name!(32) == :evil
    assert_raise MatchError, fn -> Status.value_to_name!(38210) end

    assert Status.name_to_value(:active) == {:ok, 1}
    assert Status.name_to_value(:inactive) == {:ok, 2}
    assert Status.name_to_value(:banned) == {:ok, 6}
    assert Status.name_to_value(:evil) == {:ok, 32}
    assert Status.name_to_value(:just_weird) ==
      {:error, {:invalid_enum_name, :just_weird}}

    assert Status.name_to_value!(:active) == 1
    assert Status.name_to_value!(:inactive) == 2
    assert Status.name_to_value!(:banned) == 6
    assert Status.name_to_value!(:evil) == 32
    assert_raise MatchError, fn -> Status.name_to_value!(:just_weird) end

    assert Status.meta(:names) == [:active, :inactive, :banned, :evil]
    assert Status.meta(:values) == [1, 2, 6, 32]

    struct = %StructWithEnum{}
    assert struct.status_field == nil
    assert struct.status_field_with_default == Status.inactive
    assert struct.status_map == nil
    assert struct.status_set == nil
    assert struct.status_list == nil
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
    13: optional string DEPRECATED_string;
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
    assert s.deprecated_string == nil
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
    1: optional shared.MyInteger num = 5;
  }
  """

  thrift_test "includes" do
    struct = %StructWithIncludedNum{}
    assert struct.num == 5
  end

  @thrift_file name: "complex_typedefs.thrift", contents: """
  typedef set<i32> IntSet
  struct Thangs {
    1: optional IntSet numbers
  }
  """

  thrift_test "it should be able to have typedefs" do
    thang = %Thangs{numbers: MapSet.new([1, 2, 3])}
    assert thang.numbers == MapSet.new([1, 2, 3])
  end

  @thrift_file name: "empty_container.thrift", contents: """
  struct Goth {
    1: optional list<i32> empty_like_my_soul = []
    2: optional set<i32> the_abyss = []
    3: optional map<i32, string> going_nowhere = {}
  }
  """

  thrift_test "containers can have empty defaults" do
    goth = %Goth{}
    assert goth.empty_like_my_soul == []
    assert goth.the_abyss == MapSet.new
    assert goth.going_nowhere == %{}
  end

  @thrift_file name: "nonempty_container.thrift", contents: """
  struct Cargo {
    1: optional list<i32> fib = [1, 1, 2, 3, 5]
    2: optional set<i32> answers = [42]
    3: optional map<i32, string> bad_approximations = {3: "pi"}
  }
  """

  thrift_test "containers can have non-empty defaults" do
    cargo = %Cargo{}
    assert cargo.fib == [1, 1, 2, 3, 5]
    assert cargo.answers == MapSet.new([42])
    assert cargo.bad_approximations == %{3 => "pi"}
  end

  @thrift_file name: "physical.thrift", contents: """
  const i64 Pi = 3
  const double Planck = 6.62607004e-34
  const double SpeedLimit = 3.0e8
  const double One = 1.0
  """

  @thrift_file name: "businessy.thrift", contents: """
  include "empty_container.thrift"
  include "nonempty_container.thrift"

  const nonempty_container.Cargo ImportantCargo = {"bad_approximations": {1: "zero"}}
  const empty_container.Goth TimmyDoesntUnderstandGoth = {"empty_like_my_soul": [1, 2, 3]}
  """

  thrift_test "constants" do
    assert Physical.pi == 3
    assert Physical.planck == 6.62607004e-34
    assert Physical.speed_limit == 3.0e8
    assert Physical.one == 1.0

    assert Businessy.important_cargo ==
      %Cargo{bad_approximations: %{1 => "zero"}}
    assert Businessy.timmy_doesnt_understand_goth ==
      %Goth{empty_like_my_soul: [1, 2, 3]}
  end
end
