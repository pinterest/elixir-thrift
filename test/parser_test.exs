defmodule ParserTest do

  use ExUnit.Case, async: true

  import Thrift.Parser, only: [parse: 1, parse: 2]

  alias Thrift.Parser.Models.Constant
  alias Thrift.Parser.Models.Exception
  alias Thrift.Parser.Models.Field
  alias Thrift.Parser.Models.Function
  alias Thrift.Parser.Models.Include
  alias Thrift.Parser.Models.Namespace
  alias Thrift.Parser.Models.Schema
  alias Thrift.Parser.Models.Service
  alias Thrift.Parser.Models.Struct
  alias Thrift.Parser.Models.TypeRef
  alias Thrift.Parser.Models.TEnum
  alias Thrift.Parser.Models.Union
  alias Thrift.Parser.Models.ValueRef

  import ExUnit.CaptureIO

  test "parsing comments" do
    {:ok, schema} = """
    // a simple C-style comment
    """
    |> parse

    assert schema == %Schema{}
  end

  test "parsing long-comments " do
    {:ok, schema} = """
    /* This is a long comment
    *  that spans many lines
    *  which means the docs are good
    *  aren't you happy?
    */
    """ |> parse

    assert schema == %Schema{}
  end

  test "parsing a single include header" do
    includes = """
    include "foo.thrift"
    """ |> parse([:includes])

    assert includes == [%Include{path: "foo.thrift"}]
  end

  test "parsing namespace headers" do
    namespaces = """
    namespace py foo.bar.baz
    namespace erl foo_bar
    namespace * bar.baz
    """
    |> parse([:namespaces])

    assert namespaces[:py] == %Namespace{name: :py, path: "foo.bar.baz"}
    assert namespaces[:erl] == %Namespace{name: :erl, path: "foo_bar"}
    assert namespaces[:*] == %Namespace{name: :*, path: "bar.baz"}
  end

  test "parsing include headers" do
    includes = """
    include "foo.thrift"
    include "bar.thrift"
    """
    |> parse([:includes])

    assert includes == [
      %Include{path: "foo.thrift"},
      %Include{path: "bar.thrift"}
    ]
  end

  test "parsing a byte constant" do
    constant = "const i8 BYTE_CONST = 2;"
    |> parse([:constants, :BYTE_CONST])

    assert constant == %Constant{name: :BYTE_CONST,
                                 value: 2,
                                 type: :i8}
  end

  test "parsing a negative integer constant" do
    constant = "const i16 NEG_INT_CONST = -281;"
    |> parse([:constants, :NEG_INT_CONST])

    assert constant == %Constant{name: :NEG_INT_CONST,
                                 value: -281,
                                 type: :i16}
  end

  test "parsing a small int constant" do
    constant = "const i16 SMALL_INT_CONST = 65535;"
    |> parse([:constants, :SMALL_INT_CONST])

    assert constant == %Constant{name: :SMALL_INT_CONST,
                                 value: 65535,
                                 type: :i16}
  end

  test "parsing an int constant" do
    constant = "const i32 INT_CONST = 1234;"
    |> parse([:constants, :INT_CONST])

    assert constant == %Constant{name: :INT_CONST,
                                 value: 1234,
                                 type: :i32}
  end

  test "parsing a large int constant" do
    constant = "const i64 LARGE_INT_CONST = 12347437812391;"
    |> parse([:constants, :LARGE_INT_CONST])

    assert constant == %Constant{name: :LARGE_INT_CONST,
                                 value: 12347437812391,
                                 type: :i64}
  end

  test "parsing a double constant" do
    constant = "const double DOUBLE_CONST = 123.4"
    |> parse([:constants, :DOUBLE_CONST])

    assert constant == %Constant{name: :DOUBLE_CONST,
                                 value: 123.4,
                                 type: :double}
  end

  test "parsing a string constant" do
    constant = "const string STRING_CONST = \"hi\""
    |> parse([:constants, :STRING_CONST])

    assert constant == %Constant{name: :STRING_CONST,
                                 value: "hi",
                                 type: :string}
  end

  test "parsing a set constant" do
    constant = "const set<string> SET_CONST = {\"hello\", \"bye\"}"
    |> parse([:constants, :SET_CONST])

    assert constant == %Constant{name: :SET_CONST,
                                 value: MapSet.new(["hello", "bye"]),
                                 type: {:set, :string}}
  end

  test "parsing a set int constant" do
    constant = "const set<i32> SET_INT_CONST = {1, 3}"
    |> parse([:constants, :SET_INT_CONST])

    assert constant == %Constant{name: :SET_INT_CONST,
                                 value: MapSet.new([1, 3]),
                                 type: {:set, :i32}}
  end

  test "parsing a map constant" do
    constant = "const map<string, i32> MAP_CONST = {\"hello\": 1, \"world\": 2};"
    |> parse([:constants, :MAP_CONST])

    assert constant == %Constant{name: :MAP_CONST,
                                 value: %{"world" => 2, "hello" => 1},
                                 type: {:map, {:string, :i32}}}
  end

  test "parsing a list constant" do
    constant = "const list<i32> LIST_CONST = [5, 6, 7, 8]"
    |> parse([:constants, :LIST_CONST])

    assert constant == %Constant{name: :LIST_CONST,
                                 value: [5, 6, 7, 8],
                                 type: {:list, :i32}}
  end

  test "parsing an enum value constant" do
    constant = "const string SUNNY = Weather.SUNNY;"
    |> parse([:constants, :SUNNY])

    assert constant == %Constant{
      name: :SUNNY,
      value: %ValueRef{referenced_value: :"Weather.SUNNY"},
      type: :string}
  end

  test "parsing a list constant with enum values" do
    constant = """
    const list<string> WEATHER_TYPES = [
      Weather.SUNNY,
      Weather.CLOUDY,
      Weather.RAINY,
      Weather.SNOWY
    ]
    """
    |> parse([:constants, :WEATHER_TYPES])

    assert constant == %Constant{
      name: :WEATHER_TYPES,
      type: {:list, :string},
      value: [
        %ValueRef{referenced_value: :"Weather.SUNNY"},
        %ValueRef{referenced_value: :"Weather.CLOUDY"},
        %ValueRef{referenced_value: :"Weather.RAINY"},
        %ValueRef{referenced_value: :"Weather.SNOWY"},
      ]}
  end

  test "parsing a set constant with enum values" do
    constant = """
    const set<string> WEATHER_TYPES = {
      Weather.SUNNY,
      Weather.CLOUDY,
      Weather.RAINY,
      Weather.SNOWY
    }
    """
    |> parse([:constants, :WEATHER_TYPES])

    assert constant == %Constant{
      name: :WEATHER_TYPES,
      type: {:set, :string},
      value: MapSet.new([
        %ValueRef{referenced_value: :"Weather.SUNNY"},
        %ValueRef{referenced_value: :"Weather.CLOUDY"},
        %ValueRef{referenced_value: :"Weather.RAINY"},
        %ValueRef{referenced_value: :"Weather.SNOWY"},
      ])}
  end

  test "parsing a map constant with enum keys" do
    constant = """
    const map<Weather, string> weather_messages = {
      Weather.SUNNY: "Yay, it's sunny!",
      Weather.CLOUDY: "Welcome to Cleveland!",
      Weather.RAINY: "Welcome to Seattle!",
      Weather.SNOWY: "Welcome to Canada!"
    }
    """
    |> parse([:constants, :weather_messages])

    assert constant == %Constant{
      name: :weather_messages,
      type: {:map, {%TypeRef{referenced_type: :Weather}, :string}},
      value: %{
        %ValueRef{referenced_value: :"Weather.CLOUDY"} => "Welcome to Cleveland!",
        %ValueRef{referenced_value: :"Weather.RAINY"} => "Welcome to Seattle!",
        %ValueRef{referenced_value: :"Weather.SNOWY"} => "Welcome to Canada!",
        %ValueRef{referenced_value: :"Weather.SUNNY"} => "Yay, it's sunny!"}}
  end

  test "parsing a map constant with enum values as values" do
    constant = """
    const map<string, Weather> clothes_to_wear = {
      "gloves": Weather.SNOWY,
      "umbrella": Weather.RAINY,
      "sweater": Weather.CLOUDY,
      "sunglasses": Weather.SUNNY
    }
    """
    |> parse([:constants, :clothes_to_wear])

    assert constant == %Constant{
      name: :clothes_to_wear,
      type: {:map, {:string, %TypeRef{referenced_type: :Weather}}},
      value: %{
        "gloves" => %ValueRef{referenced_value: :"Weather.SNOWY"},
        "umbrella" => %ValueRef{referenced_value: :"Weather.RAINY"},
        "sweater" => %ValueRef{referenced_value: :"Weather.CLOUDY"},
        "sunglasses" => %ValueRef{referenced_value: :"Weather.SUNNY"}}}
  end

  test "parsing an enum" do
    user_status = """
    enum UserStatus {
      ACTIVE,
      INACTIVE,
      BANNED = 6,
      EVIL = 0x20
    }
    """
    |> parse([:enums, :UserStatus])

    assert user_status == %TEnum{name: :UserStatus,
                                 values: [ACTIVE: 1, INACTIVE: 2, BANNED: 6, EVIL: 32]}
  end

  test "parsing an exception" do
    program = """
    exception ApplicationException {
      1: string message,
      2: required i32 count,
      3: optional string reason
      optional string other;
      optional string fixed = "foo"
    }
    """

    warnings = capture_io(fn ->
      exc = parse(program, [:exceptions, :ApplicationException])

      assert exc == %Exception{
        name: :ApplicationException,
        fields: [
          %Field{id: 1, name: :message, type: :string},
          %Field{id: 2, name: :count, type: :i32, required: true},
          %Field{id: 3, name: :reason, type: :string, required: false},
          %Field{id: 4, name: :other, type: :string, required: false},
          %Field{id: 5, name: :fixed, type: :string, required: false, default: "foo"}
        ]}
    end)
    |> String.split("\n")
    |> Enum.map(&String.slice(&1, (5..-5)))

    warning_1 = "Warning: id not set for field 'ApplicationException.other'."
    warning_2 = "Warning: id not set for field 'ApplicationException.fixed'."

    assert warning_1 in warnings
    assert warning_2 in warnings
  end

  test "an exception with duplicate ids" do
    program = """
    exception BadEx {
     1: optional string bad,
     1: optional string evil;
    }
    """

    expected_error = "Error: BadEx.bad, BadEx.evil share field number 1."
    assert_raise RuntimeError, expected_error, fn ->
      parse(program)
    end
  end

  test "parsing a typedef" do
    typedefs = """
    typedef i64 id
    typedef string json
    typedef list<string> string_list
    """
    |> parse([:typedefs])

    assert typedefs[:id] == :i64
    assert typedefs[:string_list] == {:list, :string}
  end

  test "parsing a struct with a bool" do
    s = """
    struct MyStruct {
      1: optional bool negative;
      2: optional bool positive = true;
      3: optional bool c_positive = 1;
      4: optional bool c_negative = 0;
    }
    """
    |> parse([:structs, :MyStruct])

    assert s == %Struct{
      name: :MyStruct,
      fields: [
        %Field{id: 1, name: :negative, type: :bool, required: false, default: nil},
        %Field{id: 2, name: :positive, type: :bool, required: false, default: true},
        %Field{id: 3, name: :c_positive, type: :bool, required: false, default: true},
        %Field{id: 4, name: :c_negative, type: :bool, required: false, default: false},
      ]}
  end

  test "parsing a struct with an int" do
    s = """
    struct MyStruct {
      1: optional string name;
    }
    """
    |> parse([:structs, :MyStruct])

    assert s == %Struct{
      name: :MyStruct,
      fields: [
        %Field{id: 1, name: :name, type: :string, required: false}
      ]}
  end

  test "parsing a struct with a typedef" do
    s = """
    typedef i64 id

    struct User {
      1: required id user_id,
      2: required string username
    }
    """ |> parse([:structs, :User])

    assert s.fields == [
      %Field{id: 1, name: :user_id, required: true, type: %TypeRef{referenced_type: :id}},
      %Field{id: 2, name: :username, required: true, type: :string}
    ]
  end

  test "parsing a struct with optional things removed" do
    struct_def = """
    struct Optionals {
      string name
      i32 count,
      i64 long_thing = 12345
      optional list<i32> optional_list,
    }
    """

    warnings = capture_io(fn ->
      s = parse(struct_def, [:structs, :Optionals])

      assert s == %Struct{
        name: :Optionals,
        fields: [
          %Field{id: 1, name: :name, type: :string, required: :default},
          %Field{id: 2, name: :count, type: :i32, required: :default},
          %Field{id: 3, name: :long_thing, type: :i64, required: :default, default: 12345},
          %Field{id: 4, name: :optional_list, type: {:list, :i32}, required: false}
        ]
      }
    end)
    |> String.split("\n")
    |> Enum.map(&String.slice(&1, 5..-5)) # get rid of the terminal chars

    [:name, :count, :long_thing, :optional_list]
    |> Enum.each(fn(field_name) ->

      warning = "Warning: id not set for field 'Optionals.#{field_name}'."
      assert warning in warnings
    end)
  end

  test "parsing an empty map default value" do
    struct = """
    struct EmptyDefault {
      1: i64 id,
      2: map<string, string> myMap={},
    }
    """
    |> parse([:structs, :EmptyDefault])

    assert struct == %Struct{
      name: :EmptyDefault,
      fields: [
        %Field{default: nil, id: 1, name: :id, required: :default, type: :i64},
        %Field{default: %{}, id: 2, name: :myMap,
               required: :default, type: {:map, {:string, :string}}}
      ]}
  end

  test "when default ids conflict with explicit ids" do

    assert_raise RuntimeError, fn ->
      capture_io fn ->
        """
        struct BadFields {
          required i32 first,
          1: optional i64 other
        }
        """ |> parse
      end
    end
  end

  test "when a struct has another struct as a member" do
    user = """
    struct Name {
      1: string first_name,
      2: string last_name
    }

    struct User {
       1: i64 id,
       2: Name name,
    }
    """ |> parse([:structs, :User])
    assert user == %Struct{
      name: :User,
      fields: [
        %Field{id: 1, type: :i64, name: :id},
        %Field{id: 2, type: %TypeRef{referenced_type: :Name}, name: :name}
      ]
    }
  end

  test "parsing a union definition" do
    union = """
    union Highlander {
      1: i32 connery,
      2: i64 lambert
    }
    """
    |> parse([:unions, :Highlander])

    assert union == %Union{
      name: :Highlander,
      fields: [
        %Field{id: 1, name: :connery, type: :i32, required: false},
        %Field{id: 2, name: :lambert, type: :i64, required: false},
      ]
    }
  end

  test "a union definition makes sure its field ids aren't repeated" do
    capture_io fn ->
      assert_raise RuntimeError, fn ->
      """
        union Highlander {
          i32 connery,
          1: i64 lambert
        }
      """
      |> parse
      end

    end
  end

  test "defining a simple service" do
    service = """
    service MyService {
      void hi()
    }
    """
    |> parse([:services, :MyService])

    assert service == %Service{
      name: :MyService,
      functions: %{hi: %Function{name: :hi, return_type: :void, params: []}}
    }
  end

  test "defining a service with a complex return type and params" do
    service = """
    struct User {
    }

    service MyService {
       map<string, i64> usernames_to_ids(1: User user)
    }
    """
    |> parse([:services, :MyService])

    assert service == %Service{
      name: :MyService,
      functions: %{usernames_to_ids:
        %Function{name: :usernames_to_ids, oneway: false, return_type: {:map, {:string, :i64}},
                  params: [%Field{id: 1, name: :user, type: %TypeRef{referenced_type: :User}}]
                 }
      }
    }
  end

  test "a oneway function in a service" do
    service = """
    service OneWay {
      oneway void fireAndForget(1: i64 value);
    }
    """
    |> parse([:services, :OneWay])

    assert service == %Service{
      name: :OneWay,
      functions: %{
        fireAndForget: %Function{
          name: :fireAndForget, oneway: true, return_type: :void,
          params: [
            %Field{id: 1, name: :value, type: :i64}
          ]
        }
      }
    }
  end

  test "a service that throws exceptions" do
    service = """
    exception ServiceException {
      1: i32 error_code = 0,
      2: string reason = "Unknown"
    }

    service Thrower {
       oneway void blowup() throws (1: ServiceException svc)
    }
    """
    |> parse([:services, :Thrower])

    %{blowup: function} = service.functions
    assert function.exceptions == [
      %Field{id: 1, name: :svc, type: %TypeRef{referenced_type: :ServiceException}}
    ]
  end

  test "a service with several functions" do
    code = """
    struct User {
      1: i64 id,
      2: string username
    }

    exception ServiceException {
      1: string message
      2: i32 code
    }

    service MultipleFns {
       void ping(),
       oneway void update(1: i64 user_id, string field, string value),
       map<i64, User> get_users(1: set<i64> user_ids) throws (1: ServiceException svc)
    }
    """
    capture_io fn ->
      service = parse(code, [:services, :MultipleFns])

      %{ping: ping, update: update, get_users: get_users} = service.functions

      assert ping == %Function{name: :ping}
      assert update == %Function{
        oneway: true,
        name: :update,
        params: [
          %Field{id: 1, name: :user_id, type: :i64},
          %Field{id: 2, name: :field, type: :string},
          %Field{id: 3, name: :value, type: :string}
        ]
      }
      assert get_users == %Function{
        name: :get_users,
        exceptions: [
          %Field{id: 1, name: :svc, type: %TypeRef{referenced_type: :ServiceException}}
        ],
        params: [
          %Field{id: 1, name: :user_ids, type: {:set, :i64}},
        ],
        return_type: {:map, {:i64, %TypeRef{referenced_type: :User}}}
      }
    end
  end

  test "a service extends another" do
    services = """
    service Pinger {
      boolean ping()
    }

    service Extender (extends Pinger)  {
      boolean is_ready()
    }

    service OtherExtender extends Pinger {
      boolean has_failed()
    }
    """
    |> parse([:services])

    pinger = services[:Extender]
    other  = services[:OtherExtender]

    assert pinger.extends == :Pinger
    assert other.extends == :Pinger
  end

  test "parsing a real thrift file" do
    # just make sure we don't blow up on parse and can parse
    # complex thrift files.

    File.read!("./test/fixtures/app/thrift/tutorial.thrift")
    |> parse

    File.read!("./test/fixtures/app/thrift/shared.thrift")
    |> parse
  end
end
