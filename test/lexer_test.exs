defmodule LexerTest do
  use ExUnit.Case, async: true

  defp tokenize(s) do
    {:ok, value, _} = :thrift_lexer.string(String.to_char_list(s))
    value
  end

  test "whitespace" do
    assert tokenize(" ") == []
  end

  test "comments" do
    assert tokenize("# foo") == []
    assert tokenize("// foo") == []
    assert tokenize("/* foo */") == []
    assert tokenize("
    /*
      foo
    */") == []
  end

  test "symbols" do
    assert tokenize(";") == []
    assert tokenize("=") == [{:=, 1}]
  end

  test "keywords" do
    keywords = ~w(
      namespace include
      void bool byte i8 i16 i64 double string binary list map set
      typedef enum struct union exception
      const oneway extends throws service required optional
    )
    for keyword <- keywords do
      value = keyword |> tokenize |> Keyword.keys |> List.first
      assert value == String.to_atom(keyword)
    end
  end

  test "boolean literals" do
    assert tokenize("true") == [true: 1]
    assert tokenize("false") == [false: 1]
  end

  test "integer literals" do
    assert tokenize("0") == [{:int, 1, 0}]
    assert tokenize("10") == [{:int, 1, 10}]
    assert tokenize("+10") == [{:int, 1, 10}]
    assert tokenize("-10") == [{:int, 1, -10}]
  end

  test "hex literals" do
    assert tokenize("0x0") == [{:int, 1, 0}]
    assert tokenize("0xff") == [{:int, 1, 255}]
    assert tokenize("+0xc0c0c0") == [{:int, 1, 12632256}]
    assert tokenize("-0xc0c0c0") == [{:int, 1, -12632256}]
  end

  test "double literals" do
    assert tokenize("0.0") == [{:double, 1, 0.0}]
    assert tokenize("3.14") == [{:double, 1, 3.14}]
    assert tokenize("6.02e23") == [{:double, 1, 6.02e23}]
  end

  test "string literals" do
    assert tokenize("''") == [{:string, 1, ''}]
    assert tokenize("'foo'") == [{:string, 1, 'foo'}]
    assert tokenize("\"foo\"") == [{:string, 1, 'foo'}]
    assert tokenize("'foo\nbar'") == [{:string, 1, 'foo\nbar'}]
    assert tokenize("'\\'") == [{:string, 1, '\\'}]
    assert tokenize("'\"\"'") == [{:string, 1, '""'}]
  end

  test "list literals" do
    assert tokenize("[]") == [{:"[", 1}, {:"]", 1}]
    assert tokenize("[1]") == [{:"[", 1}, {:int, 1, 1}, {:"]", 1}]
    assert tokenize("[1,2]") == [
     {:"[", 1},
     {:int, 1, 1},
     {:",", 1},
     {:int, 1, 2},
     {:"]", 1}]
  end

  test "map literals" do
    assert tokenize("{}") == [{:"{", 1}, {:"}", 1}]
    assert tokenize("{'hello':'world'}") == [
      {:"{", 1},
      {:string, 1, 'hello'}, {:":", 1}, {:string, 1, 'world'},
      {:"}", 1}]

    assert tokenize("{'a':1,'b':2}") == [
      {:"{", 1},
      {:string, 1, 'a'}, {:":", 1}, {:int, 1, 1},
      {:",", 1},
      {:string, 1, 'b'}, {:":", 1}, {:int, 1, 2},
      {:"}", 1}]
  end

  test "set literals" do
    assert tokenize("{}") == [{:"{", 1}, {:"}", 1}]
    assert tokenize("{1}") == [{:"{", 1}, {:int, 1, 1}, {:"}", 1}]
    assert tokenize("{1,2}") == [
      {:"{", 1},
      {:int, 1, 1},
      {:",", 1},
      {:int, 1, 2},
      {:"}", 1}]
  end

  test "identifiers" do
    assert tokenize("abc_123.xzy") == [{:ident, 1, 'abc_123.xzy'}]
    assert tokenize("abc-") == [{:st_ident, 1, 'abc-'}]
  end

  test "typedef" do
    assert tokenize("typedef i32 Integer") == [{:typedef, 1}, {:i32, 1}, {:ident, 1, 'Integer'}]
  end

  test "enum definition" do
    assert tokenize("""
      enum Operation {
        ADD = 1,
        SUBTRACT = 2,
        MULTIPLY = 3,
        DIVIDE = 4
      }
    """) == [
      {:enum, 1}, {:ident, 1, 'Operation'},
      {:"{", 1},
      {:ident, 2, 'ADD'}, {:=, 2}, {:int, 2, 1},
      {:",", 2},
      {:ident, 3, 'SUBTRACT'}, {:=, 3}, {:int, 3, 2},
      {:",", 3},
      {:ident, 4, 'MULTIPLY'}, {:=, 4}, {:int, 4, 3},
      {:",", 4},
      {:ident, 5, 'DIVIDE'}, {:=, 5}, {:int, 5, 4},
      {:"}", 6}]
  end

  test "struct definition" do
    assert tokenize("""
      struct Work {
        1: i32 num1 = 0,
        2: i32 num2,
        3: Operation op,
        4: optional string comment,
      }
    """) == [
      {:struct, 1}, {:ident, 1, 'Work'},
      {:"{", 1},
      {:int, 2, 1}, {:":", 2}, {:i32, 2}, {:ident, 2, 'num1'}, {:=, 2}, {:int, 2, 0},
      {:",", 2},
      {:int, 3, 2}, {:":", 3}, {:i32, 3}, {:ident, 3, 'num2'},
      {:",", 3},
      {:int, 4, 3}, {:":", 4}, {:ident, 4, 'Operation'}, {:ident, 4, 'op'},
      {:",", 4},
      {:int, 5, 4}, {:":", 5}, {:optional, 5}, {:string, 5}, {:ident, 5, 'comment'},
      {:",", 5},
      {:"}", 6}]
  end

  test "exception definition" do
    assert tokenize("""
      exception InvalidOperation {
        1: i32 whatOp,
        2: string why
      }
    """) == [
      {:exception, 1}, {:ident, 1, 'InvalidOperation'},
      {:"{", 1},
      {:int, 2, 1}, {:":", 2}, {:i32, 2}, {:ident, 2, 'whatOp'},
      {:",", 2},
      {:int, 3, 2}, {:":", 3}, {:string, 3}, {:ident, 3, 'why'},
      {:"}", 4}]
  end

  test "service definition" do
    assert tokenize("""
      service Calculator extends shared.SharedService {
        void ping(),
        i32 add(1:i32 num1, 2:i32 num2),
        oneway void zip()
      }
    """) == [
      {:service, 1}, {:ident, 1, 'Calculator'}, {:extends, 1}, {:ident, 1, 'shared.SharedService'},
      {:"{", 1},
      {:void, 2}, {:ident, 2, 'ping'}, {:"(", 2}, {:")", 2},
      {:",", 2},
      {:i32, 3}, {:ident, 3, 'add'},
        {:"(", 3},
          {:int, 3, 1}, {:":", 3}, {:i32, 3}, {:ident, 3, 'num1'},
          {:",", 3},
          {:int, 3, 2}, {:":", 3}, {:i32, 3}, {:ident, 3, 'num2'},
        {:")", 3},
      {:",", 3},
      {:oneway, 4}, {:void, 4}, {:ident, 4, 'zip'}, {:"(", 4}, {:")", 4},
      {:"}", 5}]
  end

  test "namespace definition" do
    assert tokenize("""
    namespace py foo.bar.baz
    namespace java com.pinterest.foo.bar.baz
    namespace * foo.bar
    """) ==
      [
        {:namespace, 1}, {:ident, 1, 'py'}, {:ident, 1, 'foo.bar.baz'},
        {:namespace, 2}, {:ident, 2, 'java'}, {:ident, 2, 'com.pinterest.foo.bar.baz'},
        {:namespace, 3}, {:*, 3}, {:ident, 3, 'foo.bar'}
      ]
  end
end
