defmodule LexerTest do
  use ExUnit.Case

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
    assert tokenize(";") == [{:symbol, 1, ';'}]
    assert tokenize("=") == [{:symbol, 1, '='}]
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

  test "identifiers" do
    assert tokenize("abc_123.xzy") == [{:ident, 1, 'abc_123.xzy'}]
    assert tokenize("abc-") == [{:st_ident, 1, 'abc-'}]
  end
end
