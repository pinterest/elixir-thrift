defmodule Thrift.Parser.NimbleTest do
  use ExUnit.Case, async: true
  import Thrift.Parser.Nimble, only: [parse_token: 1]

  describe "parse_token/1" do
    test "returns ok on integer" do
      assert parse_token("111") == {:ok, [111], "", %{}, {1, 0}, 3}
      assert parse_token("-111") == {:ok, [-111], "", %{}, {1, 0}, 4}
      assert parse_token("+111") == {:ok, [111], "", %{}, {1, 0}, 4}
    end

    test "returns error on invalid partial number" do
      assert parse_token("-A") == {:error, "expected number", "A", %{}, {1, 0}, 1}
      assert parse_token("+") == {:error, "expected number", "", %{}, {1, 0}, 1}
    end

    test "returns ok on hex" do
      assert parse_token("0x1F") == {:ok, [31], "", %{}, {1, 0}, 4}
      assert parse_token("-0x1a2") == {:ok, [-418], "", %{}, {1, 0}, 6}
      assert parse_token("+0x0FF0") == {:ok, [4080], "", %{}, {1, 0}, 7}
    end

    test "returns error on invalid partial hex" do
      assert parse_token("0xG") == {:error, "expected hexidecimal digit", "G", %{}, {1, 0}, 2}
      assert parse_token("0x") == {:error, "expected hexidecimal digit", "", %{}, {1, 0}, 2}
    end

    test "returns ok on double" do
      assert parse_token("0.0") == {:ok, [0.0], "", %{}, {1, 0}, 3}
      assert parse_token("-1.0") == {:ok, [-1.0], "", %{}, {1, 0}, 4}
      assert parse_token("+1.0") == {:ok, [1.0], "", %{}, {1, 0}, 4}
      assert parse_token("1e0") == {:ok, [1.0], "", %{}, {1, 0}, 3}
      assert parse_token("-2E1") == {:ok, [-20.0], "", %{}, {1, 0}, 4}
      assert parse_token("+3.2e1") == {:ok, [32.0], "", %{}, {1, 0}, 6}
      assert parse_token("43.2E-1") == {:ok, [4.32], "", %{}, {1, 0}, 7}
      assert parse_token("-5.432E+1") == {:ok, [-54.32], "", %{}, {1, 0}, 9}
    end

    test "returns error on invalid partial double" do
      assert parse_token("0.a") ==
               {:error, "expected integer fraction for significand", "a", %{}, {1, 0}, 2}

      assert parse_token("1.") ==
               {:error, "expected integer fraction for significand", "", %{}, {1, 0}, 2}

      assert parse_token("0e!") == {:error, "expected integer exponent", "!", %{}, {1, 0}, 2}
      assert parse_token("0E+e") == {:error, "expected integer exponent", "+e", %{}, {1, 0}, 2}
      assert parse_token("0E") == {:error, "expected integer exponent", "", %{}, {1, 0}, 2}
    end

    test "returns ok on literal" do
      assert parse_token(~s("hi")) == {:ok, ["hi"], "", %{}, {1, 0}, 4}
      assert parse_token(~s('hello')) == {:ok, ["hello"], "", %{}, {1, 0}, 7}
      assert parse_token(~s("hi 'world'")) == {:ok, ["hi 'world'"], "", %{}, {1, 0}, 12}
      assert parse_token(~s("hi \\"world\\"")) == {:ok, ["hi \"world\""], "", %{}, {1, 0}, 14}
      assert parse_token(~s('hello \\'world\\'')) == {:ok, ["hello 'world'"], "", %{}, {1, 0}, 17}
    end

    test "returns error on invalid partial literal" do
      assert parse_token(~s("hi)) ==
               {:error, "expected literal delimiter ?\"", "", %{}, {1, 0}, 3}

      assert parse_token(~s("hello) <> <<128>>) ==
               {:error, "expected utf8 codepoint", <<128>>, %{}, {1, 0}, 6}
    end

    test "returns ok on identifier" do
      assert parse_token("hi") == {:ok, [[:hi]], "", %{}, {1, 0}, 2}
      assert parse_token("Hello") == {:ok, [[:Hello]], "", %{}, {1, 0}, 5}
      assert parse_token("_hey") == {:ok, [[:_hey]], "", %{}, {1, 0}, 4}
      assert parse_token("hello.world") == {:ok, [[:hello, :world]], "", %{}, {1, 0}, 11}
    end

    test "returns error on invalid patial identifier" do
      assert parse_token("hi.0") ==
               {:error, "expected alphabetic character or underscore to continue identifier", "0",
                %{}, {1, 0}, 3}

      assert parse_token("Hello.!") ==
               {:error, "expected alphabetic character or underscore to continue identifier", "!",
                %{}, {1, 0}, 6}
    end

    test "returns ok on whitespace" do
      assert parse_token(" hi") == {:ok, [], "hi", %{}, {1, 0}, 1}
      assert parse_token("\nhey") == {:ok, [], "hey", %{}, {2, 1}, 1}
      assert parse_token("\thello") == {:ok, [], "hello", %{}, {1, 0}, 1}
      assert parse_token("\vheya") == {:ok, [], "heya", %{}, {1, 0}, 1}
      assert parse_token("\rhiya") == {:ok, [], "hiya", %{}, {1, 0}, 1}
      assert parse_token("\fyo") == {:ok, [], "yo", %{}, {1, 0}, 1}
    end
  end
end
