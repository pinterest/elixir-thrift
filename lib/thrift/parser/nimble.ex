defmodule Thrift.Parser.Nimble do
  import NimbleParsec

  defmodule Lexer do
    @moduledoc false
    @punctuator ~c"(){}[]<>,;=*"

    @keywords ~w(
      namespace include cpp_include
      typedef enum union struct exception
      void bool byte i8 i16 i32 i64 double string binary list map set
      const oneway extends throws service required optional
      true false
    )

    def token(combinator \\ empty()) do
      choice(combinator, [
        keyword(),
        identifier(),
        literal(),
        number(),
        punctuator(),
        whitespace()
      ])
    end

    defp whitespace() do
      ascii_string([?\s, ?\t, ?\n, ?\v, ?\f, ?\r], min: 1)
      |> ignore()
      |> label("whitespace")
    end

    defp keyword() do
      @keywords
      |> Enum.map(&(string(&1) |> replace(String.to_atom(&1))))
      |> choice()
      |> label("keyword")
    end

    defp punctuator() do
      @punctuator
      |> Enum.map(&(string(<<&1>>) |> replace(String.to_atom(<<&1>>))))
      |> choice()
      |> label("punctuator")
    end

    defp literal() do
      choice([
        literal_with(?"),
        literal_with(?')
      ])
      |> reduce({List, :to_string, []})
      |> label("literal")
    end

    defp literal_with(char) do
      delim = ascii_char([char])

      delim
      |> ignore()
      |> concat(
        choice([
          utf8_char([?\\]) |> ignore() |> concat(delim),
          utf8_char([]),
          error(eos(), "expected literal delimiter ?#{[char]}"),
          error(empty(), "expected utf8 codepoint")
        ])
        |> repeat_until([delim])
      )
      |> ignore(delim)
    end

    defp number(combinator \\ empty()) do
      combinator
      |> choice([
        ascii_char([?-, ?+])
        |> choice([
          unsigned_number(),
          empty()
          |> error("expected number")
        ])
        |> post_traverse({__MODULE__, :__sign__, []}),
        unsigned_number()
      ])
      |> label("number")
    end

    defp unsigned_number() do
      choice([
        hex(),
        integer(min: 1)
        |> choice([
          ignore(ascii_char([?.]))
          |> integer(min: 1)
          |> optional(ignore(ascii_char([?E, ?e])) |> exponent()),
          empty()
          |> replace(0)
          |> ignore(ascii_char([?E, ?e]))
          |> exponent(),
          empty()
        ])
      ])
      |> optional(
        ascii_char([?.])
        |> ignore()
        |> error(empty(), "expected integer fraction for significand")
      )
      |> post_traverse({__MODULE__, :__number__, []})
    end

    def __sign__(_rest, acc, context, _line, _offset) do
      case acc do
        [number, ?-] ->
          {[-number], context}

        [number, ?+] ->
          {[number], context}
      end
    end

    defp hex() do
      string("0x")
      |> ignore()
      |> choice([
        ascii_string([?0..?9, ?a..?f, ?A..?F], min: 1)
        |> map({String, :to_integer, [16]}),
        empty()
        |> error("expected hexidecimal digit")
      ])
    end

    defp exponent(combinator) do
      combinator
      |> choice([
        choice([
          ascii_char([?-, ?+]),
          empty() |> replace(?+)
        ])
        |> integer(min: 1),
        empty()
        |> error("expected integer exponent")
      ])
    end

    def __number__(_rest, acc, context, _line, _offset) do
      case acc do
        [_int] ->
          {acc, context}

        [fraction, int] ->
          {[String.to_float("#{int}.#{fraction}")], context}

        [exponent, exponent_sign, fraction, int] ->
          {[String.to_float("#{int}.#{fraction}e#{[exponent_sign]}#{exponent}")], context}
      end
    end

    def identifier() do
      ascii_char([?a..?z, ?A..?Z, ?_])
      |> repeat(ascii_char([?a..?z, ?A..?Z, ?_, ?0..?9]))
      |> reduce({List, :to_atom, []})
      |> optional(
        repeat(
          ascii_char([?.])
          |> ignore()
          |> choice([
            ascii_char([?a..?z, ?A..?Z, ?_])
            |> repeat(ascii_char([?a..?z, ?A..?Z, ?_, ?0..?9]))
            |> reduce({List, :to_atom, []}),
            empty()
            |> error("expected alphabetic character or underscore to continue identifier")
          ])
        )
      )
      |> wrap()
      |> label("identifier")
    end

    defp error(combinator \\ empty(), to_error, label) do
      pre_traverse(combinator, to_error, {__MODULE__, :__error__, [label]})
    end

    def __error__(_rest, _acc, _context, _line, _offset, label) do
      {:error, label}
    end
  end

  defparsec(:parse_token, Lexer.token())
end
