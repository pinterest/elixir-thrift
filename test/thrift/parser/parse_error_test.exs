defmodule Thrift.Parser.ParseErrorTest do
  use ExUnit.Case

  @project_root Path.expand("../..", __DIR__)
  @test_file_dir Path.join([@project_root, "tmp", "parse_error_test"])

  import Thrift.Parser, only: [parse: 1, parse_file: 1]

  setup do
    File.rm_rf!(@test_file_dir)
    File.mkdir_p!(@test_file_dir)
    on_exit(fn -> File.rm_rf!(@test_file_dir) end)
  end

  test "a file that throws parser errors raises an exception" do
    contents = """
    stract Typo {
        1: optional i32 id
    }
    """

    assert {:error, _} = parse(contents)

    path = Path.join(@test_file_dir, "syntax_error.thrift")
    File.write!(path, contents)

    assert_raise(
      Thrift.FileParseError,
      ~r/#{path} on line 1:/,
      fn -> parse_file(path) end
    )

    other_path = Path.join(@test_file_dir, "includes_syntax_error.thrift")

    File.write!(other_path, """
    include "syntax_error.thrift"

    struct NoError {
      1: optional i32 id
    }
    """)

    # should raise an error on the included file,
    # since that is where the syntax error is
    assert_raise(
      Thrift.FileParseError,
      ~r/#{path} on line 1:/,
      fn -> parse_file(other_path) end
    )
  end

  test "a file that throws lexer errors raises an exception" do
    contents = """
    // error on the next line
    /8
    """

    assert {:error, {2, _}} = parse(contents)

    path = Path.join(@test_file_dir, "lexer_error.thrift")
    File.write!(path, contents)

    assert_raise(
      Thrift.FileParseError,
      ~r/#{path} on line 2:/,
      fn -> parse_file(path) end
    )

    other_path = Path.join(@test_file_dir, "includes_syntax_error.thrift")

    File.write!(other_path, """
    include "lexer_error.thrift"

    struct NoError {
      1: optional i32 id
    }
    """)

    # should raise an error on the included file,
    # since that is where the syntax error is
    assert_raise(
      Thrift.FileParseError,
      ~r/#{path} on line 2:/,
      fn -> parse_file(other_path) end
    )
  end
end
