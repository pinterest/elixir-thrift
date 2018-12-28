defmodule Thrift.Parser.ParseErrorTest do
  use ExUnit.Case

  @project_root Path.expand("../..", __DIR__)
  @test_file_dir Path.join([@project_root, "tmp", "parse_error_test"])

  import Thrift.Parser, only: [parse_string: 1, parse_file_group: 1]

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

    assert {:error, {nil, 1, _}} = parse_string(contents)

    path = Path.join(@test_file_dir, "syntax_error.thrift")
    File.write!(path, contents)

    assert {:error, [{^path, 1, "syntax error before: \"stract\""}]} = parse_file_group(path)

    other_path = Path.join(@test_file_dir, "includes_syntax_error.thrift")

    File.write!(other_path, """
    include "syntax_error.thrift"

    struct NoError {
      1: optional i32 id
    }
    """)

    # should raise an error on the included file,
    # since that is where the syntax error is
    assert {:error, [{^path, 1, "syntax error before: \"stract\""}]} =
             parse_file_group(other_path)
  end

  test "a file that throws lexer errors raises an exception" do
    contents = """
    // error on the next line
    /8
    """

    assert {:error, {nil, 2, _}} = parse_string(contents)

    path = Path.join(@test_file_dir, "lexer_error.thrift")
    File.write!(path, contents)

    assert {:error, [{^path, 2, "illegal characters \"/8\""}]} = parse_file_group(path)

    other_path = Path.join(@test_file_dir, "includes_syntax_error.thrift")

    File.write!(other_path, """
    include "lexer_error.thrift"

    struct NoError {
      1: optional i32 id
    }
    """)

    # should raise an error on the included file,
    # since that is where the syntax error is
    assert {:error, [{^path, 2, "illegal characters \"/8\""}]} = parse_file_group(other_path)
  end
end
