defmodule Thrift do
  @moduledoc ~S"""
  [Thrift](https://thrift.apache.org/) implementation for Elixir including a
  Thrift IDL parser, a code generator, and an RPC system

  ## Thrift IDL Parsing

  `Thrift.Parser` parses [Thrift IDL](https://thrift.apache.org/docs/idl) into
  an abstract syntax tree used for code generation. You can also work with
  `Thrift.AST` directly to support additional use cases, such as building
  linters or analysis tools.

  ## Code Generation

  `Mix.Tasks.Compile.Thrift` is a Mix compiler task that automates Thrift code
  generation. To use it, add `:thrift` to your project's `:compilers` list.
  For example:

      compilers: [:thrift | Mix.compilers]

  It's important to add `:thrift` *before* the `:elixir` compiler entry. The
  Thrift compiler generates Elixir source files, which are in turn compiled by
  the `:elixir` compiler.

  Configure the compiler using a keyword list under the top-level `:thrift`
  key. The only required compiler option is `:files`, which defines the list
  of Thrift files to compile.

  By default, the generated Elixir source files will be written to the `lib`
  directory, but you can change that using the `output_path` option.

  In this example, we gather all of the `.thrift` files under the `thrift`
  directory and write our output files to the `lib/generated` directory:

      defmodule MyProject.Mixfile do
        # ...
        def project do
          [
            # ...
            compilers: [:thrift | Mix.compilers],
            thrift: [
              files: Path.wildcard("thrift/**/*.thrift"),
              output_path: "lib/generated"
            ]
          ]
        end
      end

  You can also use the `Mix.Tasks.Thrift.Generate` Mix task to generate code
  on-demand. By default, it uses the same project configuration as the
  compiler task above, but options can also be specified using command line
  arguments.
  """

  @typedoc "Thrift data types"
  @type data_type ::
          :bool
          | :byte
          | :i8
          | :i16
          | :i32
          | :i64
          | :double
          | :string
          | :binary
          | {:map, data_type, data_type}
          | {:set, data_type}
          | {:list, data_type}

  @type i8 :: -128..127
  @type i16 :: -32_768..32_767
  @type i32 :: -2_147_483_648..2_147_483_647
  @type i64 :: -9_223_372_036_854_775_808..9_223_372_036_854_775_807
  @type double :: float()

  @typedoc "Thrift message types"
  @type message_type :: :call | :reply | :exception | :oneway

  @doc """
  Returns a list of atoms, each of which is a name of a Thrift primitive type.
  """
  @spec primitive_names() :: [Thrift.Parser.Types.Primitive.t()]
  def primitive_names do
    [:bool, :i8, :i16, :i32, :i64, :binary, :double, :byte, :string]
  end

  defmodule NaN do
    @moduledoc """
    A struct for handling IEEE-754 NaN values.
    """

    @type t :: %NaN{
            sign: 0 | 1,
            # 2^52 - 1
            fraction: 1..4_503_599_627_370_495
          }
    defstruct sign: nil,
              fraction: nil
  end
end
