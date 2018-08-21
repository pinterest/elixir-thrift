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
  of Thrift files to compile. See `Mix.Tasks.Compile.Thrift` for the full set
  of available options.

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

  ### Thrift Definitions

  Given some Thrift type definitions:

  ```thrift
  namespace elixir Thrift.Test

  exception UserNotFound {
    1: string message
  }

  struct User {
    1: i64 id,
    2: string username,
  }

  service UserService {
    bool ping(),
    User getUser(1: i64 id) throws (1: UserNotFound e),
    bool delete(1: i64 id),
  }
  ```

  ... the generated code will be placed in the following modules under
  `lib/thrift/`:

    Definition                | Module
    ------------------------- | -----------------------------------------------
    `User` struct             | `Thrift.Test.User`
    *└ binary protocol*       | `Thrift.Test.User.BinaryProtocol`
    `UserNotFound` exception  | `Thrift.Test.UserNotFound`
    *└ binary protocol*       | `Thrift.Test.UserNotFound.BinaryProtocol`
    `UserService` service     | `Thrift.Test.UserService.Handler`
    *└ binary framed client*  | `Thrift.Test.UserService.Binary.Framed.Client`
    *└ binary framed server*  | `Thrift.Test.UserService.Binary.Framed.Server`

  ### Namespaces

  The generated modules' namespace is determined by the `:namespace` compiler
  option, which defaults to `Thrift.Generated`. Individual `.thrift` files can
  specify their own namespace using the `namespace` keyword, taking precedence
  over the compiler's value.

  ```thrift
  namespace elixir Thrift.Test
  ```

  Unfortunately, the Apache Thrift compiler will produce a warning on this line
  because it doesn't recognize `elixir` as a supported language. While that
  warning is benign, it can be annoying. For that reason, you can also specify
  your Elixir namespace as a "magic" namespace comment:

  ```thrift
  #@namespace elixir Thrift.Test
  ```

  This alternate syntax is [borrowed from Scrooge][scrooge-namespaces], which
  uses the same trick for defining Scala namespaces.

  [scrooge-namespaces]: https://twitter.github.io/scrooge/Namespaces.html

  ## Clients

  Service clients are built on `Thrift.Binary.Framed.Client`. This module
  uses the `Connection` behaviour to implement network state handling. In
  practice, you won't be interacting with this low-level module directly,
  however.

  A client interface module is generated for each service. This is much more
  convenient to use from application code because it provides distinct Elixir
  functions for each Thrift service function. It also handles argument packing,
  return value unpacking, and other high-level conversions. In this example,
  this generated module is `Thrift.Test.UserService.Binary.Framed.Client`.

  Each generated client function comes in two flavors: a standard version (e.g.
  `Client.get_user/3`) that returns `{:ok, response}` or `{:error, reason}`,
  and a *bang!* variant (e.g. `Client.get_user!/3`) that raises an exception on
  errors.

      iex> alias Thrift.Test.UserService.Binary.Framed.Client
      iex> {:ok, client} = Client.start_link("localhost", 2345, [])
      iex> {:ok, user} = Client.get_user(client, 123)
      {:ok, %Thrift.Test.User{id: 123, username: "user"}}

  Note that the generated function names use [Elixir's naming conventions]
  [naming], so `getUser` becomes `get_user`.

  [naming]: http://elixir-lang.org/docs/stable/elixir/naming-conventions.html
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
    A struct that represents [IEEE-754 NaN](https://en.wikipedia.org/wiki/NaN)
    values.
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
