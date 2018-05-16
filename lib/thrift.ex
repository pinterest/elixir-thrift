defmodule Thrift do
  @moduledoc ~S"""
  Thrift provides [Apache Thrift](http://thrift.apache.org/) support:

    * `Mix.Tasks.Compile.Thrift` - mix task for generate Erlang source files
      from `.thrift` schema files

    * `Thrift.Parser` - functions for parsing `.thift` schema files
  """

  @typedoc "Thrift data types"
  @type data_type ::
    :bool | :byte | :i8 | :i16 | :i32 | :i64 | :double | :string | :binary |
    {:map, data_type, data_type} | {:set, data_type} | {:list, data_type}

  @type i8   :: (-128..127)
  @type i16 :: (-32_768..32_767)
  @type i32 :: (-2_147_483_648..2_147_483_647)
  @type i64 :: (-9_223_372_036_854_775_808..9_223_372_036_854_775_807)
  @type double :: float()

  @typedoc "Thrift message types"
  @type message_type :: :call | :reply | :exception | :oneway

  @doc """
  Returns a list of atoms, each of which is a name of a Thrift primitive type.
  """
  @spec primitive_names() :: [Thrift.Parser.Types.Primitive.t]
  def primitive_names do
    [:bool, :i8, :i16, :i32, :i64, :binary, :double, :byte, :string]
  end

  defmodule NaN do
    @moduledoc """
    A struct for handling IEEE-754 NaN values.
    """

    @type t :: %NaN{sign: 0 | 1,
                    fraction: (1..4_503_599_627_370_495)  # 2^52 - 1
                   }
    defstruct sign: nil,
              fraction: nil
  end
end
