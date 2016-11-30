defmodule Thrift.Parser.Literals do
  @moduledoc """
  A module containing types for defining Thrift literals
  Thrift literals are used when setting default values and constants.
  """
  defmodule Primitive do
    @moduledoc """
    A Thrift primitive type
    """
    @type t :: integer | boolean | String.t | float
  end

  defmodule List do
    @moduledoc """
    A Thrift list
    """
    @type t :: [Thrift.Parser.Literals.t]
  end

  defmodule Map do
    @moduledoc """
    A Thrift map
    """
    @type t :: %{Thrift.Parser.Literals.t => Thrift.Parser.Literals.t}
  end

  defmodule Container do
    @moduledoc """
    A Thrift container type
    """
    @type t :: Map.t | List.t
  end

  @type t :: Container.t | Primitive.t
  @type s :: atom
end
