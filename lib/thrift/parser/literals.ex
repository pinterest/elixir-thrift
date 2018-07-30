defmodule Thrift.Parser.Literals do
  @moduledoc false

  defmodule Primitive do
    @moduledoc false
    @type t :: integer | boolean | String.t() | float
  end

  defmodule List do
    @moduledoc false
    @type t :: [Thrift.Parser.Literals.t()]
  end

  defmodule Map do
    @moduledoc false
    @type t :: %{Thrift.Parser.Literals.t() => Thrift.Parser.Literals.t()}
  end

  defmodule Container do
    @moduledoc false
    @type t :: Map.t() | List.t()
  end

  @type t :: Container.t() | Primitive.t()
  @type s :: atom
end
