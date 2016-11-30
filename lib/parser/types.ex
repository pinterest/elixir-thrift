defmodule Thrift.Parser.Types do
  @moduledoc """
  A container module for modules containing typespecs for Thrift files.
  """
  defmodule Primitive do
    @moduledoc """
    Typespec for Thrift primitives
    """
    @type t :: :bool | :i8 | :i16 | :i64 | :binary | :double | :byte | :string
  end

  defmodule Ident do
    @moduledoc """
    A Thrift identifier
    """
    @type t :: String.t
  end

  defmodule Standalone do
    @moduledoc """
    A Thrift type that isn't a container
    """
    @type t :: Ident.t | Primitive.t
  end

  defmodule List do
    @moduledoc """
    A Thrift list.
    """
    @type t :: {:list, Thrift.Parser.Types.t}
  end

  defmodule Map do
    @moduledoc """
    A Thrift map
    """
    @type t  :: {:map, {Thrift.Parser.Types.t, Thrift.Parser.Types.t}}
  end

  defmodule Set do
    @moduledoc """
    A Thrift set
    """
    @type t :: {:set, Thrift.Parser.Types.t}
  end

  defmodule Container do
    @moduledoc """
    A Thrift contianer type
    """
    @type t :: List.t | Map.t | Set.t
  end

  @type t :: Container.t | Standalone.t
end
