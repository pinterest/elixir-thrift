defmodule Thrift.Parser.Types do
  @moduledoc false

  defmodule Primitive do
    @moduledoc false
    @type t :: :bool | :i8 | :i16 | :i32 | :i64 | :binary | :double | :byte | :string
  end

  defmodule Ident do
    @moduledoc false
    @type t :: String.t
  end

  defmodule Standalone do
    @moduledoc false
    @type t :: Ident.t | Primitive.t
  end

  defmodule List do
    @moduledoc false
    @type t :: {:list, Thrift.Parser.Types.t}
  end

  defmodule Map do
    @moduledoc false
    @type t :: {:map, {Thrift.Parser.Types.t, Thrift.Parser.Types.t}}
  end

  defmodule Set do
    @moduledoc false
    @type t :: {:set, Thrift.Parser.Types.t}
  end

  defmodule Container do
    @moduledoc false
    @type t :: List.t | Map.t | Set.t
  end

  @type t :: Container.t | Standalone.t
end
