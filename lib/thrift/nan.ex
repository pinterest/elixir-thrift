defmodule Thrift.NaN do
  @moduledoc """
  A struct for handling IEEE-754 NaN values.
  """

  @type t :: %Thrift.NaN{sign: 0 | 1,
                         fraction: (1..4_503_599_627_370_495)  # 2^52 - 1
                        }
  defstruct sign: nil,
            fraction: nil
end
