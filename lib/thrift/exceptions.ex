defmodule Thrift.Exceptions do
  defmodule TApplicationException do
    defexception message: nil, type: nil
  end
end
