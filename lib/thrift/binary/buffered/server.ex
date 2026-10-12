defmodule Thrift.Binary.Buffered.Server do
  @moduledoc """
  Binary protocol server using buffered (unframed) transport.

  Incoming messages are limited to 16 MiB and 64 levels of nesting.

  Options are the same as `Thrift.Binary.Framed.Server.start_link/4`.
  Generated services expose this as `YourService.Binary.Buffered.Server`.
  """
  alias Thrift.Binary.Framed.Server

  def start_link(server_module, port, handler_module, opts \\ []),
    do: Server.start_link(server_module, port, handler_module, Keyword.put(opts, :packet, 0))

  defdelegate stop(pid), to: Server
end
