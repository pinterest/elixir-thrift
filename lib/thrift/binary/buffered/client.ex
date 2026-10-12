defmodule Thrift.Binary.Buffered.Client do
  @moduledoc """
  Binary protocol client using buffered (unframed) transport.

  Writes each RPC in one send and reads a complete binary protocol message.
  Incoming messages are limited to 16 MiB and 64 levels of nesting.

  Options are the same as `Thrift.Binary.Framed.Client.start_link/3`.
  Generated services expose this as `YourService.Binary.Buffered.Client`.
  """
  alias Thrift.Binary.Framed.Client

  def start_link(host, port, opts \\ []),
    do: Client.start_link(host, port, Keyword.put(opts, :packet, 0))

  defdelegate close(conn), to: Client
  defdelegate connect(conn, opts), to: Client
  defdelegate call(conn, name, args, module, opts), to: Client
  defdelegate oneway(conn, name, args, opts), to: Client
end
