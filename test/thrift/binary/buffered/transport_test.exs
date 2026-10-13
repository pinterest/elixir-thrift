defmodule Thrift.Binary.Buffered.TransportTest do
  use ThriftTestCase

  @thrift_file name: "buffered.thrift",
               contents: """
               exception Rejected { 1: string message }
               service Echo {
                 string echo(1: string text),
                 map<string, list<i64>> nested(1: map<string, list<i64>> value),
                 void fail() throws (1: Rejected rejected),
                 oneway void notify(),
               }
               """

  alias __MODULE__.Echo.Binary.Buffered.{Client, Server}
  alias Thrift.Binary.Buffered.Message
  alias Thrift.Protocol.Binary

  defmodule Handler do
    def echo(text), do: text
    def nested(value), do: value
    def notify(), do: :ok
    def fail(), do: raise(Thrift.Binary.Buffered.TransportTest.Rejected, message: "rejected")
  end

  setup do
    {:ok, server} = Server.start_link(Handler, 0)
    Process.unlink(server)
    port = :ranch.get_port(Handler)
    on_exit(fn -> if Process.alive?(server), do: Server.stop(server) end)
    {:ok, port: port}
  end

  thrift_test "generated client supports repeated calls, containers, exceptions and oneway",
              ctx do
    {:ok, client} = Client.start_link("127.0.0.1", ctx.port)
    assert {:ok, ""} = Client.echo(client, "")
    assert {:ok, nil} = Client.notify(client)
    assert {:ok, "after oneway"} = Client.echo(client, "after oneway")
    nested = %{"empty" => [], "numbers" => [1, -2, 3]}
    assert {:ok, ^nested} = Client.nested(client, nested)
    assert {:error, {:exception, %Rejected{message: "rejected"}}} = Client.fail(client)
    assert "still connected" = Client.echo!(client, "still connected")
    assert :ok = Client.close(client)
  end

  thrift_test "raw peer can fragment and coalesce requests without frame prefixes", ctx do
    {:ok, socket} =
      :gen_tcp.connect(String.to_charlist("localhost"), ctx.port, [
        :binary,
        active: false,
        packet: 0
      ])

    # Hand-written binary protocol requests, independent of our serializer.
    request = <<0x80, 1, 0, 1, 4::32, "echo", 7::32, 11, 1::16, 3::32, "abc", 0>>
    <<first, rest::binary>> = request
    :ok = :gen_tcp.send(socket, <<first>>)
    :ok = :gen_tcp.send(socket, [rest, request])
    response = <<0x80, 1, 0, 2, 4::32, "echo", 7::32, 11, 0::16, 3::32, "abc", 0>>
    assert {:ok, ^response} = :gen_tcp.recv(socket, byte_size(response), 1000)
    assert {:ok, ^response} = :gen_tcp.recv(socket, byte_size(response), 1000)
    :gen_tcp.close(socket)
  end

  thrift_test "server accepts legacy message headers", ctx do
    {:ok, socket} =
      :gen_tcp.connect(String.to_charlist("localhost"), ctx.port, [
        :binary,
        active: false,
        packet: 0
      ])

    :ok = :gen_tcp.send(socket, <<4::32, "echo", 1, 9::32, 11, 1::16, 0::32, 0>>)
    assert {:ok, response} = Message.recv(:gen_tcp, socket, 1000)
    assert {:ok, {:reply, 9, "echo", _}} = Binary.deserialize(:message_begin, response)
    :gen_tcp.close(socket)
  end
end
