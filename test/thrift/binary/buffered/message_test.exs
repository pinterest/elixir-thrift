defmodule Thrift.Binary.Buffered.MessageTest do
  use ExUnit.Case, async: true
  alias Thrift.Binary.Buffered.Message
  alias Thrift.Protocol.Binary

  setup do
    {:ok, listener} = :gen_tcp.listen(0, [:binary, active: false, packet: 0])
    {:ok, port} = :inet.port(listener)

    {:ok, sender} =
      :gen_tcp.connect(String.to_charlist("localhost"), port, [:binary, active: false, packet: 0])

    {:ok, receiver} = :gen_tcp.accept(listener)
    :gen_tcp.close(listener)

    on_exit(fn ->
      :gen_tcp.close(sender)
      :gen_tcp.close(receiver)
    end)

    {:ok, sender: sender, receiver: receiver}
  end

  test "reads every field type and leaves the next message buffered", ctx do
    fields = [
      {2, Binary.serialize(:bool, true)},
      {3, Binary.serialize(:i8, -5)},
      {6, Binary.serialize(:i16, -300)},
      {8, Binary.serialize(:i32, 999)},
      {10, Binary.serialize(:i64, -999)},
      {4, Binary.serialize(:double, 1.5)},
      {11, Binary.serialize(:string, "")},
      {12, <<11, 1::16, 3::32, 0, 1, 2, 0>>},
      {13, Binary.serialize({:map, {:string, {:list, :i64}}}, %{"a" => [1, 2]})},
      {14, Binary.serialize({:set, :string}, MapSet.new(["a", "b"]))},
      {15, Binary.serialize({:list, :i32}, [])}
    ]

    body = Enum.map(fields, fn {type, bytes} -> [<<type, 1::16>>, bytes] end)

    message =
      IO.iodata_to_binary([Binary.serialize(:message_begin, {:reply, 0, "all"}), body, <<0>>])

    :ok = :gen_tcp.send(ctx.sender, [message, message])
    assert {:ok, ^message} = Message.recv(:gen_tcp, ctx.receiver, 1000)
    assert {:ok, ^message} = Message.recv(:gen_tcp, ctx.receiver, 1000)
  end

  test "rejects negative lengths and unknown field types", ctx do
    header = Binary.serialize(:message_begin, {:reply, 0, "bad"})
    :ok = :gen_tcp.send(ctx.sender, [header, <<11, 1::16, -1::signed-32>>])
    assert {:error, :invalid_message} = Message.recv(:gen_tcp, ctx.receiver, 1000)
    :ok = :gen_tcp.send(ctx.sender, [header, <<99, 1::16>>])
    assert {:error, :invalid_message} = Message.recv(:gen_tcp, ctx.receiver, 1000)
  end

  test "bounds message size and nesting", ctx do
    header = Binary.serialize(:message_begin, {:reply, 0, "bad"})
    :ok = :gen_tcp.send(ctx.sender, [header, <<11, 1::16, 16_777_216::32>>])
    assert {:error, :message_too_large} = Message.recv(:gen_tcp, ctx.receiver, 1000)
    :ok = :gen_tcp.send(ctx.sender, [header, List.duplicate(<<12, 1::16>>, 65)])
    assert {:error, :invalid_message} = Message.recv(:gen_tcp, ctx.receiver, 1000)
  end

  test "incomplete messages time out or report a closed connection", ctx do
    :ok = :gen_tcp.send(ctx.sender, <<0x80, 1>>)
    assert {:error, :timeout} = Message.recv(:gen_tcp, ctx.receiver, 20)
    :gen_tcp.close(ctx.sender)
    assert {:error, :closed} = Message.recv(:gen_tcp, ctx.receiver, 1000)
  end
end
