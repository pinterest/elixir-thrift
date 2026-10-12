defmodule Thrift.Binary.Buffered.Message do
  @moduledoc false

  # Exact reads leave subsequent messages in the socket buffer. Bound both
  # message size and nesting, and use one deadline for the entire message.
  @max_size 16 * 1024 * 1024
  @max_depth 64

  def recv(transport, socket, timeout) do
    deadline = if timeout == :infinity, do: :infinity, else: now() + timeout
    state = {transport, socket, deadline, [], 0}

    try do
      {header, state} = read(state, 4)

      state =
        case header do
          <<0x80, 1, 0, type>> when type in 1..4 ->
            state = value(11, state, 0)
            elem(read(state, 4), 1)

          <<size::signed-32>> when size >= 0 ->
            {_, state} = read(state, size)
            {<<type>>, state} = read(state, 1)
            if type not in 1..4, do: throw({:error, :invalid_message})
            elem(read(state, 4), 1)

          _ ->
            throw({:error, :invalid_message})
        end

      {_, _, _, chunks, _} = value(12, state, 0)
      {:ok, chunks |> Enum.reverse() |> IO.iodata_to_binary()}
    catch
      {:error, _} = error -> error
    end
  end

  defp value(_, _, depth) when depth > @max_depth, do: throw({:error, :invalid_message})
  defp value(type, state, _) when type in [2, 3], do: elem(read(state, 1), 1)
  defp value(6, state, _), do: elem(read(state, 2), 1)
  defp value(8, state, _), do: elem(read(state, 4), 1)
  defp value(type, state, _) when type in [4, 10], do: elem(read(state, 8), 1)

  defp value(11, state, _) do
    {size, state} = count(state)
    elem(read(state, size), 1)
  end

  defp value(12, state, depth) do
    case read(state, 1) do
      {<<0>>, state} ->
        state

      {<<type>>, state} ->
        {_, state} = read(state, 2)
        state = value(type, state, depth + 1)
        value(12, state, depth)
    end
  end

  defp value(13, state, depth) do
    {<<key, val>>, state} = read(state, 2)
    {size, state} = count(state)
    repeat(size, [key, val], state, depth + 1)
  end

  defp value(type, state, depth) when type in [14, 15] do
    {<<element>>, state} = read(state, 1)
    {size, state} = count(state)
    repeat(size, [element], state, depth + 1)
  end

  defp value(_, _, _), do: throw({:error, :invalid_message})

  defp repeat(0, _, state, _), do: state

  defp repeat(count, types, state, depth) do
    state = Enum.reduce(types, state, &value(&1, &2, depth))
    repeat(count - 1, types, state, depth)
  end

  defp count(state) do
    {<<size::signed-32>>, state} = read(state, 4)
    if size < 0 or size > @max_size, do: throw({:error, :invalid_message})
    {size, state}
  end

  defp read(state, 0), do: {<<>>, state}

  defp read({transport, socket, deadline, chunks, total}, size) do
    if total + size > @max_size, do: throw({:error, :message_too_large})
    timeout = if deadline == :infinity, do: :infinity, else: max(deadline - now(), 0)

    case transport.recv(socket, size, timeout) do
      {:ok, bytes} -> {bytes, {transport, socket, deadline, [bytes | chunks], total + size}}
      {:error, _} = error -> throw(error)
    end
  end

  defp now, do: System.monotonic_time(:millisecond)
end
