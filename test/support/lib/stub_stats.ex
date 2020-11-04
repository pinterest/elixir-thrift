defmodule StubStats do
  use GenServer

  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__), only: [stats: 1, reset_stats: 0]
    end
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def stats(metric_name) do
    GenServer.call(__MODULE__, {:stats, metric_name})
  end

  def reset_stats do
    GenServer.call(__MODULE__, :reset_stats)
  end

  def init(opts) do
    handler_module = Keyword.fetch!(opts, :handler_module)

    events = [
      [Thrift, handler_module, :peek_first_byte],
      [Thrift, handler_module, :ssl_handshake],
      [Thrift, handler_module, :receive_message],
      [Thrift, handler_module, :call],
      [Thrift, handler_module, :send_reply],
      [Thrift, handler_module, :request_size],
      [Thrift, handler_module, :response_size]
    ]

    callback = &GenServer.call(__MODULE__, {:metric, &1, &2, &3, &4})

    :ok = :telemetry.attach_many(inspect(self()), events, callback, nil)
    {:ok, []}
  end

  def terminate(reason, _state) do
    IO.inspect(reason, label: "StubStats terminate reason")
    :telemetry.detach(inspect(self()))
  end

  def handle_call({:metric, event, _measurements, metadata, nil}, _, state) do
    [Thrift, _handler_module, metric_name] = event
    state = [{metric_name, metadata} | state]
    {:reply, :ok, state}
  end

  def handle_call({:stats, metric_name}, _, state) do
    metadatas =
      state
      |> Enum.flat_map(fn
        {^metric_name, metadata} -> [metadata]
        {_, _} -> []
      end)
      |> Enum.reverse()

    {:reply, metadatas, state}
  end

  def handle_call(:reset_stats, _, _) do
    {:reply, :ok, []}
  end
end
