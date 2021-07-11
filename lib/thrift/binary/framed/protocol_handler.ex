defmodule Thrift.Binary.Framed.ProtocolHandler do
  @moduledoc false

  @default_timeout 20_000
  @ssl_header_byte 0x16
  @tcp_header_byte 0x00

  @typedoc "A module that implements the :ranch_transport behaviour"
  @type transport :: :ranch_tcp

  @typedoc "Transport-specific options"
  @type transport_opts :: :ranch_tcp.opts()

  alias Thrift.{
    Protocol.Binary,
    TApplicationException,
    Transport.SSL
  }

  require Logger

  defstruct [
    :transport,
    :socket,
    :server_module,
    :handler_module,
    :on_connect,
    :recv_timeout
  ]

  @spec start_link(reference, port, transport, {module, module, transport_opts, [SSL.option()]}) ::
          GenServer.on_start()
  def start_link(
        ref,
        socket,
        transport,
        {server_module, handler_module, on_connect, transport_opts, ssl_opts}
      ) do
    pid =
      spawn_link(__MODULE__, :init, [
        ref,
        socket,
        transport,
        server_module,
        handler_module,
        on_connect,
        transport_opts,
        ssl_opts
      ])

    {:ok, pid}
  end

  @dialyzer {:nowarn_function, init: 8}
  @spec init(reference, port, :ranch_tcp, module, module, function, :ranch_tcp.opts(), [
          SSL.option()
        ]) ::
          :ok | no_return
  def init(ref, socket, :ranch_tcp, server_module, handler_module, on_connect, tcp_opts, ssl_opts) do
    :ok = :ranch.accept_ack(ref)

    {recv_timeout, tcp_opts} = Keyword.pop(tcp_opts, :recv_timeout, @default_timeout)
    :ok = :inet.setopts(socket, Keyword.put(tcp_opts, :packet, 4))

    state = %__MODULE__{
      transport: :gen_tcp,
      socket: socket,
      server_module: server_module,
      handler_module: handler_module,
      on_connect: on_connect,
      recv_timeout: recv_timeout
    }

    case SSL.configuration(ssl_opts) do
      {:error, %_exception{} = err} ->
        Logger.error(fn ->
          format_log("SSL configuration error", Exception.format(:error, err, []), state)
        end)

        close(state)

      nil ->
        peek_first_byte(:disabled, nil, state)

      {optional, ssl_opts} when optional in [:required, :optional] ->
        peek_first_byte(optional, ssl_opts, state)
    end
  end

  defp peek_first_byte(
         optional,
         ssl_opts,
         %__MODULE__{transport: :gen_tcp, socket: socket, recv_timeout: recv_timeout} = state
       ) do
    span = start_span(:peek_first_byte, state)

    with :ok <- :inet.setopts(socket, packet: :raw),
         {:ok, first_bytes} <- :gen_tcp.recv(socket, 4, recv_timeout),
         :ok <- :gen_tcp.unrecv(socket, first_bytes),
         :ok <- :inet.setopts(socket, packet: 4) do
      <<first_byte::8-unsigned, _::binary>> = first_bytes
      {:ok, first_byte}
    end
    |> case do
      {:ok, @ssl_header_byte} when optional in [:required, :optional] ->
        finish_span(span, result: "ssl")
        ssl_handshake(ssl_opts, state)

      {:ok, @ssl_header_byte} when optional == :disabled ->
        Logger.error(fn ->
          format_log("peek_first_byte", "SSL disabled", state)
        end)

        finish_span(span, result: "ssl_rejected")
        close(state)

      {:ok, @tcp_header_byte} when optional == :required ->
        Logger.error(fn ->
          format_log("peek_first_byte", "SSL required", state)
        end)

        finish_span(span, result: "tcp_rejected")
        close(state)

      {:ok, @tcp_header_byte} when optional in [:disabled, :optional] ->
        finish_span(span, result: "tcp")
        on_connect(state)

      {:ok, _other_byte} ->
        Logger.error(fn ->
          format_log("peek_first_byte", "unknown protocol", state)
        end)

        finish_span(span, result: "unknown_protocol")
        close(state)

      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        finish_span(span, result: to_string(closed))
        close(state)

      {:error, error} ->
        Logger.error(fn ->
          format_log("peek_first_byte", :ssl.format_error(error), state)
        end)

        finish_span(span, result: "error")
        close(state)
    end
  end

  defp ssl_handshake(
         ssl_opts,
         %__MODULE__{transport: :gen_tcp, socket: socket, recv_timeout: recv_timeout} = state
       ) do
    span = start_span(:ssl_handshake, state)

    # As of OTP 21.0, `:ssl.ssl_accept/3` is deprecated in favour of `:ssl.handshake/3`.
    # This check allows us to support both, depending on which OTP version is being used.
    if function_exported?(:ssl, :handshake, 3) do
      apply(:ssl, :handshake, [socket, ssl_opts, recv_timeout])
    else
      apply(:ssl, :ssl_accept, [socket, ssl_opts, recv_timeout])
    end
    |> case do
      {:ok, ssl_socket} ->
        finish_span(span, result: "success")
        state = %__MODULE__{state | transport: :ssl, socket: ssl_socket}
        on_connect(state)

      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        finish_span(span, result: to_string(closed))
        close(state)

      {:error, error} ->
        Logger.error(fn ->
          format_log("ssl_handshake", :ssl.format_error(error), state)
        end)

        finish_span(span, result: "error")
        close(state)
    end
  end

  defp on_connect(state) do
    case state.on_connect do
      nil ->
        receive_message(state)

      on_connect when is_function(on_connect, 1) ->
        if on_connect.(state.socket) do
          receive_message(state)
        else
          close(state)
        end
    end
  end

  defp receive_message(
         %__MODULE__{transport: transport, socket: socket, recv_timeout: recv_timeout} = state
       ) do
    span = start_span(:receive_message, state)

    case transport.recv(socket, 0, recv_timeout) do
      {:ok, serialized_message} ->
        finish_span(span, result: "success")
        deserialize_message(serialized_message, state)

      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        finish_span(span, result: to_string(closed))
        close(state)

      {:error, error} ->
        Logger.error(fn ->
          format_log("receive_message", :ssl.format_error(error), state)
        end)

        finish_span(span, result: "error")
        close(state)
    end
  end

  defp deserialize_message(serialized_message, state) do
    case Binary.deserialize(:message_begin, serialized_message) do
      {:ok, message} ->
        handle_message(message, state)

      {:error, reason} ->
        Logger.error(fn ->
          format_log("deserialize_message", inspect(reason), state)
        end)

        close(state)
    end
  end

  defp handle_message(
         {:call, sequence_id, method_name, args_binary},
         %__MODULE__{server_module: server_module, handler_module: handler_module} = state
       ) do
    request_size = IO.iodata_length(args_binary)
    gauge(:request_size, request_size, state, method: method_name)

    span = start_span(:call, state, method: method_name)

    case server_module.handle_thrift(method_name, args_binary, handler_module) do
      :noreply -> {:reply, <<0>>}
      other -> other
    end
    |> case do
      {:reply, serialized_response} ->
        finish_span(span, result: "success")

        serialized_message = Binary.serialize(:message_begin, {:reply, sequence_id, method_name})

        response_size = IO.iodata_length(serialized_message)
        gauge(:response_size, response_size, state, method: method_name, result: "success")

        send_reply([serialized_message | serialized_response], :continue, state)

      {:client_error, %TApplicationException{} = exc} ->
        finish_span(span, result: "client_error")

        serialized_message =
          Binary.serialize(:message_begin, {:exception, sequence_id, method_name})

        serialized_exception = Binary.serialize(:application_exception, exc)

        response_size = IO.iodata_length(serialized_exception)
        gauge(:response_size, response_size, state, method: method_name, result: "client_error")

        send_reply([serialized_message | serialized_exception], :continue, state)

      {:server_error, %TApplicationException{} = exc} ->
        finish_span(span, result: "error")

        serialized_message =
          Binary.serialize(:message_begin, {:exception, sequence_id, method_name})

        serialized_exception = Binary.serialize(:application_exception, exc)

        response_size = IO.iodata_length(serialized_exception)
        gauge(:response_size, response_size, state, method: method_name, result: "error")

        send_reply([serialized_message | serialized_exception], :stop, state)
    end
  end

  defp handle_message(
         {:oneway, _sequence_id, method_name, args_binary},
         %__MODULE__{server_module: server_module, handler_module: handler_module} = state
       ) do
    spawn(fn ->
      server_module.handle_thrift(method_name, args_binary, handler_module)
    end)

    send_reply(<<0>>, :continue, state)
  end

  defp send_reply(data, after_reply, %__MODULE__{transport: transport, socket: socket} = state) do
    span = start_span(:send_reply, state)

    case transport.send(socket, data) do
      :ok when after_reply == :continue ->
        finish_span(span, result: "success")
        receive_message(state)

      :ok when after_reply == :stop ->
        finish_span(span, result: "success")
        close(state)

      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        finish_span(span, result: to_string(closed))
        close(state)

      {:error, error} ->
        Logger.error(fn ->
          format_log("send_reply", :ssl.format_error(error), state)
        end)

        finish_span(span, result: "error")
        close(state)
    end
  end

  defp close(%__MODULE__{transport: transport, socket: socket}) do
    transport.close(socket)
  end

  defp start_span(metric_name, %__MODULE__{handler_module: handler_module}, tags \\ []) do
    event_name = [Thrift, handler_module, metric_name]
    time = System.monotonic_time()
    metadata = tags_to_metadata(tags)
    {:span, event_name, metadata, time}
  end

  defp finish_span({:span, event_name, metadata, time}, tags) do
    duration = System.monotonic_time() - time
    metadata = tags_to_metadata(tags, metadata)
    :telemetry.execute(event_name, %{duration: duration}, metadata)
  end

  defp gauge(metric_name, value, %__MODULE__{handler_module: handler_module}, tags)
       when is_number(value) do
    event_name = [Thrift, handler_module, metric_name]
    metadata = tags_to_metadata(tags)
    :telemetry.execute(event_name, %{value: value}, metadata)
  end

  # Check that tags are simple and well-formed for use in systems like
  # OpenTSDB. This check is only done in test due to the performance cost.
  if Mix.env() == :test do
    defp validate_tag(tag) when is_atom(tag) do
      String.to_atom(validate_tag(Atom.to_string(tag)))
    end

    defp validate_tag(tag) when is_binary(tag) do
      false = tag =~ ~r{[^a-zA-Z_]}
      tag
    end
  else
    defmacrop validate_tag(tag) do
      tag
    end
  end

  defp tags_to_metadata(tags, metadata \\ %{}) do
    Enum.reduce(tags, metadata, fn
      {k, v}, metadata ->
        Map.put(metadata, validate_tag(k), validate_tag(v))
    end)
  end

  defp format_log(context, message, %__MODULE__{handler_module: handler_module}) do
    "#{inspect(handler_module)} (#{inspect(self())}) #{context}: #{message}"
  end
end
