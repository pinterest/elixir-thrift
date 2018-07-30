defmodule Thrift.Binary.Framed.ProtocolHandler do
  @moduledoc """
  A GenServer that accepts connections on a server and processes the thrift messages.
  """

  @default_timeout 20_000
  @ssl_header_byte 0x16

  @typedoc "A module that implements the :ranch_transport behaviour"
  @type transport :: :ranch_tcp

  @typedoc "Transport-specific options"
  @type transport_opts :: :ranch_tcp.opts()

  alias Thrift.{
    Protocol,
    TApplicationException,
    Transport.SSL
  }

  require Logger

  @spec start_link(reference, port, transport, {module, module, transport_opts, [SSL.option()]}) ::
          GenServer.on_start()
  def start_link(
        ref,
        socket,
        transport,
        {server_module, handler_module, transport_opts, ssl_opts}
      ) do
    pid =
      spawn_link(__MODULE__, :init, [
        ref,
        socket,
        transport,
        server_module,
        handler_module,
        transport_opts,
        ssl_opts
      ])

    {:ok, pid}
  end

  @spec init(reference, port, :ranch_tcp, module, module, :ranch_tcp.opts(), [SSL.option()]) ::
          :ok | no_return
  def init(ref, socket, :ranch_tcp = transport, server_module, handler_module, tcp_opts, ssl_opts) do
    :ok = :ranch.accept_ack(ref)

    {recv_timeout, tcp_opts} = Keyword.pop(tcp_opts, :recv_timeout, @default_timeout)

    with {:ok, first_bytes} <- :gen_tcp.recv(socket, 4, recv_timeout),
         :ok <- :gen_tcp.unrecv(socket, first_bytes) do
      <<first_byte::8-unsigned, _::binary>> = first_bytes

      transport_options = Keyword.put(tcp_opts, :packet, 4)
      transport.setopts(socket, transport_options)

      maybe_ssl_handshake(
        socket,
        first_byte,
        ssl_opts,
        server_module,
        handler_module,
        recv_timeout
      )
    else
      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        :ok = transport.close(socket)

      {:error, reason} ->
        # :ssl.format_error handles posix errors as well as ssl errors
        Logger.info(fn ->
          "#{inspect(handler_module)} (#{inspect(self())}) connection error: #{
            :ssl.format_error(reason)
          } (#{inspect(reason)})"
        end)

        :ok = transport.close(socket)
    end
  end

  defp maybe_ssl_handshake(socket, first_byte, ssl_opts, server_module, handler_module, timeout) do
    with {optional, ssl_opts} when optional in [:required, :optional] <-
           SSL.configuration(ssl_opts),
         {:ok, transport, socket} <-
           maybe_ssl_accept(socket, first_byte, optional, ssl_opts, timeout) do
      do_thrift_call({transport, socket, server_module, handler_module, timeout})
    else
      nil ->
        do_thrift_call({:gen_tcp, socket, server_module, handler_module, timeout})

      {:error, %_exception{} = err} ->
        Logger.error(fn ->
          "#{inspect(handler_module)} (#{inspect(self())}) configuration error: " <>
            Exception.format(:error, err, [])
        end)

      {:error, reason} ->
        Logger.info(fn ->
          "#{inspect(handler_module)} (#{inspect(self())}) handshake error: #{
            :ssl.format_error(reason)
          } (#{inspect(reason)})"
        end)
    end
  end

  defp maybe_ssl_accept(socket, @ssl_header_byte, _optional, ssl_opts, timeout) do
    case :ssl.ssl_accept(socket, ssl_opts, timeout) do
      {:ok, ssl_sock} ->
        {:ok, :ssl, ssl_sock}

      error ->
        error
    end
  end

  defp maybe_ssl_accept(socket, _first_byte, :optional, _ssl_opts, _timeout) do
    {:ok, :gen_tcp, socket}
  end

  defp maybe_ssl_accept(_socket, _first_byte, :required, _ssl_opts, _timeout) do
    {:error, :closed}
  end

  defp do_thrift_call({transport, socket, server_module, handler_module, recv_timeout} = args) do
    with {:ok, message} <- transport.recv(socket, 0, recv_timeout),
         parsed <- Protocol.Binary.deserialize(:message_begin, message),
         {:ok, :reply, data} <- handle_thrift_message(parsed, server_module, handler_module),
         :ok <- transport.send(socket, data) do
      do_thrift_call(args)
    else
      {:error, {:server_error, thrift_data}} ->
        :ok = transport.send(socket, thrift_data)
        exit({:shutdown, :server_error})

      {:error, {:protocol_error, reason}} ->
        Logger.warn(fn ->
          "#{inspect(handler_module)} (#{inspect(self())}) decode error: #{inspect(reason)}"
        end)

        :ok = transport.close(socket)

      {:error, closed} when closed in [:closed, :econnreset, :timeout] ->
        :ok = transport.close(socket)

      {:error, reason} ->
        # :ssl.format_error handles posix error as well as ssl
        Logger.info(fn ->
          "#{inspect(handler_module)} (#{inspect(self())}) connection error: #{
            :ssl.format_error(reason)
          } (#{inspect(reason)})"
        end)

        :ok = transport.close(socket)
    end
  end

  defp handle_thrift_message(
         {:ok, {:call, sequence_id, name, args_binary}},
         server_module,
         handler_module
       ) do
    case server_module.handle_thrift(name, args_binary, handler_module) do
      {:reply, serialized_reply} ->
        message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

        {:ok, :reply, [message | serialized_reply]}

      {:server_error, %TApplicationException{} = exc} ->
        message = Protocol.Binary.serialize(:message_begin, {:exception, sequence_id, name})
        serialized_exception = Protocol.Binary.serialize(:application_exception, exc)

        {:error, {:server_error, [message | serialized_exception]}}

      :noreply ->
        message = Protocol.Binary.serialize(:message_begin, {:reply, sequence_id, name})

        {:ok, :reply, [message | <<0>>]}
    end
  end

  defp handle_thrift_message(
         {:ok, {:oneway, _seq_id, name, args_binary}},
         server_module,
         handler_module
       ) do
    spawn(server_module, :handle_thrift, [name, args_binary, handler_module])
    {:ok, :reply, <<0>>}
  end

  defp handle_thrift_message({:error, msg}, _, _) do
    {:error, {:protocol_error, msg}}
  end
end
