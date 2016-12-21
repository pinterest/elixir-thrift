defmodule Thrift.Generator.Client do
  defmodule Framed do
    alias Thrift.Generator.Service
    alias Thrift.Parser.Models.Function

    def generate(service_module, service) do
      functions = service.functions
      |> Map.values
      |> Enum.map(&generate_function(service_module, &1))

      quote do
        defmodule Client.Framed do
          alias Thrift.Protocols.Binary
          alias Thrift.TApplicationException

          defmodule State do
            defstruct host: nil, port: nil, tcp_opts: nil, timeout: 5000, sock: nil
          end

          use Connection

          def init({host, port, tcp_opts, timeout}) do
            s = %State{host: host,
                       port: port,
                       tcp_opts: tcp_opts,
                       timeout: timeout,
                       sock: nil}

            {:connect, :init, s}
          end

          def start_link(host, port, tcp_opts, timeout \\ 5000) do
            Connection.start_link(__MODULE__, {host, port, tcp_opts, timeout})
          end

          def close(conn), do: Connection.call(conn, :close)

          def connect(_, %{sock: nil, host: host, port: port, tcp_opts: opts, timeout: timeout} = s) do
            case :gen_tcp.connect(host, port, [active: false, packet: 4] ++ opts, timeout) do
              {:ok, sock} ->
                {:ok, %{s | sock: sock}}

              {:error, _} ->
                {:backoff, 1000, s}
            end
          end

          def disconnect(info, %{sock: sock} = s) do
            :ok = :gen_tcp.close(sock)
            case info do
              {:close, from} ->
                Connection.reply(from, :ok)

              {:error, :closed} ->
                :error_logger.format("Connection closed~n", [])

              {:error, reason} ->
                reason = :inet.format_error(reason)
                :error_logger.format("Connection error: ~s~n", [reason])
            end
            {:connect, :reconnect, %{s | sock: nil}}
          end

          unquote_splicing(functions)

          def handle_call(_, _, %{sock: nil} = s) do
            {:reply, {:error, :closed}, s}
          end

          def handle_call({:send, data}, _, %{sock: sock} = s) do
            case :gen_tcp.send(sock, data) do
              :ok ->
                {:reply, :ok, s}
              {:error, _} = error ->
                {:disconnect, error, error, s}
            end
          end

          def handle_call({:recv, bytes, timeout}, _, %{sock: sock} = s) do
            case :gen_tcp.recv(sock, bytes, timeout) do
              {:ok, _} = ok ->
                {:reply, ok, s}

              {:error, :timeout} = timeout ->
                {:reply, timeout, s}

              {:error, _} = error ->
                {:disconnect, error, error, s}
            end
          end

          def handle_call(:close, from, s) do
            {:disconnect, {:close, from}, s}
          end

          defp deserialize_message_reply(message, rpc_name, sequence_id, reply_module) do
            Binary.deserialize(:message_begin, message)
            |> handle_message(sequence_id, rpc_name, reply_module)
          end

          def handle_message({:ok, {:reply, decoded_sequence_id, decoded_rpc_name, decoded_response}},
                             sequence_id, rpc_name, reply_module)
          when (decoded_sequence_id == sequence_id) and (decoded_rpc_name == rpc_name) do

            case reply_module.deserialize(decoded_response) do
              {%{success: nil}=resp, ""} ->

                response = resp
                |> Map.delete(:__struct__)
                |> Map.values
                |> Enum.reject(&is_nil(&1))

                case response do
                  [exception] ->
                    {:error, {:exception, exception}}

                  [] ->
                  # This case is when we have a void return on the
                  # remote RPC
                  {:ok, nil}
                end

              {%{success: success}, ""} when not is_nil(success) ->
                {:ok, success}

                {resp, extra} ->
                {:error, {:extraneous_data, resp, extra}}
           end
          end

          def handle_message({:ok, {:exception, decoded_sequence_id, decoded_rpc_name, response}}, sequence_id, rpc_name, _)
          when decoded_sequence_id == sequence_id and decoded_rpc_name == rpc_name do

            exc = read_t_application_exception(response, %TApplicationException{})
            {:error, {:exception, exc}}
          end

          def handle_message({:ok, {_, decoded_sequence_id, _, decoded_rpc_name, _}},
                             sequence_id, rpc_name, _) do
            ex = case {decoded_sequence_id, decoded_rpc_name} do
                   {^sequence_id, mismatched_rpc_name} ->
                     message = "The server replied to #{mismatched_rpc_name}, but we sent #{rpc_name}"
                     %TApplicationException{message: message,
                                            type: TApplicationException.exception_type(3)}

                   {mismatched_sequence_id, ^rpc_name} ->
                     message = "Invalid sequence id. The client sent #{sequence_id}, but the server replied with #{mismatched_sequence_id}"

                     %TApplicationException{message: message,
                                            type: TApplicationException.exception_type(4)}

                   {_mismatched_sequence_id, _mismatched_rpc_name} ->
                     message = "Both sequence id and rpc name are wrong. The server is extremely uncompliant."
                     %TApplicationException{message: message,
                                            type: :sequence_id_and_rpc_name_mismatched}
                 end
            {:error, {:exception, ex}}
          end

          def handle_message({:error, _} = err, _, _, _) do
            err
          end

          defp read_t_application_exception(
                <<11::size(8),
                1::16-unsigned,
                message_size::32-signed,
                message::binary-size(message_size),
                rest::binary>>, accum) do
            # read the message string
            read_t_application_exception(rest, Map.put(accum, :message, message))
          end
          defp read_t_application_exception(
                <<8::size(8),
                2::16-unsigned,
                type::32-signed,
                rest::binary>>, accum) do
            # read the type
            exception_type = TApplicationException.exception_type(type)
            read_t_application_exception(rest, Map.put(accum, :type, exception_type))
          end
          defp read_t_application_exception(<<0>>, accum) do
            # read the field stop and return
            accum
          end
          defp read_t_application_exception(error, _) do
            message = "Could not decode TApplicationException, remaining was #{inspect error}"
            %TApplicationException{message: message,
                                   type: TApplicationException.exception_type(7)}
          end
        end
      end
    end

    def generate_function(service_module, function) do

      args_module = service_module
      |> Module.concat(Service.service_module_name(function, :args))

      response_module = Service.service_module_name(function, :response)

      underscored_name = function.name
      |> Atom.to_string
      |> Macro.underscore
      |> String.to_atom

      bang_name = :"#{underscored_name}!"

      vars = function.params
      |> Enum.map(&Macro.var(&1.name, nil))

      assignments = function.params
      |> Enum.zip(vars)
      |> Enum.map(fn {param, var} ->
        quote do
          {unquote(param.name), unquote(var)}
        end
      end)

      rpc_name = Atom.to_string(function.name)

      quote do
        def unquote(underscored_name)(client, unquote_splicing(vars), timeout \\ 5000) do
          serialized_args = %unquote(args_module){unquote_splicing(assignments)}
          |> unquote(args_module).BinaryProtocol.serialize

          sequence_id = :erlang.unique_integer([:positive])
          message = Binary.serialize(
            :message_begin,
            {unquote(message_type(function)), sequence_id, unquote(rpc_name)})

          GenServer.call(client, {:send, [message | serialized_args]})

          unquote(build_response_handler(function, rpc_name, response_module))
        end
        def unquote(bang_name)(client, unquote_splicing(vars), timeout \\ 5000) do
          case unquote(underscored_name)(client, unquote_splicing(vars), timeout) do
            {:ok, rsp} ->
              rsp

            {:error, {:exception, ex}} ->
              raise ex

            {:error, _}=err ->
              raise err
          end
        end
      end
    end

    defp message_type(%Function{oneway: true}), do: :oneway
    defp message_type(%Function{oneway: false}), do: :call

    defp build_response_handler(%Function{oneway: true}, _, _) do
      quote do
        _timeout = timeout
        {:ok, nil}
      end
    end
    defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
      quote do
        case GenServer.call(client, {:recv, 0, timeout}) do
          {:ok, message} ->

            message
            |> IO.iodata_to_binary
            |> deserialize_message_reply(unquote(rpc_name), sequence_id, unquote(response_module))
          {:error, _} = err ->
            err
        end
      end
    end
  end
end
