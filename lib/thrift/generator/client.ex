defmodule Thrift.Generator.Client do
  defmodule Framed do
    alias Thrift.Generator.Service
    alias Thrift.Parser.Models.Function
    alias Thrift.Clients

    def generate(service_module, service) do
      functions = service.functions
      |> Map.values
      |> Enum.map(&generate_function(service_module, &1))

      quote do
        defmodule Client.Framed do
          alias Thrift.Clients
          alias Thrift.Protocols.Binary

          def start_link(host, port, tcp_opts, timeout \\ 5000) do
            Clients.Binary.start_link(host, port, tcp_opts, timeout)
          end
          unquote_splicing(functions)
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
        GenServer.call(client, {:oneway, [message | serialized_args]})

        {:ok, nil}
      end
    end
    defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
      quote do
        case GenServer.call(client, {:request, [message | serialized_args], timeout}) do
          {:ok, message} ->
            message
            |> IO.iodata_to_binary
            |> Clients.Binary.deserialize_message_reply(unquote(rpc_name), sequence_id, unquote(response_module))

          {:error, _} = err ->
            err
        end
      end
    end
  end
end
