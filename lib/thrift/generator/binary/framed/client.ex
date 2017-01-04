defmodule Thrift.Generator.Binary.Framed.Client do
  @moduledoc false

  alias Thrift.Generator.Service
  alias Thrift.Parser.Models.Function

  def generate(service_module, service) do
    functions = service.functions
    |> Map.values
    |> Enum.map(&generate_handler_function(service_module, &1))

    quote do
      defmodule Binary.Framed.Client do
        @moduledoc false

        alias Thrift.Binary.Framed.Client, as: ClientImpl
        alias Thrift.Protocol.Binary

        defdelegate close(conn), to: ClientImpl
        defdelegate connect(conn, opts), to: ClientImpl

        def start_link(host, port, opts) do
          ClientImpl.start_link(host, port, opts)
        end
        unquote_splicing(functions)
      end
    end
  end

  defp generate_handler_function(service_module, function) do
    args_module = service_module
    |> Module.concat(Service.module_name(function, :args))

    response_module = Service.module_name(function, :response)

    underscored_name = function.name
    |> Atom.to_string
    |> Macro.underscore
    |> String.to_atom

    underscored_options_name = :"#{underscored_name}_with_options"
    bang_name = :"#{underscored_name}!"
    options_bang_name = :"#{underscored_options_name}!"

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

    def_type = if function.oneway do
      quote do: defp
    else
      quote do: def
    end

    quote do
      unquote(def_type)(unquote(underscored_options_name)(client, unquote_splicing(vars), opts)) do
        serialized_args = %unquote(args_module){unquote_splicing(assignments)}
        |> unquote(args_module).BinaryProtocol.serialize

        sequence_id = :erlang.unique_integer([:positive])
        message = Binary.serialize(
          :message_begin,
          {unquote(message_type(function)), sequence_id, unquote(rpc_name)})

        unquote(build_response_handler(function, rpc_name, response_module))
      end

      def unquote(underscored_name)(client, unquote_splicing(vars)) do
        unquote(underscored_options_name)(client, unquote_splicing(vars), [])
      end

      unquote(def_type)(unquote(options_bang_name)(client, unquote_splicing(vars), opts)) do
        case unquote(underscored_options_name)(client, unquote_splicing(vars), opts) do
          {:ok, rsp} ->
            rsp

          {:error, {:exception, ex}} ->
            raise ex

          {:error, _} = err ->
            raise err
        end
      end

      def unquote(bang_name)(client, unquote_splicing(vars)) do
        unquote(options_bang_name)(client, unquote_splicing(vars), [])
      end
    end
  end

  defp message_type(%Function{oneway: true}), do: :oneway
  defp message_type(%Function{oneway: false}), do: :call

  defp build_response_handler(%Function{oneway: true}, _, _) do
    quote do
      _ = opts
      ClientImpl.oneway(client, [message | serialized_args])

      {:ok, nil}
    end
  end
  defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
    quote do

      case ClientImpl.request(client, [message | serialized_args], opts) do
        {:ok, message} ->
          message
          |> ClientImpl.deserialize_message_reply(unquote(rpc_name), sequence_id, unquote(response_module))

        {:error, _} = err ->
          err
      end
    end
  end
end
