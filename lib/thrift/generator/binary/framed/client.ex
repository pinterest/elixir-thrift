defmodule Thrift.Generator.Binary.Framed.Client do
  @moduledoc false

  alias Thrift.Generator.{Service, Utils}
  alias Thrift.Parser.Models.Function

  def generate(service_module, service) do
    functions = service.functions
    |> Map.values
    |> Enum.map(&generate_handler_function(service_module, &1))
    |> Utils.merge_blocks

    quote do
      defmodule Binary.Framed.Client do
        @moduledoc false

        alias Thrift.Binary.Framed.Client, as: ClientImpl

        defdelegate close(conn), to: ClientImpl
        defdelegate connect(conn, opts), to: ClientImpl

        def start_link(host, port, opts \\ []) do
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
        args = %unquote(args_module){unquote_splicing(assignments)}
        serialized_args = unquote(args_module).BinaryProtocol.serialize(args)

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

  defp build_response_handler(%Function{oneway: true}, rpc_name, _) do
    quote do
      :ok = ClientImpl.oneway(client, unquote(rpc_name), serialized_args, opts)
      {:ok, nil}
    end
  end
  defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
    quote do
      case ClientImpl.call(client, unquote(rpc_name), serialized_args, opts) do
        {:ok, serialized_response} ->
          case unquote(Module.concat(response_module, :BinaryProtocol)).deserialize(serialized_response) do
            {%{success: nil} = resp, ""} ->
              responses = resp
              |> Map.from_struct
              |> Map.values
              |> Enum.reject(&is_nil(&1))

              case responses do
                [exception] ->
                  {:error, {:exception, exception}}

                [] ->
                  # This case is when we have a void return on the
                  # remote RPC
                  {:ok, nil}
              end

            {%{success: success}, ""} ->
              {:ok, success}
          end

        {:error, _} = err ->
          err
      end
    end
  end
end
