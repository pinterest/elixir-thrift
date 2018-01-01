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
    args_module = Module.concat(service_module, Service.module_name(function, :args))
    response_module = Service.module_name(function, :response)
    rpc_name = Atom.to_string(function.name)

    # Make two Elixir-friendly function names: an underscored version of the
    # Thrift function name and a "bang!" exception-raising variant.
    function_name = function.name
    |> Atom.to_string
    |> Macro.underscore
    |> String.to_atom
    bang_name = :"#{function_name}!"

    # Apply some macro magic to the names to avoid conflicts with Elixir
    # reserved symbols like "and".
    function_name = Macro.pipe(function_name, quote do unquote end, 0)
    bang_name = Macro.pipe(bang_name, quote do unquote end, 0)

    vars = Enum.map(function.params, &Macro.var(&1.name, nil))

    assignments = function.params
    |> Enum.zip(vars)
    |> Enum.map(fn {param, var} ->
      quote do
        {unquote(param.name), unquote(var)}
      end
    end)

    quote do
      def(unquote(function_name)(client, unquote_splicing(vars), rpc_opts \\ [])) do
        args = %unquote(args_module){unquote_splicing(assignments)}
        serialized_args = unquote(args_module).BinaryProtocol.serialize(args)
        unquote(build_response_handler(function, rpc_name, response_module))
      end

      def(unquote(bang_name)(client, unquote_splicing(vars), rpc_opts \\ [])) do
        case unquote(function_name)(client, unquote_splicing(vars), rpc_opts) do
          {:ok, rsp} ->
            rsp

          {:error, {:exception, ex}} ->
            raise ex

          {:error, _} = err ->
            raise err
        end
      end
    end
  end

  defp build_response_handler(%Function{oneway: true}, rpc_name, _response_module) do
    quote bind_quoted: [rpc_name: rpc_name] do
      :ok = ClientImpl.oneway(client, rpc_name, serialized_args, rpc_opts)
      {:ok, nil}
    end
  end
  defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
    deserialize_module = Module.concat(response_module, :BinaryProtocol)
    quote bind_quoted: [deserialize_module: deserialize_module, rpc_name: rpc_name] do
      ClientImpl.call(client, rpc_name, serialized_args, deserialize_module, rpc_opts)
    end
  end
end
