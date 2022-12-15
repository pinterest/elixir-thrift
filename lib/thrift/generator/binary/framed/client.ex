defmodule Thrift.Generator.Binary.Framed.Client do
  @moduledoc false

  alias Thrift.AST.Function
  alias Thrift.Generator.{Service, Utils}

  def generate(service) do
    functions =
      service.functions
      |> Map.values()
      |> Enum.map(&generate_handler_function(&1))
      |> Utils.merge_blocks()

    quote do
      defmodule Binary.Framed.Client do
        @moduledoc false

        alias Thrift.Binary.Framed.Client, as: ClientImpl

        defdelegate close(conn), to: ClientImpl
        defdelegate connect(conn, opts), to: ClientImpl
        defdelegate start_link(host, port, opts \\ []), to: ClientImpl

        unquote_splicing(functions)
      end
    end
  end

  defp generate_handler_function(function) do
    args_module = Service.module_name(function, :args)
    response_module = Service.module_name(function, :response)
    rpc_name = Atom.to_string(function.name)

    # Make two Elixir-friendly function names: an underscored version of the
    # Thrift function name and a "bang!" exception-raising variant.
    function_name =
      function.name
      |> Atom.to_string()
      |> Macro.underscore()
      |> String.to_atom()

    bang_name = :"#{function_name}!"

    # Apply some macro magic to the names to avoid conflicts with Elixir
    # reserved symbols like "and".
    function_name =
      Macro.pipe(
        function_name,
        quote do
          unquote
        end,
        0
      )

    bang_name =
      Macro.pipe(
        bang_name,
        quote do
          unquote
        end,
        0
      )

    vars = Enum.map(function.params, &Macro.var(&1.name, nil))

    assignments =
      function.params
      |> Enum.zip(vars)
      |> Enum.map(fn {param, var} ->
        quote do
          {unquote(param.name), unquote(var)}
        end
      end)

    quote do
      def(unquote(function_name)(client, unquote_splicing(vars), rpc_opts \\ [])) do
        args = %unquote(args_module){unquote_splicing(assignments)}
        serialized_args = unquote(args_module).serialize(args)
        unquote(build_response_handler(function, rpc_name, response_module))
      end

      def(unquote(bang_name)(client, unquote_splicing(vars), rpc_opts \\ [])) do
        case unquote(function_name)(client, unquote_splicing(vars), rpc_opts) do
          {:ok, rsp} ->
            rsp

          {:error, {:exception, ex}} ->
            raise ex

          {:error, reason} ->
            raise Thrift.ConnectionError, reason: reason
        end
      end
    end
  end

  defp build_response_handler(%Function{oneway: true}, rpc_name, _response_module) do
    quote do
      :ok = ClientImpl.oneway(client, unquote(rpc_name), serialized_args, rpc_opts)
      {:ok, nil}
    end
  end

  defp build_response_handler(%Function{oneway: false}, rpc_name, response_module) do
    quote do
      ClientImpl.call(client, unquote(rpc_name), serialized_args, unquote(response_module), rpc_opts)
    end
  end
end
