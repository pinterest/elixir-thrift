defmodule Thrift.Generator.Server do
  @moduledoc false

  alias Thrift.Generator.{
    Service,
    Utils
  }

  alias Thrift.Parser.FileGroup

  def generate(service_module, service, file_group) do
    functions =
      service.functions
      |> Map.values()
      |> Enum.map(&generate_handler_function(file_group, service_module, &1))
      |> Utils.merge_blocks()
      |> Utils.sort_defs()

    quote do
      defmodule Binary.Framed.Server do
        @moduledoc false
        require Logger

        alias Thrift.Binary.Framed.Server, as: ServerImpl
        defdelegate stop(name), to: ServerImpl

        def start_link(handler_module, port, opts \\ []) do
          ServerImpl.start_link(__MODULE__, port, handler_module, opts)
        end

        unquote_splicing(functions)
      end
    end
  end

  def generate_handler_function(file_group, service_module, function) do
    rpc_name = Atom.to_string(function.name)
    args_module = Module.concat(service_module, Service.module_name(function, :args))
    response_module = Module.concat(service_module, Service.module_name(function, :response))

    struct_matches =
      Enum.map(function.params, fn param ->
        {param.name, Macro.var(param.name, nil)}
      end)

    quote do
      def deserialize(unquote(rpc_name), payload) do
        unquote(args_module).SerDe.deserialize(payload)
      end

      def handle_thrift(%unquote(args_module){unquote_splicing(struct_matches)}, handler_module) do
        unquote(build_handler_call(file_group, function, response_module))
      end
    end
  end

  defp build_handler_call(file_group, function, response_module) do
    handler_fn_name = Utils.underscore(function.name)
    handler_args = Enum.map(function.params, &Macro.var(&1.name, nil))

    body =
      quote do
        rsp = handler_module.unquote(handler_fn_name)(unquote_splicing(handler_args))
        unquote(build_responder(function.return_type, response_module))
      end

    wrap_with_try_catch(body, function, file_group, response_module)
  end

  defp wrap_with_try_catch(quoted_handler, function, file_group, response_module) do
    rescue_blocks =
      Enum.flat_map(function.exceptions, fn
        exc ->
          resolved = FileGroup.resolve(file_group, exc)
          dest_module = FileGroup.dest_module(file_group, resolved.type)
          error_var = Macro.var(exc.name, nil)
          field_setter = quote do: {unquote(exc.name), unquote(error_var)}

          quote do
            unquote(error_var) in unquote(dest_module) ->
              {:reply, %unquote(response_module){unquote(field_setter)}}
          end
      end)

    quote do
      try do
        unquote(quoted_handler)
      rescue
        unquote(rescue_blocks)
      end
    end
  end

  defp build_responder(:void, _) do
    quote do
      _ = rsp
      :noreply
    end
  end

  defp build_responder(_, response_module) do
    quote do
      {:reply, %unquote(response_module){success: rsp}}
    end
  end
end
