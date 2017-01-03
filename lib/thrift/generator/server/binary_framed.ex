defmodule Thrift.Generator.Server.BinaryFramed do
  @moduledoc false
  alias Thrift.Generator.{
    Service,
    Utils
  }
  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.Function
  alias Thrift.Servers.BinaryFramed

  def generate(service_module, service, file_group) do
    functions = service.functions
    |> Map.values
    |> Enum.map(&generate_handler_function(file_group, service_module, &1))

    quote do
      defmodule Server.Framed do
        @moduledoc false
        require Logger

        alias Thrift.Servers.BinaryFramed
        defdelegate stop(name), to: BinaryFramed

        def start_link(handler_module, port, opts) do
          BinaryFramed.start_link(__MODULE__, port, handler_module, opts)
        end

        unquote_splicing(functions)
      end
    end
  end

  def generate_handler_function(file_group, service_module, %Function{params: []} = function) do
    fn_name = Atom.to_string(function.name)
    handler_fn_name = Utils.underscore(function.name)
    response_module = service_module
    |> Module.concat(Service.module_name(function, :response))

    handler = quote do
      rsp = handler_module.unquote(handler_fn_name)()
      unquote(build_responder(function.return_type, response_module))
    end
    |> wrap_with_try_catch(function, file_group, response_module)

    quote do
      def handle_thrift(unquote(fn_name), _binary_data, handler_module) do
        unquote(handler)
      end
    end
  end
  def generate_handler_function(file_group, service_module, function) do
    fn_name = Atom.to_string(function.name)
    args_module = service_module
    |> Module.concat(Service.module_name(function, :args))

    response_module = service_module
    |> Module.concat(Service.module_name(function, :response))

    struct_matches = function.params
    |> Enum.map(fn param ->
      {param.name, Macro.var(param.name, nil)}
    end)

    quote do
      def handle_thrift(unquote(fn_name), binary_data, handler_module) do
        case unquote(args_module).BinaryProtocol.deserialize(binary_data) do
          {%unquote(args_module){unquote_splicing(struct_matches)}, ""} ->
            unquote(build_handler_call(file_group, function, response_module))

          {_, extra} ->
            raise Thrift.TApplicationException, [
              message: "Could not decode #{inspect extra}",
              type: :protocol_error
            ]
        end
      end
    end
  end

  defp build_handler_call(file_group, function, response_module) do
    handler_fn_name = Utils.underscore(function.name)
    handler_args = function.params
    |> Enum.map(&Macro.var(&1.name, nil))

    quote do
      rsp = handler_module.unquote(handler_fn_name)(unquote_splicing(handler_args))
      unquote(build_responder(function.return_type, response_module))
    end
    |> wrap_with_try_catch(function, file_group, response_module)
  end

  defp wrap_with_try_catch(quoted_handler, function, file_group, response_module) do
    rescue_blocks = function.exceptions
    |> Enum.flat_map(fn
      exc ->
        resolved = FileGroup.resolve(file_group, exc)
        dest_module = FileGroup.dest_module(file_group, resolved.type)
        error_var = Macro.var(exc.name, nil)
        field_setter = quote do: {unquote(exc.name), unquote(error_var)}

        quote do
          unquote(error_var) in unquote(dest_module) ->
            serialized_exception = %unquote(response_module){unquote(field_setter)}
            |> unquote(response_module).BinaryProtocol.serialize()
            {:reply, serialized_exception}
        end
    end)

    quote do
      try do
        unquote(quoted_handler)
      rescue
        unquote(rescue_blocks)
      catch kind, reason ->
        formatted_exception = Exception.format(kind, reason, System.stacktrace)
        Logger.error("Exception not defined in thrift spec was thrown: #{formatted_exception}")
        {:server_error, Thrift.TApplicationException.exception(
          message: "Server error: #{formatted_exception}",
          type: :internal_error)}
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
      response = %unquote(response_module){success: rsp}
      {:reply, unquote(response_module).BinaryProtocol.serialize(response)}
    end
  end
end
