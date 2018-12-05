defmodule Mix.Tasks.Thrift.HttpClient.Generate do
  use Mix.Task
  require Logger

  @shortdoc "Generates Elixir source files for client HTTP thrift service"

  @moduledoc """
  Generates Elixir HTTP thrift client for a given service

  To generate a client:

      mix thrift.http_client.generate  thrift/calculator.thrift

  It assumes that you have already generated the thrift support files with

      mix compile.thrift  thrift/calculator.thrift

  Each service requires an endpoint to be configured in your config file

      config :calculator, Calculator.Generated.HTTP.Client,
                        endpoint: "http://localhost:8080/TCalculatorServlet/Calculator"

  """

  @default_namespace "Thrift.Generated"

  # ----------------------------------------------------------------------------
  def run(args) do
    Logger.info("Compiling HTTP Thrift Service ...")

    {opts, files} =
      OptionParser.parse!(
        args,
        switches: [out: :string],
        aliases: []
      )

    output_path = opts[:out] || "lib"

    for file <- files do
      file
      |> Thrift.Parser.parse_file()
      |> Map.get(:schemas)
      |> Enum.each(fn {_, schema} ->
        namespace = get_namespace(schema)
        dest_dir = get_destination_dir(namespace, output_path)

        modules =
          Enum.map(schema.services, fn {service_name, service_spec} ->
            gen_module(namespace, service_name, service_spec)
          end)

        code =
          quote do
            unquote(gen_http_helper(namespace))
            unquote_splicing(modules)
          end

        File.mkdir("#{dest_dir}/http")
        output_file = "#{dest_dir}/http/client.ex"

        File.write!(output_file, Macro.to_string(code))
        System.cmd("mix", ["format", output_file])
      end)
    end

    :ok
  end

  # ----------------------------------------------------------------------------
  def get_namespace(schema) do
    schema.namespaces
    |> Map.get(:elixir, %Thrift.AST.Namespace{
      line: -1,
      scope: :elixir,
      value: @default_namespace
    })
    |> Map.get(:value)
  end

  # ----------------------------------------------------------------------------
  def get_destination_dir(namespace, output_path) do
    schema_dir =
      namespace
      |> String.split(".")
      |> Enum.map(&Macro.underscore/1)
      |> Path.join()

    "#{output_path}/#{schema_dir}"
  end

  # ----------------------------------------------------------------------------
  def gen_http_helper(namespace) do
    module_name = Module.concat([namespace, HTTP, Client])

    Logger.info("""
      Generating HTTP Client for #{namespace}
      NOTE:
      Make sure to update your config files with
      config :your_app, #{Macro.to_string(module_name)},
                        endpoint: "http://x.x.y.y:8080/some_endpoint"
    """)

    quote do
      defmodule unquote(module_name) do
        require Logger

        @default_headers [
          Accept: "application/x-thrift",
          "Content-Type": "application/x-thrift"
        ]
        @http_client_options [hackney: [pool: :default]]
        @service_endpoint Application.get_env(
                            Mix.Project.config()[:app],
                            unquote(module_name),
                            %{}
                          )[:endpoint]

        def post_thrift(serialized_binary) do
          if @service_endpoint == nil do
            Logger.error(
              "service endpoint not defined for #{Macro.to_string(__MODULE__)}!! Please add it to config file!"
            )
          end

          case HTTPoison.post(
                 @service_endpoint,
                 serialized_binary,
                 @default_headers,
                 @http_client_options
               ) do
            {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
              {:ok, body}

            {:ok, %HTTPoison.Response{status_code: code, body: body}} ->
              {:service_err, code, body}

            {:error, %HTTPoison.Error{reason: reason}} ->
              {:http_err, inspect(reason)}
          end
        end
      end
    end
  end

  # ----------------------------------------------------------------------------
  def gen_module(namespace, service_name, service_spec) do
    module_name = Module.concat([namespace, HTTP, service_name])

    functions =
      service_spec.functions
      |> Enum.map(&gen_function(&1, namespace, service_name))

    quote do
      defmodule unquote(module_name) do
        alias unquote(Module.concat(namespace, service_name))
        alias unquote(Module.concat([namespace, HTTP]))
        alias Thrift.Protocol.Binary

        unquote_splicing(functions)
      end
    end
  end

  # ----------------------------------------------------------------------------
  def gen_function({_name, %Thrift.AST.Function{} = function}, namespace, service_name) do
    args_module = module_name(function, service_name, :args)

    args_binary_module = Module.concat(args_module, :BinaryProtocol)

    response_module = module_name(function, service_name, :response)

    function_name = to_snake_case(function.name)

    vars = Enum.map(function.params, &Macro.var(&1.name, nil))

    assignments =
      function.params
      |> Enum.zip(vars)
      |> Enum.map(fn {param, var} ->
        quote do
          {unquote(param.name), unquote(var)}
        end
      end)

    doc =
      function.params
      |> Enum.map(fn %Thrift.AST.Field{name: name, type: type} ->
        "#{name}: #{param_type(namespace, type)}"
      end)
      |> Enum.join("\n\n")

    quote do
      @doc unquote(doc)

      def(unquote(function_name)(unquote_splicing(vars))) do
        args = %unquote(args_module){unquote_splicing(assignments)}
        serialized_args = unquote(args_binary_module).serialize(args)

        tcall =
          Binary.serialize(
            :message_begin,
            {:call, 0, unquote(Atom.to_string(function.name))}
          )

        payload = [tcall | serialized_args] |> IO.iodata_to_binary()

        with {:ok, body} <- HTTP.Client.post_thrift(payload),
             {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
             {response, _} <- unquote(response_module).deserialize(ser_resp) do
          response
        else
          {:service_err, code, body} ->
            Thrift.TApplicationException.exception(
              type: :missing_result,
              message: "service error [#{code}] #{body}"
            )

          {:http_err, err_str} ->
            Thrift.TApplicationException.exception(
              type: :missing_result,
              message: "http error: #{err_str}"
            )

          {:ok, {:exception, _, _, ser_ex}} ->
            Binary.deserialize(:application_exception, ser_ex)
        end
      end
    end
  end

  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  def module_name(%Thrift.AST.Function{} = function, service_name, suffix) do
    struct_name =
      "#{function.name}_#{suffix}"
      |> Macro.camelize()

    Module.concat(Elixir, "#{service_name}.#{struct_name}" |> String.to_atom())
  end

  def to_snake_case(name) do
    name
    |> Atom.to_string()
    |> Macro.underscore()
    |> String.to_atom()
  end

  # Type helpers for func doc generation  --------------------------------------
  def param_type(namespace, %Thrift.AST.TypeRef{referenced_type: type}) do
    type = Module.concat(namespace, type) |> Macro.to_string()
    "`#{type}`"
  end

  # list of
  def param_type(namespace, {:list, %Thrift.AST.TypeRef{referenced_type: type}}) do
    type = Module.concat(namespace, type) |> Macro.to_string()
    "[`#{type}`]"
  end

  def param_type(_namespace, simple_type) do
    simple_type
  end
end
