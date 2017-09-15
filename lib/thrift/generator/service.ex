defmodule Thrift.Generator.Service do
  @moduledoc false
  alias Thrift.Parser.FileGroup
  alias Thrift.{
    Generator,
    Generator.StructGenerator,
  }

  alias Thrift.Parser.Models.{
    Field,
    Function,
    Struct
  }

  # The response struct uses a %Field{} to represent the service function's
  # return value. Functions can return :void while fields cannot. Until we can
  # sort out that mismatch, disable dialyzer warnings for those functions.
  @dialyzer [{:nowarn_function, generate: 2},
             {:nowarn_function, generate_response_struct: 2}]

  def generate(schema, service) do
    file_group = schema.file_group
    dest_module = FileGroup.dest_module(file_group, service)

    functions = service.functions |> Map.values
    arg_structs = Enum.map(functions, &generate_args_struct(schema, &1))
    response_structs = for function <- functions, !function.oneway do
      generate_response_struct(schema, function)
    end

    framed_client = Generator.Binary.Framed.Client.generate(dest_module, service)
    framed_server = Generator.Binary.Framed.Server.generate(dest_module, service, file_group)

    service_module = quote do
      defmodule unquote(dest_module) do
        unquote_splicing(arg_structs)
        unquote_splicing(response_structs)

        unquote(framed_client)

        unquote(framed_server)
      end
    end

    {dest_module, service_module}
  end

  def generate_args_struct(schema, function) do
    arg_module_name = module_name(function, :args)

    struct = Struct.new(Atom.to_charlist(arg_module_name), function.params)

    StructGenerator.generate(:struct, schema, struct.name, struct)
  end

  def generate_response_struct(schema, function) do
    success = %Field{id: 0,
                     name: :success,
                     required: false,
                     type: function.return_type}

    exceptions = function.exceptions
    |> Enum.map(&Map.put(&1, :required, false))

    fields = [success | exceptions]

    response_module_name = module_name(function, :response)
    response_struct = Struct.new(Atom.to_charlist(response_module_name), fields)

    StructGenerator.generate(:struct, schema, response_struct.name, response_struct)
  end

  def module_name(%Function{} = function, suffix) do
    struct_name = "#{function.name}_#{suffix}"
    |> Macro.camelize
    |> String.to_atom

    Module.concat(Elixir, struct_name)
  end
end
