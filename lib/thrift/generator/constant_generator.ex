defmodule Thrift.Generator.ConstantGenerator do
  @moduledoc false

  alias Thrift.Generator.Utils
  alias Thrift.Generator.StructGenerator

  def generate(full_name, constants, schema) do
    constant_defs = Enum.map(constants, fn(constant) ->
      name = Utils.underscore(constant.name)
      value = StructGenerator.default_value(constant.value, constant.type, schema)
      quote do
        def unquote(name)() do
          unquote(value)
        end
      end
    end)
    quote do
      defmodule unquote(full_name) do
        unquote_splicing(constant_defs)
      end
    end
  end
end
