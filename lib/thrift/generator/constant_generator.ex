defmodule Thrift.Generator.ConstantGenerator do
  @moduledoc false

  alias Thrift.Generator.Utils
  alias Thrift.Parser.Models.{
    Constant,
    Schema,
  }

  @spec generate(atom, [Constant.t], Schema.t) :: Macro.t
  def generate(full_name, constants, schema) do
    macro_defs = Enum.map(constants, fn(constant) ->
      name = Utils.underscore(constant.name)
      value = Utils.quote_value(constant.value, constant.type, schema)
      quote do
        defmacro unquote(Macro.var(name, nil)), do: Macro.escape(unquote(value))
      end
    end)
    quote do
      defmodule unquote(full_name) do
        unquote_splicing(macro_defs)
      end
    end
  end
end
