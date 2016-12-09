defmodule Thrift.Generator.EnumGenerator do
  def generate(name, enum) do
    macro_defs = Enum.map(enum.values, fn {key, value} ->
      macro_name = key |> to_string |> String.downcase |> String.to_atom
      quote do
        defmacro unquote(Macro.var(macro_name, nil)), do: unquote(value)
      end
    end)
    member_defs = Enum.map(enum.values, fn {_key, value} ->
      quote do
        def member?(unquote(value)), do: true
      end
    end)
    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{enum.name}")
        unquote_splicing(macro_defs)
        unquote_splicing(member_defs)
        def member?(_), do: false
      end
    end
  end
end
