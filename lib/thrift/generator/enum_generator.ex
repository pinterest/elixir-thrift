defmodule Thrift.Generator.EnumGenerator do

  def generate(name, enum) do
    macro_defs = Enum.map(enum.values, fn {key, value} ->
      macro_name = to_ordinal(key)
      quote do
        defmacro unquote(Macro.var(macro_name, nil)), do: unquote(value)
      end
    end)

    member_defs = Enum.map(enum.values, fn {_key, value} ->
      quote do
        def member?(unquote(value)), do: true
      end
    end)

    ordinal_defs = Enum.map(enum.values, fn {key, value} ->
      ordinal_name = to_ordinal(key)
      quote do
        def ordinal(unquote(value)), do: unquote(ordinal_name)
      end
    end)

    ordinals = enum.values
    |> Keyword.keys
    |> Enum.map(&to_ordinal/1)

    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{enum.name}")
        unquote_splicing(macro_defs)

        unquote_splicing(ordinal_defs)
        def ordinal(_), do: :"!!INVALID"

        def ordinals, do: unquote(ordinals)

        unquote_splicing(member_defs)
        def member?(_), do: false
      end
    end
  end

  defp to_ordinal(key) do
    key |> to_string |> String.downcase |> String.to_atom
  end
end
