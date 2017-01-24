defmodule Thrift.Generator.EnumGenerator do

  def generate(name, enum) do
    macro_defs = Enum.map(enum.values, fn {key, value} ->
      macro_name = to_name(key)
      quote do
        defmacro unquote(Macro.var(macro_name, nil)), do: unquote(value)
      end
    end)

    member_defs = Enum.map(enum.values, fn {_key, value} ->
      quote do
        def member?(unquote(value)), do: true
      end
    end)

    value_to_name_defs = Enum.map(enum.values, fn {key, value} ->
      enum_name = to_name(key)
      quote do
        def value_to_name(unquote(value)), do: {:ok, unquote(enum_name)}
      end
    end)

    names = enum.values
    |> Keyword.keys
    |> Enum.map(&to_name/1)

    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{enum.name}")
        unquote_splicing(macro_defs)

        unquote_splicing(value_to_name_defs)
        def value_to_name(v), do: {:error, {:invalid_enum_value, v}}

        def value_to_name!(value) do
          {:ok, name} = value_to_name(value)
          name
        end

        def names, do: unquote(names)
        def values, do: unquote(Keyword.values(enum.values))

        unquote_splicing(member_defs)
        def member?(_), do: false
      end
    end
  end

  defp to_name(key) do
    key |> to_string |> String.downcase |> String.to_atom
  end
end
