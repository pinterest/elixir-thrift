defmodule Thrift.Generator.EnumGenerator do

  def generate(name, enum) do
    macro_defs = Enum.map(enum.values, fn {key, _value} ->
      macro_name = key
      |> to_name
      |> Macro.pipe(quote do unquote end, 0)

      quote do
        defmacro unquote(macro_name)() do
          IO.warn "Thrift enum macros are deprecated. " <>
              "See https://github.com/pinterest/elixir-thrift/issues/312."
          unquote(key)
        end
      end
    end)

    member_defs = Enum.map(enum.values, fn {key, _value} ->
      quote do
        def member?(unquote(key)), do: true
      end
    end)

    value_to_name_defs = Enum.map(enum.values, fn {key, value} ->
      quote do
        def value_to_name(unquote(value)), do: {:ok, unquote(key)}
      end
    end)

    name_to_value_defs = Enum.map(enum.values, fn {key, value} ->
      quote do
        def name_to_value(unquote(key)), do: {:ok, unquote(value)}
      end
    end)

    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{enum.name}")
        unquote_splicing(macro_defs)

        unquote_splicing(value_to_name_defs)
        def value_to_name(v), do: {:error, {:invalid_enum_value, v}}

        unquote_splicing(name_to_value_defs)
        def name_to_value(k), do: {:error, {:invalid_enum_name, k}}

        def value_to_name!(value) do
          {:ok, name} = value_to_name(value)
          name
        end

        def name_to_value!(name) do
          {:ok, value} = name_to_value(name)
          value
        end

        def meta(:names), do: unquote(Keyword.keys(enum.values))
        def meta(:values), do: unquote(Keyword.values(enum.values))

        unquote_splicing(member_defs)
        def member?(_), do: false
      end
    end
  end

  defp to_name(key) do
    key |> to_string |> String.downcase |> String.to_atom
  end
end
