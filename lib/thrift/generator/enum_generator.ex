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

    name_member_defs = Enum.map(enum.values, fn {key, _value} ->
      enum_name = to_name(key)
      quote do
        def name?(unquote(enum_name)), do: true
      end
    end)

    value_to_name_defs = Enum.map(enum.values, fn {key, value} ->
      enum_name = to_name(key)
      quote do
        def value_to_name(unquote(value)), do: {:ok, unquote(enum_name)}
      end
    end)

    name_to_value_defs = Enum.map(enum.values, fn {key, value} ->
      enum_name = to_name(key)
      quote do
        def name_to_value(unquote(enum_name)), do: {:ok, unquote(value)}
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

        def meta(:names), do: unquote(names)
        def meta(:values), do: unquote(Keyword.values(enum.values))

        unquote_splicing(member_defs)
        def member?(_), do: false

        unquote_splicing(name_member_defs)
        def name?(_), do: false
      end
    end
  end

  defp to_name(key) do
    key |> to_string |> String.downcase |> String.to_atom
  end
end
