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

    bang_value_to_name_defs = Enum.map(enum.values, fn {key, value} ->
      enum_name = to_name(key)
      quote do
        def value_to_name!(unquote(value)), do: unquote(enum_name)
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
        def value_to_name(_), do: {:error, :invalid_enum_value}

        unquote_splicing(bang_value_to_name_defs)

        def names, do: unquote(names)

        unquote_splicing(member_defs)
        def member?(_), do: false
      end
    end
  end

  defp to_name(key) do
    key |> to_string |> String.downcase |> String.to_atom
  end
end
