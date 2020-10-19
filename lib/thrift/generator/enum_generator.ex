defmodule Thrift.Generator.EnumGenerator do
  @moduledoc false

  def generate(name, enum) do
    macro_defs =
      Enum.map(enum.values, fn {key, value} ->
        macro_name =
          key
          |> to_name
          |> Macro.pipe(
            quote do
              unquote
            end,
            0
          )

        quote do
          defmacro unquote(macro_name)(), do: unquote(value)
        end
      end)

    member_defs =
      Enum.flat_map(enum.values, fn {_key, value} ->
        quote do
          unquote(value) -> true
        end
      end)

    name_member_defs =
      Enum.flat_map(enum.values, fn {key, _value} ->
        enum_name = to_name(key)

        quote do
          unquote(enum_name) -> true
        end
      end)

    value_to_name_defs =
      Enum.flat_map(enum.values, fn {key, value} ->
        enum_name = to_name(key)

        quote do
          unquote(value) -> {:ok, unquote(enum_name)}
        end
      end)

    name_to_value_defs =
      Enum.flat_map(enum.values, fn {key, value} ->
        enum_name = to_name(key)

        quote do
          unquote(enum_name) -> {:ok, unquote(value)}
        end
      end)

    names =
      enum.values
      |> Keyword.keys()
      |> Enum.map(&to_name/1)

    quote do
      defmodule unquote(name) do
        @moduledoc false
        unquote_splicing(macro_defs)

        def value_to_name(v) do
          case v do
            unquote(
              value_to_name_defs ++
                quote do
                  _ -> {:error, {:invalid_enum_value, v}}
                end
            )
          end
        end

        def name_to_value(k) do
          case k do
            unquote(
              name_to_value_defs ++
                quote do
                  _ -> {:error, {:invalid_enum_name, k}}
                end
            )
          end
        end

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

        def member?(v) do
          case v do
            unquote(
              member_defs ++
                quote do
                  _ -> false
                end
            )
          end
        end

        def name?(k) do
          case k do
            unquote(
              name_member_defs ++
                quote do
                  _ -> false
                end
            )
          end
        end
      end
    end
  end

  defp to_name(key) do
    key
    |> to_string()
    |> String.downcase()
    |> String.to_atom()
  end
end
