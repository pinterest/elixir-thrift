defmodule Thrift.Generator.Models do
  def generate!(thrift_filename, output_dir) do
    file_group = thrift_filename
    |> Thrift.Parser.parse_file

    Enum.flat_map(file_group.schemas, fn {_, schema} ->
      schema
      |> Map.put(:file_group, file_group)
      |> generate_schema(output_dir)
    end)
  end

  def generate_schema(schema, output_dir) do
    List.flatten([
      generate_enums(schema),
      generate_structs(schema),
      generate_exceptions(schema),
    ])
    |> Enum.map(fn {name, quoted} ->
      filename = name
      |> inspect
      |> String.split(".")
      |> Enum.map(&Macro.underscore/1)
      |> Path.join
      |> Kernel.<>(".ex")

      source = Macro.to_string(quoted)

      path = Path.join(output_dir, filename)
      path |> Path.dirname |> File.mkdir_p!
      path |> File.write!(source)

      filename
    end)
  end

  defp namespace(%{namespaces: namespaces}) do
    case namespaces do
      %{elixir: %{path: path}} ->
        path |> String.split(".") |> Enum.map(&Macro.camelize/1) |> Enum.join(".")
      %{} ->
        nil
    end
  end

  defp generate_enums(schema) do
    for {name, %{values: values}} <- schema.enums do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_enum(full_name, values)}
    end
  end

  defp generate_enum(name, values) do
    parts = Enum.map(values, fn {key, value} ->
      macro_name = key |> to_string |> String.downcase |> String.to_atom
      quote do
        defmacro unquote(Macro.var(macro_name, nil)), do: unquote(value)
      end
    end)
    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{inspect name}")
        unquote_splicing(parts)
      end
    end
  end

  defp generate_structs(schema) do
    for {name, %{fields: fields}} <- schema.structs do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_struct(schema, full_name, fields)}
    end
  end

  defp generate_struct(schema, name, fields) do
    parts = Enum.map(fields, fn
      %{name: name, default: nil, type: type} ->
        {name, zero(schema, type)}
      %{name: name, default: default} when not is_nil(default) ->
        {name, default}
    end)
    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift struct #{inspect name}")
        defstruct unquote(parts)
      end
    end
  end

  defp generate_exceptions(schema) do
    for {name, %{fields: fields}} <- schema.exceptions do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_exception(schema, full_name, fields)}
    end
  end

  defp generate_exception(schema, name, fields) do
    parts = Enum.map(fields, fn
      %{name: name, default: nil, type: type} ->
        {name, zero(schema, type)}
      %{name: name, default: default} when not is_nil(default) ->
        {name, default}
    end)
    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift exception #{inspect name}")
        defstruct unquote(parts)
      end
    end
  end

  # Zero values for base types
  defp zero(_schema, :bool), do: false
  defp zero(_schema, :byte), do: 0
  defp zero(_schema, :i8), do: 0
  defp zero(_schema, :i16), do: 0
  defp zero(_schema, :i32), do: 0
  defp zero(_schema, :i64), do: 0
  defp zero(_schema, :double), do: 0.0
  defp zero(_schema, :string), do: ""
  defp zero(_schema, :binary), do: ""
  defp zero(_schema, :slist), do: ""

  defp zero(_schema, {:list, _}) do
    []
  end

  defp zero(_schema, %{values: [{_, value} | _]}) do
    value
  end

  # Zero values for referenced types
  defp zero(schema, %{referenced_type: type}=ref) do
    cond do
      # Local references
      Map.has_key?(schema.enums, type) ->
        zero(schema, schema.enums[type])
      Map.has_key?(schema.typedefs, type) ->
        zero(schema, schema.typedefs[type])
      Map.has_key?(schema.structs, type) ->
        model_name = namespace(schema) |> Module.concat(type)
        quote do: %unquote(model_name){}

      # Included references
      true ->
        case Thrift.Parser.FileGroup.resolve(schema.file_group, ref) do
          nil ->
            raise "Unknown type: #{inspect type}"
          thing ->
            zero(schema, thing)
        end
    end
  end
end
