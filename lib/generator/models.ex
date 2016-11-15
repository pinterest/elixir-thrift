defmodule Thrift.Generator.Models do
  alias Thrift.Protocols.Binary
  alias Thrift.Parser.FileGroup

  def generate!(thrift_filename, output_dir) when is_bitstring(thrift_filename) do
    thrift_filename
    |> Thrift.Parser.parse_file
    |> generate!(output_dir)
  end

  def generate!(%FileGroup{}=file_group, output_dir) do
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
    for {name, enum} <- schema.enums do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_enum(full_name, enum)}
    end
  end

  defp generate_enum(name, enum) do
    parts = Enum.map(enum.values, fn {key, value} ->
      macro_name = key |> to_string |> String.downcase |> String.to_atom
      quote do
        defmacro unquote(Macro.var(macro_name, nil)), do: unquote(value)
      end
    end)
    quote do
      defmodule unquote(name) do
        @moduledoc unquote("Auto-generated Thrift enum #{enum.name}")
        unquote_splicing(parts)
      end
    end
  end

  defp generate_structs(schema) do
    for {name, struct} <- schema.structs do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_struct(schema, full_name, struct)}
    end
  end

  defp generate_struct(schema, name, struct) do
    struct_parts = Enum.map(struct.fields, fn
      %{name: name, default: nil, type: type} ->
        {name, zero(schema, type)}
      %{name: name, default: default} when not is_nil(default) ->
        {name, default}
    end)
    binary_serializer = Binary.generate_serializer(name, schema.file_group, struct)
    quote do
      defmodule unquote(name) do
        _ = unquote "Auto-generated Thrift struct #{struct.name}"
        unquote_splicing(for field <- struct.fields do
          quote do
            _ = unquote "#{field.name} #{inspect field.type}"
          end
        end)
        defstruct unquote(struct_parts)
        def new, do: %__MODULE__{}
        unquote(binary_serializer)
      end
    end
  end

  defp generate_exceptions(schema) do
    for {name, struct} <- schema.exceptions do
      full_name = namespace(schema) |> Module.concat(name)
      {full_name, generate_exception(schema, full_name, struct)}
    end
  end

  defp generate_exception(schema, name, struct) do
    struct_parts = Enum.map(struct.fields, fn
      %{name: name, default: nil, type: type} ->
        {name, zero(schema, type)}
      %{name: name, default: default} when not is_nil(default) ->
        {name, default}
    end)
    binary_serializer = Binary.generate_serializer(name, schema.file_group, struct)
    quote do
      defmodule unquote(name) do
        _ = unquote "Auto-generated Thrift exception #{struct.name}"
        unquote_splicing(for field <- struct.fields do
          quote do
            _ = unquote "#{field.name} #{inspect field.type}"
          end
        end)
        defstruct unquote(struct_parts)
        def new, do: %__MODULE__{}
        unquote(binary_serializer)
      end
    end
  end

  # Zero values for built-in types
  defp zero(_schema, :bool), do: false
  defp zero(_schema, :byte), do: 0
  defp zero(_schema, :i8), do: 0
  defp zero(_schema, :i16), do: 0
  defp zero(_schema, :i32), do: 0
  defp zero(_schema, :i64), do: 0
  defp zero(_schema, :double), do: 0.0
  defp zero(_schema, :string), do: ""
  defp zero(_schema, :binary), do: ""
  defp zero(_schema, {:map, _}), do: %{}
  defp zero(_schema, {:list, _}), do: []
  defp zero(_schema, {:set, _}), do: quote do: MapSet.new
  defp zero(_schema, %{values: [{_, value} | _]}), do: value

  # Zero values for user defined types
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
