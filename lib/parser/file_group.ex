defmodule Thrift.Parser.FileGroup do
  @moduledoc """
  Represents a group of parsed files. When you parse a file, it might include other thrift files.
  These files are in turn accumulated and parsed and added to this module.
  Additionally, this module allows resolution of the names of Structs / Enums / Unions etc across
  files.
  """
  alias Thrift.Parser.{
    FileGroup,
    FileRef,
    Resolver,
    ParsedFile
  }

  alias Thrift.Parser.Models.{
    Field,
    Namespace,
    StructRef,
    Schema,
    Struct,
  }

  @type t :: %FileGroup{
    resolver: pid,
    initial_file: Path.t,
    parsed_files: %{FileRef.thrift_include => %ParsedFile{}},
    schemas: %{FileRef.thrift_include => %Schema{}},
    ns_mappings: %{atom => %Namespace{}}
  }

  defstruct resolver: nil, initial_file: nil, parsed_files: %{}, schemas: %{}, resolutions: %{}, ns_mappings: %{}

  def new(initial_file) do
    {:ok, resolver} = Resolver.start_link()
    %FileGroup{initial_file: initial_file, resolver: resolver}
  end

  def add(file_group, parsed_file) do
    file_group = add_includes(file_group, parsed_file)
    new_parsed_files = Map.put(file_group.parsed_files, parsed_file.name, parsed_file)
    new_schemas = Map.put(file_group.schemas, parsed_file.name, parsed_file.schema)

    Resolver.add(file_group.resolver, parsed_file)
    %__MODULE__{file_group |
                parsed_files: new_parsed_files,
                schemas: new_schemas}
  end

  def add_includes(%__MODULE__{} = group,
                   %ParsedFile{schema: schema, file_ref: file_ref}) do

    Enum.reduce(schema.includes, group, fn(include, file_group) ->
      parsed_file = file_ref.path
      |> Path.dirname
      |> Path.join(include.path)
      |> FileRef.new
      |> ParsedFile.new
      add(file_group, parsed_file)
    end)
  end

  def update_resolutions(file_group) do
    # since in a file, we can refer to things defined in that file in a non-qualified
    # way, we add unqualified names to the resolutions map.
    resolutions = Resolver.get(file_group.resolver)
    to_update = resolutions
    |> Enum.map(fn {name, v}=kvp ->
      case String.split(Atom.to_string(name), ".") do
        [_initial_module, rest] ->
          {:"#{rest}", v}
        _ ->
          kvp
      end
    end)
    |> Map.new

    resolutions = Map.merge(resolutions, to_update)
    ns_mappings = build_ns_mappings(file_group.schemas)
    Resolver.stop(file_group.resolver)

    %FileGroup{file_group |
               resolutions: resolutions,
               ns_mappings: ns_mappings}
  end

  def resolve(%FileGroup{} = group, %Field{type: %StructRef{} = ref} = field) do
    %Field{field | type: resolve(group, ref)}
  end

  def resolve(%FileGroup{resolutions: resolutions}, %StructRef{referenced_type: type_name}) do
    resolutions[type_name]
  end

  def resolve(%FileGroup{resolutions: resolutions}, path) when is_atom(path) do
    resolutions[path]
  end

  def resolve(_, other) do
    other
  end


  def dest_module(file_group, %Struct{name: name}) do
    dest_module(file_group, name)
  end

  def dest_module(file_group, name) do
    [thrift_module, struct_name] = name
    |> Atom.to_string
    |> String.split(".")
    |> Enum.map(&String.to_atom/1)

    case file_group.ns_mappings[thrift_module] do
      nil ->
        Module.concat(Elixir, struct_name)
      namespace = %Namespace{} ->
        namespace.path
        |> String.split(".")
        |> Enum.map(&Macro.camelize/1)
        |> Enum.join(".")
        |> String.to_atom
        |> Module.concat(struct_name)
    end
  end

  defp build_ns_mappings(schemas) do
    schemas
    |> Enum.map(fn {module_name, %Schema{namespaces: ns}} ->
      {String.to_atom(module_name), ns[:elixir]}
    end)
    |> Map.new
  end
end
