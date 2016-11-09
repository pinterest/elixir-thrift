defmodule Thrift.Parser.FileGroup do
  alias Thrift.Parser.{
    FileGroup,
    FileRef,
    Resolver,
    ParsedFile
  }

  alias Thrift.Parser.Models.{
    Field,
    StructRef,
    Schema,
  }

  defdelegate fix_refs(), to: Resolver

  @type t :: %FileGroup{
    parsed_files: %{FileRef.thrift_include => %ParsedFile{}},
    schemas: %{FileRef.thrift_include => %Schema{}}}

  defstruct parsed_files: %{}, schemas: %{}, resolutions: %{}

  def add(file_group, parsed_file) do
    file_group = add_includes(file_group, parsed_file)
    new_parsed_files = Map.put(file_group.parsed_files, parsed_file.name, parsed_file)
    new_schemas = Map.put(file_group.schemas, parsed_file.name, parsed_file.schema)

    Resolver.add(parsed_file)
    %__MODULE__{file_group |
                parsed_files: new_parsed_files,
                schemas: new_schemas}
  end

  def add_includes(group=%__MODULE__{},
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

  def resolve(%FileGroup{}=group, %Field{type: %StructRef{}=ref}=field) do
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

end
