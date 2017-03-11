defmodule Thrift.Generator do
  @moduledoc """
  This module provides functions for generating Elixir source code from Thrift
  IDL files (`.thrift`).
  """

  alias Thrift.Parser.{
    FileGroup,
    Models.Schema,
    Models.Constant
  }
  alias Thrift.{
    Generator,
    Generator.ConstantGenerator,
    Generator.EnumGenerator,
    Generator.StructGenerator
  }

  @doc """
  Returns the list of target paths that would be generated from a Thrift file.
  """
  @spec targets(FileGroup.t) :: [Path.t]
  def targets(%FileGroup{} = file_group) do
    Enum.flat_map(file_group.schemas, fn {_, schema} ->
      schema
      |> Map.put(:file_group, file_group)
      |> generate_schema
      |> Enum.map(fn {name, _} -> target_path(name) end)
    end)
  end

  @spec target_path(String.t) :: Path.t
  defp target_path(module_name) do
    module_name
    |> inspect
    |> String.split(".")
    |> Enum.map(&Macro.underscore/1)
    |> Path.join
    |> Kernel.<>(".ex")
  end

  def generate!(thrift_filename, output_dir) when is_bitstring(thrift_filename) do
    thrift_filename
    |> Thrift.Parser.parse_file
    |> generate!(output_dir)
  end

  def generate!(%FileGroup{} = file_group, output_dir) do
    Enum.flat_map(file_group.schemas, fn {_, schema} ->
      schema
      |> Map.put(:file_group, file_group)
      |> generate_schema
      |> write_schema_to_file(output_dir)
    end)
  end

  def generate_to_string!(%FileGroup{} = file_group) do
    Enum.flat_map(file_group.schemas, fn {_, schema} ->
      %Schema{schema | file_group: file_group}
      |> generate_schema
    end)
    |> Enum.reverse
    |> Enum.map(fn {_, code} ->
      Macro.to_string(code)
    end)
    |> Enum.join("\n")
  end

  def generate_schema(schema) do
    current_module_file_group = FileGroup.set_current_module(schema.file_group, schema.module)
    schema = %Schema{schema | file_group: current_module_file_group}
    List.flatten([
      generate_enum_modules(schema),
      generate_const_modules(schema),
      generate_struct_modules(schema),
      generate_union_modules(schema),
      generate_exception_modules(schema),
      generate_services(schema),
      generate_behaviours(schema)
    ])
  end

  defp write_schema_to_file(generated_modules, output_dir) do
    generated_modules
    |> resolve_name_collisions
    |> Enum.map(fn {name, quoted} ->
      filename = target_path(name)
      source = Macro.to_string(quoted)

      path = Path.join(output_dir, filename)
      path |> Path.dirname |> File.mkdir_p!
      path |> File.write!(source)

      filename
    end)
  end

  defp resolve_name_collisions(generated_modules) do
    Enum.reduce(generated_modules, [], fn({name, quoted}, acc) ->
      Keyword.update(
        acc,
        name,
        quoted,
        &resolve_name_collision(name, &1, quoted)
      )
    end)
  end

  # We resolve name collisions (two generated modules with the same name)
  # pairwise by inspecting the types of modules generated.
  # Most collisions cannot be resolved.  Constants can be merged into
  # modules that define other types (structs, etc).
  defp resolve_name_collision(name, q1, q2) do
    # breaks apart the module's ast and gets the parts we need
    {meta1, ast1} = get_meta_and_ast(q1)
    {meta2, ast2} = get_meta_and_ast(q2)

    # the context will tell us what type (e.g., Enum, Constant, etc.)
    #   was defined by each module
    context1 = Keyword.get(meta1, :context)
    context2 = Keyword.get(meta2, :context)

    # only allow constants to be merged into other modules
    #   but make sure the meta is for the not-constant module, so that
    #   subsequent collisions are properly dealt with
    cond do
      context1 == Thrift.Generator.ConstantGenerator ->
        combine_module_defs(name, meta2, ast1, ast2)
      context2 == Thrift.Generator.ConstantGenerator ->
        combine_module_defs(name, meta1, ast1, ast2)
      true ->
        raise "Name collision: #{name}"
    end
  end

  defp get_meta_and_ast(quoted) do
    {:defmodule, meta, [_name, [do: {:__block__, [], ast}]]} = quoted
    {meta, ast}
  end

  defp combine_module_defs(name, meta, ast1, ast2) do
    {:defmodule, meta, [name, [do: {:__block__, [], ast1 ++ ast2}]]}
  end

  defp generate_enum_modules(schema) do
    for {_, enum} <- schema.enums do
      full_name = FileGroup.dest_module(schema.file_group, enum)
      {full_name, EnumGenerator.generate(full_name, enum)}
    end
  end

  defp generate_const_modules(%Schema{constants: constants})
  when constants == %{} do
    # no constants => nothing to generate
    []
  end
  defp generate_const_modules(schema) do
    # schema.constants is a map %{name: constant} but constant includes the
    # name and all we really need is the values
    #
    # we also only want constants that are defined in the main file from this
    # file group
    constants = schema.constants
    |> Map.values
    |> Enum.filter(&FileGroup.own_constant?(schema.file_group, &1))

    if Enum.empty?(constants) do
      # if we filtered out all of the constants, we don't need to write
      # anything
      []
    else
      # name of the generated module
      full_name = FileGroup.dest_module(schema.file_group, Constant)
      [{full_name, ConstantGenerator.generate(full_name, constants, schema)}]
    end
  end

  defp generate_struct_modules(schema) do
    for {_, struct} <- schema.structs do
      full_name = FileGroup.dest_module(schema.file_group, struct)
      {full_name, StructGenerator.generate(:struct, schema, full_name, struct)}
    end
  end

  defp generate_union_modules(schema) do
    for {_, union} <- schema.unions do
      full_name = FileGroup.dest_module(schema.file_group, union)
      {full_name, StructGenerator.generate(:union, schema, full_name, union)}
    end
  end

  defp generate_exception_modules(schema) do
    for {_, exception} <- schema.exceptions do
      full_name = FileGroup.dest_module(schema.file_group, exception)
      {full_name, StructGenerator.generate(:exception, schema, full_name, exception)}
    end
  end

  defp generate_services(schema) do
    for {_, service} <- schema.services do
      Generator.Service.generate(schema, service)
    end
  end

  defp generate_behaviours(schema) do
    for {_, service} <- schema.services do
      Generator.Behaviour.generate(schema, service)
    end
  end

end
