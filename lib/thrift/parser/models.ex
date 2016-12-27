defmodule Thrift.Parser.Models do
  @moduledoc """
  Models used by the Thrift parser that represent different Thrift components.
  The models defined here are returned by the parse functions in the
  `Thrift.Parser` module.
  """

  alias Thrift.Parser.{Literals, Types}

  defmodule Namespace do
    @moduledoc """
    A Thrift namespace.
    The namespace is a language-specific place where the generated structs are
    placed.
    """

    @type t :: %Namespace{name: String.t, path: String.t}
    defstruct name: nil, path: nil

    import Thrift.Parser.Conversions

    @spec new(char_list, char_list) :: %Namespace{}
    def new(name, path) do
      %Namespace{name: atomify(name), path: List.to_string(path)}
    end
  end

  defmodule Include do
    @moduledoc """
    An included file.
    In Thrift, you can include other files to share structs, enums and the like.
    """

    @type t :: %Include{path: String.t}
    defstruct path: nil

    import Thrift.Parser.Conversions

    @spec new(char_list) :: %Include{}
    def new(path) do
      %Include{path: List.to_string(path)}
    end
  end

  defmodule Constant do
    @moduledoc """
    A Thrift constant.
    Constants of any primitive or container type can be created in Thrift.
    """

    @type t :: %Constant{name: String.t, value: Literal.t, type: Types.t}
    defstruct name: nil, value: nil, type: nil

    import Thrift.Parser.Conversions

    @spec new(char_list, Literals.t, Types.t) :: %Constant{}
    def new(name, val, type) do
      %Constant{name: atomify(name), value: cast(type, val), type: type}
    end
  end

  defmodule TEnum do
    @moduledoc """
    A Thrift enumeration
    An enumeration contains names and (usually sequential) values, and
    allows you to map from one to the other.
    """

    @type enum_value :: bitstring | integer
    @type t :: %TEnum{name: String.t, values: %{String.t => enum_value}}
    defstruct name: nil, values: []

    import Thrift.Parser.Conversions

    @spec new(char_list, %{char_list => enum_value}) :: %TEnum{}
    def new(name, values) do
      values = values
      |> Enum.with_index
      |> Enum.map(fn
        {{name, value}, _index} ->
          {atomify(name),  value}

        {name, index} ->
          {atomify(name), index + 1}
      end)

      %TEnum{name: atomify(name), values: values}
    end
  end

  defmodule TEnumValue do
    @moduledoc """
    A reference to an enum value
    For example, in a constant or default value.

       const string DEFAULT_WEATHER = Weather.SUNNY;
   """
    @type t :: %TEnumValue{enum_name: atom, enum_value: atom, type: atom}
    defstruct enum_name: nil, enum_value: nil, type: nil

    import Thrift.Parser.Conversions

    @spec new(char_list) :: %TEnumValue{}
    def new(enum_value) do
      [enum_name, enum_value] = enum_value
      |> List.to_string
      |> String.split(".")
      |> Enum.map(&String.to_atom/1)

      %TEnumValue{enum_name: enum_name, enum_value: enum_value}
    end
  end

  defmodule Field do
    @moduledoc """
    A Thrift field.

    Fields define a named type and can occur in functions, structs, unions,
    exceptions and the parameter list and `throws` clauses of functions.

    Fields can refer to each other. These are represented by the FieldReference
    type.

    This module also contains some utilities for validating and fixing up fields.
    """

    @type printable :: String.t | atom
    @type t :: %Field{id: integer, name: String.t, type: Types.t,
                      required: boolean, default: Literals.t}
    defstruct id: nil, name: nil, type: nil, required: :default, default: nil

    import Thrift.Parser.Conversions

    @spec new(integer, boolean, Types.t, char_list, Literals.t) :: %Field{}
    def new(id, required, type, name, default) do
      %Field{id: id,
             type: type,
             name: atomify(name),
             required: required,
             default: cast(type, default)}
    end

    @spec build_field_list(printable, [%Field{}]) :: [%Field{}]
    def build_field_list(parent_name, fields) do
      fields
      |> update_ids(parent_name)
      |> validate_ids(parent_name)
    end

    defp validate_ids(fields, name) do
      dupes = fields
      |> Enum.group_by(&(&1.id))
      |> Enum.filter(fn {_, v} -> length(v) > 1 end)

      unless Enum.empty?(dupes) do
        {id, dupe_fields} = List.first(dupes)

        names = dupe_fields
        |> Enum.map(&("#{name}.#{&1.name}"))
        |> Enum.sort
        |> Enum.join(", ")

        raise "Error: #{names} share field number #{id}."
      end

      fields
    end

    defp update_ids(fields, parent_name) do
      alias Thrift.Parser.Shell
      fields
      |> Enum.with_index
      |> Enum.map(fn
        {%__MODULE__{} = field, idx} ->
          case field.id do
            nil ->
              Shell.warn "Warning: id not set for field '#{parent_name}.#{field.name}'."
              %__MODULE__{field | id: idx + 1}
            _ ->
              field
          end
      end)
    end
  end

  defmodule Exception do
    @moduledoc """
    A Thrift exception

    Exceptions can happen when the remote service encounters an error.
    """

    @type t :: %Exception{name: String.t, fields: [%Field{}]}
    defstruct fields: %{}, name: nil

    import Thrift.Parser.Conversions
    alias Thrift.Parser.Models.Field

    @spec new(char_list, [%Field{}, ...]) :: %Exception{}
    def new(name, fields) do
      ex_name = atomify(name)
      updated_fields = Field.build_field_list(ex_name, fields)

      %Exception{name: ex_name, fields: updated_fields}
    end
  end

  defmodule Struct do
    @moduledoc """
    A Thrift struct

    The basic datastructure in Thrift, structs have aa name and a field list.
    """

    @type t :: %Struct{name: String.t, fields: %{String.t => %Field{}}}
    defstruct name: nil, fields: %{}

    import Thrift.Parser.Conversions
    alias Thrift.Parser.Models.Field

    @spec new(char_list, [%Field{}, ...]) :: %Struct{}
    def new(name, fields) do
      struct_name = atomify(name)
      fields = Field.build_field_list(struct_name, fields)

      %Struct{name: struct_name, fields: fields}
    end
  end

  defmodule Union do
    @moduledoc """
    A Thrift union

    Unions can have one field set at a time.
    """

    @type t :: %Union{name: String.t, fields: %{String.t => %Field{}}}
    defstruct name: nil, fields: %{}

    import Thrift.Parser.Conversions
    alias Thrift.Parser.Models.Field

    @spec new(char_list, [%Field{}, ...]) :: %Union{}
    def new(name, fields) do
      name = atomify(name)

      fields = name
      |> Field.build_field_list(fields)
      |> Enum.map(fn(%Field{} = field) ->
        # According to Thrift docs, unions have implicitly optional
        # fields. See https://thrift.apache.org/docs/idl#union
        %Field{field | required: false}
      end)

      %Union{name: name, fields: fields}
    end

    def validator(%Union{}, var_name) do
      union_var = Macro.var(var_name, nil)
      quote do
        set_fields = unquote(union_var)
        |> Map.delete(:__struct__)
        |> Enum.reject(fn {_, val} -> is_nil(val) end)
        case set_fields do
          [] ->
            :ok

          [_] ->
            :ok

          set_fields ->

            field_names = Enum.map(set_fields, &elem(&1, 0))
            raise %Thrift.Union.TooManyFieldsSetException{
              message: "Thrift union has more than one field set",
              set_fields: field_names
            }
        end
      end
    end
    def validator(_, var_name) do
      non_union_var = Macro.var(var_name, nil)
      quote do
        _ = unquote(non_union_var)
      end
    end
  end

  defmodule StructRef do
    @moduledoc """
    A reference to another struct.

    While not a Thrift type, this represents when a Thrift type refers to
    another.
    """

    @type t :: %StructRef{referenced_type: String.t}
    defstruct referenced_type: nil

    import Thrift.Parser.Conversions

    @spec new(char_list) :: %StructRef{}
    def new(referenced_type) do
      %StructRef{referenced_type: atomify(referenced_type)}
    end
  end

  defmodule Function do
    @moduledoc """
    A Thrift function

    Functions are remote endpoints for Thrift services. They contain an argument list, exceptions and return a typed object.
    They can also be `oneway`, which means that Thrift doesn't have to wait for
    a reply from them.
    """

    @type return :: :void | Types.t
    @type t :: %Function{oneway: boolean, return_type: return, name: String.t,
                         params: [%Field{}], exceptions: [%Exception{}]}
    defstruct oneway: false, return_type: :void, name: nil, params: [], exceptions: []
    alias Thrift.Parser.Models.Field
    import Thrift.Parser.Conversions

    @spec new(boolean, Types.t, char_list, [%Field{}, ...], [%Exception{}, ...]) :: %Function{}
    def new(oneway, return_type, name, params, exceptions) do
      name = atomify(name)
      params = Field.build_field_list(name, params)

      %Function{
        oneway: oneway,
        return_type: return_type,
        name: name,
        params: params,
        exceptions: exceptions
      }
    end
  end

  defmodule Service do
    @moduledoc """
    A Thrift service

    Services hold RPC functions and can extend other services.
    """

    @type t :: %Service{name: String.t, extends: String.t, functions: %{atom => %Function{}}}
    defstruct name: nil, extends: nil, functions: %{}

    import Thrift.Parser.Conversions

    @spec new(char_list, [%Function{}, ...], char_list) :: %Service{}
    def new(name, functions, extends) do
      fn_map = Enum.into(functions, %{}, fn(f) -> {f.name, f} end)
      %Service{name: atomify(name), extends: atomify(extends), functions: fn_map}
    end
  end

  defmodule Schema do
    @moduledoc """
    A Thrift schema.

    A program represents a single parsed file in Thrift.
    Many programs can be compiled together to build a Thrift service.

    This is the root datastructure that the parser emits after running.
    """

    @type header :: %Include{} | %Namespace{}
    @type typedef :: {:typedef, Types.t, atom}
    @type definition :: %Service{} | %TEnum{} | %Exception{} | %Union{} | %Struct{} | %Constant{} | typedef
    @type model :: header | definition
    @type t :: %Schema{
      absolute_path: Path.t,
      module: String.t,
      thrift_namespace: String.t,
      namespaces: %{String.t => %Namespace{}},
      structs: %{String.t => %Struct{}},
      services: %{String.t => %Service{}},
      enums: %{String.t => %TEnum{}},
      unions: %{String.t => %Union{}},
      includes: [%Include{}],
      constants: %{String.t => Literals.t},
      exceptions: %{String.t => %Exception{}},
      typedefs: %{String.t => Types.t}
    }
    defstruct absolute_path: nil,
    module: nil,
    thrift_namespace: nil,
    namespaces: %{},
    structs: %{},
    services: %{},
    enums: %{},
    unions: %{},
    includes: [],
    constants: %{},
    exceptions: %{},
    typedefs: %{}

    import Thrift.Parser.Conversions
    alias Thrift.Parser.Models.{Constant,
                                Exception,
                                Include,
                                Namespace,
                                Struct,
                                TEnum,
                                Union
                               }

    @doc """
    Constructs a schema with both headers and definitions.
    """
    @spec new(Path.t, [header], [definition]) :: t
    def new(file_absolute_path, headers, defs) do
      orig_schema = %Schema{absolute_path: file_absolute_path,
                            module: module_name(file_absolute_path)}

      schema = headers
      |> Enum.reverse
      |> Enum.reduce(orig_schema, &merge(&2, &1))

      defs
      |> Enum.reverse
      |> Enum.reduce(schema, &merge(&2, &1))
    end

    defp module_name(nil), do: nil

    defp module_name(path_name) when is_bitstring(path_name) do
      path_name
      |> Path.basename
      |> Path.rootname
      |> String.to_atom
    end

    @spec merge(t, model) :: t
    defp merge(schema, %Include{} = inc) do
      %Schema{schema | includes: [inc | schema.includes]}
    end

    defp merge(schema, %Namespace{} = ns) do
      %Schema{schema | namespaces: Map.put(schema.namespaces, ns.name, ns)}
    end

    defp merge(schema, %Constant{} = const) do
      %Schema{schema | constants: Map.put(schema.constants, const.name, const)}
    end

    defp merge(schema, %TEnum{} = enum) do
      %Schema{schema | enums: Map.put(schema.enums, enum.name, canonicalize_name(schema, enum))}
    end

    defp merge(schema, %Exception{} = exc) do
      %Schema{schema | exceptions: Map.put(schema.exceptions, exc.name, canonicalize_name(schema, exc))}
    end

    defp merge(schema, %Struct{} = s) do
      %Schema{schema | structs: Map.put(schema.structs, s.name, canonicalize_name(schema, s))}
    end

    defp merge(schema, %Union{} = union) do
      %Schema{schema | unions: Map.put(schema.unions, union.name, canonicalize_name(schema, union))}
    end

    defp merge(schema, %Service{} = service) do
      %Schema{schema | services: Map.put(schema.services, service.name, canonicalize_name(schema, service))}
    end

    defp merge(schema, {:typedef, actual_type, type_alias}) do
      %Schema{schema | typedefs: Map.put(schema.typedefs, atomify(type_alias), actual_type)}
    end

    defp canonicalize_name(%{module: nil}, model) do
      model
    end

    defp canonicalize_name(schema, %{name: name} = model) do
      %{model | name: :"#{schema.module}.#{name}"}
    end
  end

  @type all :: %Namespace{} | %Include{} | %Constant{} | %TEnum{} | %Field{} | %Exception{} | %Struct{} | %Union{} | %Function{} | %Service{} | %Schema{}
end
