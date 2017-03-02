defmodule Thrift.Parser.Models do
  @moduledoc """
  Models used by the Thrift parser that represent different Thrift components.
  The models defined here are returned by the parse functions in the
  `Thrift.Parser` module.
  """

  import Thrift.Parser.Conversions
  alias Thrift.Parser.{Literals, Types}

  @type line :: nil | pos_integer

  defmodule Namespace do
    @moduledoc """
    A Thrift namespace.
    The namespace is a language-specific place where the generated structs are
    placed.
    """

    @type t :: %Namespace{line: Parser.line, name: atom, path: String.t}

    @enforce_keys [:name, :path]
    defstruct line: nil, name: nil, path: nil

    @spec new(charlist, charlist) :: t
    def new(name, path) do
      %Namespace{name: atomify(name), path: List.to_string(path)}
    end
  end

  defmodule Include do
    @moduledoc """
    An included file.
    In Thrift, you can include other files to share structs, enums and the like.
    """

    @type t :: %Include{line: Parser.line, path: String.t}

    @enforce_keys [:path]
    defstruct line: nil, path: nil

    @spec new(charlist) :: t
    def new(path) do
      %Include{path: List.to_string(path)}
    end
  end

  defmodule Constant do
    @moduledoc """
    A Thrift constant.
    Constants of any primitive or container type can be created in Thrift.
    """

    @type t :: %Constant{line: Parser.line, name: atom, value: Literals.t, type: Types.t}

    @enforce_keys [:name, :value, :type]
    defstruct line: nil, name: nil, value: nil, type: nil

    @spec new(charlist, Literals.t, Types.t) :: t
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
    @type t :: %TEnum{line: Parser.line, name: atom, values: [{atom, enum_value}]}

    @enforce_keys [:name, :values]
    defstruct line: nil, name: nil, values: []

    @spec new(charlist, %{charlist => enum_value}) :: t
    def new(name, values) do
      values = values
      |> Enum.with_index
      |> Enum.map(fn
        {{name, value}, _index} ->
          {atomify(name), value}

        {name, index} ->
          {atomify(name), index + 1}
      end)

      %TEnum{name: atomify(name), values: values}
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
    @type t :: %Field{line: Parser.line, id: integer, name: atom, type: Types.t,
                      required: boolean, default: Literals.t}

    @enforce_keys [:id, :name, :type]
    defstruct line: nil, id: nil, name: nil, type: nil, required: :default, default: nil

    @spec new(integer, boolean, Types.t, charlist, Literals.t) :: t
    def new(id, required, type, name, default) do
      %Field{id: id,
             type: type,
             name: atomic_snake(name),
             required: required,
             default: cast(type, default)}
    end

    @spec build_field_list(printable, [Field.t]) :: [Field.t]
    def build_field_list(parent_name, fields) do
      fields
      |> assign_missing_ids
      |> validate_ids(parent_name)
    end

    # Fields without explicit indices are automatically assigned starting from
    # -1 and working their way down. Implicit field indices were deprecated by
    # Apache Thrift, but we support them for greater compatibility.
    defp assign_missing_ids(fields, auto_index \\ -1)
    defp assign_missing_ids([%Field{id: nil} = field | fields], auto_index) do
      [%Field{field | id: auto_index} | assign_missing_ids(fields, auto_index - 1)]
    end
    defp assign_missing_ids([field | fields], auto_index) do
      [field | assign_missing_ids(fields, auto_index)]
    end
    defp assign_missing_ids([], _), do: []

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
  end

  defmodule Exception do
    @moduledoc """
    A Thrift exception

    Exceptions can happen when the remote service encounters an error.
    """

    @type t :: %Exception{line: Parser.line, name: atom, fields: [Field.t]}

    @enforce_keys [:name, :fields]
    defstruct line: nil, fields: %{}, name: nil

    @spec new(charlist, [Field.t, ...]) :: t
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

    @type t :: %Struct{line: Parser.line, name: atom, fields: [Field.t]}

    @enforce_keys [:name, :fields]
    defstruct line: nil, name: nil, fields: %{}

    @spec new(charlist, [Field.t, ...]) :: t
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

    @type t :: %Union{line: Parser.line, name: atom, fields: [Field.t]}

    @enforce_keys [:name, :fields]
    defstruct line: nil, name: nil, fields: %{}

    @spec new(charlist, [Field.t, ...]) :: t
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

  defmodule TypeRef do
    @moduledoc """
    A reference to another type.

    While not a Thrift type, this represents when a Thrift type refers to
    another.
    """

    @type t :: %TypeRef{line: Parser.line, referenced_type: atom}

    @enforce_keys [:referenced_type]
    defstruct line: nil, referenced_type: nil

    @spec new(charlist) :: t
    def new(referenced_type) do
      %TypeRef{referenced_type: atomify(referenced_type)}
    end
  end

  defmodule ValueRef do
    @moduledoc """
    A reference to another value, such as an enum or const.
    """

    @type t :: %ValueRef{line: Parser.line, referenced_value: atom}

    @enforce_keys [:referenced_value]
    defstruct line: nil, referenced_value: nil

    @spec new(charlist) :: t
    def new(referenced_value) do
      %ValueRef{referenced_value: atomify(referenced_value)}
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
    @type t :: %Function{line: Parser.line, oneway: boolean, return_type: return, name: atom,
                         params: [Field.t], exceptions: [Exception.t]}

    @enforce_keys [:name]
    defstruct line: nil, oneway: false, return_type: :void, name: nil, params: [], exceptions: []

    @spec new(boolean, Types.t, charlist, [Field.t, ...], [Exception.t, ...]) :: t
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

    @type t :: %Service{line: Parser.line, name: atom, extends: atom, functions: %{atom => Function.t}}

    @enforce_keys [:name, :functions]
    defstruct line: nil, name: nil, extends: nil, functions: %{}

    @spec new(charlist, [Function.t, ...], charlist) :: t
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

    @type header :: Include.t | Namespace.t
    @type typedef :: {:typedef, Types.t, atom}
    @type definition :: Service.t | TEnum.t | Exception.t | Union.t | Struct.t | Constant.t | typedef
    @type model :: header | definition
    @type t :: %Schema{
      absolute_path: Path.t,
      module: String.t,
      thrift_namespace: String.t,
      namespaces: %{String.t => Namespace.t},
      structs: %{String.t => Struct.t},
      services: %{String.t => Service.t},
      enums: %{String.t => TEnum.t},
      unions: %{String.t => Union.t},
      includes: [Include.t],
      constants: %{String.t => Literals.t},
      exceptions: %{String.t => Exception.t},
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
      %Schema{schema | constants: put_new_strict(schema.constants, const.name, const)}
    end

    defp merge(schema, %TEnum{} = enum) do
      %Schema{schema | enums: put_new_strict(schema.enums, enum.name, canonicalize_name(schema, enum))}
    end

    defp merge(schema, %Exception{} = exc) do
      %Schema{schema | exceptions: put_new_strict(schema.exceptions, exc.name, canonicalize_name(schema, exc))}
    end

    defp merge(schema, %Struct{} = s) do
      %Schema{schema | structs: put_new_strict(schema.structs, s.name, canonicalize_name(schema, s))}
    end

    defp merge(schema, %Union{} = union) do
      %Schema{schema | unions: put_new_strict(schema.unions, union.name, canonicalize_name(schema, union))}
    end

    defp merge(schema, %Service{} = service) do
      %Schema{schema | services: put_new_strict(schema.services, service.name, canonicalize_name(schema, service))}
    end

    defp merge(schema, {:typedef, actual_type, type_alias}) do
      %Schema{schema | typedefs: put_new_strict(schema.typedefs, atomify(type_alias), actual_type)}
    end

    defp canonicalize_name(%{module: nil}, model) do
      model
    end

    defp canonicalize_name(schema, %{name: name} = model) do
      %{model | name: :"#{schema.module}.#{name}"}
    end

    defp put_new_strict(map, key, value) do
      case map[key] do
        nil ->
          Map.put(map, key, value)
        _ ->
          raise "Name collision: #{key}"
      end
    end
  end

  @type all :: Namespace.t | Include.t | Constant.t | TEnum.t | Field.t | Exception.t | Struct.t | Union.t | Function.t | Service.t | Schema.t
end
