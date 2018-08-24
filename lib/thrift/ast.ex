defmodule Thrift.AST do
  @moduledoc """
  Thrift Abstract Syntax Tree

  `Thrift.Parser` returns a tree of these structures, starting with a
  `Thrift.AST.Schema` node at the root.

  ## Headers

  - `Thrift.AST.Include`
  - `Thrift.AST.CppInclude`
  - `Thrift.AST.Namespace`

  ## Definitions

  - `Thrift.AST.Constant`
  - `Thrift.AST.TEnum`

  Each struct has a `line` field that indicates the source line number on
  which it was defined. Many structs also have an `annotations` field which
  contains the map of annotations associated with it.
  """

  import Thrift.Parser.Conversions
  alias Thrift.Parser.{FileGroup, Literals, Types}

  defmodule Namespace do
    @moduledoc """
    A namespace specifies the target module, package, etc. for a document's
    type definitions.

    The namespace's scope indicates the target language; a `*` indicates that
    the namespace applies to all languages. A document can specify multiple
    namespaces, each with its own language scope.
    """

    @type t :: %Namespace{line: Thrift.Parser.line(), scope: atom, value: String.t()}

    @enforce_keys [:scope, :value]
    defstruct line: nil, scope: nil, value: nil

    @spec new(charlist, charlist) :: t
    def new(scope, value) do
      %Namespace{scope: atomify(scope), value: List.to_string(value)}
    end
  end

  defmodule Include do
    @moduledoc """
    An include makes another file's symbols available within this file (with a
    prefix).
    """

    @type t :: %Include{line: Thrift.Parser.line(), path: Path.t()}

    @enforce_keys [:path]
    defstruct line: nil, path: nil

    @spec new(charlist) :: t
    def new(path) do
      %Include{path: List.to_string(path)}
    end
  end

  defmodule CppInclude do
    @moduledoc """
    A C++ include adds a custom C++ include to the output C++ code.
    """

    @type t :: %CppInclude{line: Thrift.Parser.line(), path: Path.t()}

    @enforce_keys [:path]
    defstruct line: nil, path: nil

    @spec new(charlist) :: t
    def new(path) do
      %CppInclude{path: List.to_string(path)}
    end
  end

  defmodule Constant do
    @moduledoc """
    An constant value definition.
    """
    @type t :: %Constant{
            line: Thrift.Parser.line(),
            name: atom,
            value: Literals.t(),
            type: Types.t()
          }

    @enforce_keys [:name, :value, :type]
    defstruct line: nil, name: nil, value: nil, type: nil

    @spec new(charlist, Literals.t(), Types.t()) :: t
    def new(name, value, type) do
      %Constant{name: atomify(name), value: cast(type, value), type: type}
    end
  end

  defmodule TEnum do
    @moduledoc """
    An enumerated type with named values.
    """
    @type value :: integer()
    @type t :: %TEnum{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            name: atom,
            values: [{atom, value}, ...]
          }

    @enforce_keys [:name, :values]
    defstruct line: nil, annotations: %{}, name: nil, values: []

    @spec new(charlist, %{required(charlist) => value}) :: t
    def new(name, values) do
      {_, values} =
        Enum.reduce(values, {0, []}, fn
          {name, value}, {_index, acc} ->
            {value + 1, [{atomify(name), value} | acc]}

          name, {index, acc} ->
            {index + 1, [{atomify(name), index} | acc]}
        end)

      %TEnum{name: atomify(name), values: Enum.reverse(values)}
    end
  end

  defmodule Field do
    @moduledoc false
    @type printable :: String.t() | atom
    @type t :: %Field{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            id: integer,
            name: atom,
            type: Types.t(),
            required: boolean,
            default: Literals.t()
          }

    @enforce_keys [:id, :name, :type]
    defstruct line: nil,
              annotations: %{},
              id: nil,
              name: nil,
              type: nil,
              required: :default,
              default: nil

    @spec new(integer, boolean, Types.t(), charlist, Literals.t()) :: t
    def new(id, required, type, name, default) do
      %Field{
        id: id,
        type: type,
        name: atomic_snake(name),
        required: required,
        default: cast(type, default)
      }
    end

    @spec build_field_list(printable, [Field.t()]) :: [Field.t()]
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
      dupes =
        fields
        |> Enum.group_by(& &1.id)
        |> Enum.filter(fn {_, v} -> length(v) > 1 end)

      unless Enum.empty?(dupes) do
        {id, dupe_fields} = List.first(dupes)

        names =
          dupe_fields
          |> Enum.map(&"#{name}.#{&1.name}")
          |> Enum.sort()
          |> Enum.join(", ")

        raise "Error: #{names} share field number #{id}."
      end

      fields
    end
  end

  defmodule Exception do
    @moduledoc false
    @type t :: %Exception{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            name: atom,
            fields: [Field.t()]
          }

    @enforce_keys [:name, :fields]
    defstruct line: nil, annotations: %{}, fields: %{}, name: nil

    @spec new(charlist, [Field.t(), ...]) :: t
    def new(name, fields) do
      ex_name = atomify(name)
      updated_fields = Field.build_field_list(ex_name, fields)

      %Exception{name: ex_name, fields: updated_fields}
    end
  end

  defmodule Struct do
    @moduledoc false
    @type t :: %Struct{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            name: atom,
            fields: [Field.t()]
          }

    @enforce_keys [:name, :fields]
    defstruct line: nil, annotations: %{}, name: nil, fields: %{}

    @spec new(charlist, [Field.t(), ...]) :: t
    def new(name, fields) do
      struct_name = atomify(name)
      fields = Field.build_field_list(struct_name, fields)

      %Struct{name: struct_name, fields: fields}
    end
  end

  defmodule Union do
    @moduledoc false
    @type t :: %Union{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            name: atom,
            fields: [Field.t()]
          }

    @enforce_keys [:name, :fields]
    defstruct line: nil, annotations: %{}, name: nil, fields: %{}

    @spec new(charlist, [Field.t(), ...]) :: t
    def new(name, fields) do
      name = atomify(name)

      fields =
        name
        |> Field.build_field_list(fields)
        |> Enum.map(fn %Field{} = field ->
          # According to Thrift docs, unions have implicitly optional
          # fields. See https://thrift.apache.org/docs/idl#union
          %Field{field | required: false}
        end)

      %Union{name: name, fields: fields}
    end

    def validator(%Union{}, var_name) do
      union_var = Macro.var(var_name, nil)

      quote do
        set_fields =
          unquote(union_var)
          |> Map.delete(:__struct__)
          |> Enum.reject(fn {_, val} -> is_nil(val) end)

        case set_fields do
          [] ->
            :ok

          [_] ->
            :ok

          set_fields ->
            field_names = Enum.map(set_fields, &elem(&1, 0))

            raise %Thrift.Union.TooManyFieldsSetError{
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
    @moduledoc false
    @type t :: %TypeRef{line: Thrift.Parser.line(), referenced_type: atom}

    @enforce_keys [:referenced_type]
    defstruct line: nil, referenced_type: nil

    @spec new(charlist) :: t
    def new(referenced_type) do
      %TypeRef{referenced_type: atomify(referenced_type)}
    end
  end

  defmodule ValueRef do
    @moduledoc false
    @type t :: %ValueRef{line: Thrift.Parser.line(), referenced_value: atom}

    @enforce_keys [:referenced_value]
    defstruct line: nil, referenced_value: nil

    @spec new(charlist) :: t
    def new(referenced_value) do
      %ValueRef{referenced_value: atomify(referenced_value)}
    end
  end

  defmodule Function do
    @moduledoc false
    @type return :: :void | Types.t()
    @type t :: %Function{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            oneway: boolean,
            return_type: return,
            name: atom,
            params: [Field.t()],
            exceptions: [Exception.t()]
          }

    @enforce_keys [:name]
    defstruct line: nil,
              annotations: %{},
              oneway: false,
              return_type: :void,
              name: nil,
              params: [],
              exceptions: []

    @spec new(boolean, Types.t(), charlist, [Field.t(), ...], [Exception.t(), ...]) :: t
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
    @moduledoc false
    @type t :: %Service{
            line: Thrift.Parser.line(),
            annotations: Thrift.Parser.annotations(),
            name: atom,
            extends: atom,
            functions: %{atom => Function.t()}
          }

    @enforce_keys [:name, :functions]
    defstruct line: nil, annotations: %{}, name: nil, extends: nil, functions: %{}

    @spec new(charlist, [Function.t(), ...], charlist) :: t
    def new(name, functions, extends) do
      fn_map = Enum.into(functions, %{}, fn f -> {f.name, f} end)
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

    @type header :: Include.t() | Namespace.t()
    @type typedef :: {:typedef, Types.t(), atom}
    @type definition ::
            Service.t()
            | TEnum.t()
            | Exception.t()
            | Union.t()
            | Struct.t()
            | Constant.t()
            | typedef
    @type t :: %Schema{
            path: Path.t() | nil,
            module: String.t(),
            namespaces: %{String.t() => Namespace.t()},
            structs: %{String.t() => Struct.t()},
            services: %{String.t() => Service.t()},
            enums: %{String.t() => TEnum.t()},
            unions: %{String.t() => Union.t()},
            includes: [Include.t()],
            cpp_includes: [CppInclude.t()],
            constants: %{String.t() => Literals.t()},
            exceptions: %{String.t() => Exception.t()},
            typedefs: %{String.t() => Types.t()},
            file_group: FileGroup.t()
          }
    defstruct path: nil,
              module: nil,
              namespaces: %{},
              structs: %{},
              services: %{},
              enums: %{},
              unions: %{},
              includes: [],
              cpp_includes: [],
              constants: %{},
              exceptions: %{},
              typedefs: %{},
              file_group: nil

    @doc """
    Constructs a schema from header and definition lists.
    """
    @spec new(Path.t() | nil, [header], [definition]) :: t
    def new(path, headers, defs) do
      schema = %Schema{path: path, module: module_name(path)}

      (headers ++ defs)
      |> Enum.reverse()
      |> Enum.reduce(schema, &merge(&2, &1))
    end

    defp module_name(nil), do: nil

    defp module_name(path) when is_bitstring(path) do
      path
      |> Path.basename()
      |> Path.rootname()
      |> String.to_atom()
    end

    @spec merge(t, header | definition) :: t
    defp merge(schema, %Include{} = inc) do
      %Schema{schema | includes: [inc | schema.includes]}
    end

    defp merge(schema, %CppInclude{} = cpp_inc) do
      %Schema{schema | cpp_includes: [cpp_inc | schema.cpp_includes]}
    end

    defp merge(schema, %Namespace{} = ns) do
      %Schema{schema | namespaces: Map.put(schema.namespaces, ns.scope, ns)}
    end

    defp merge(schema, %Constant{} = const) do
      %Schema{schema | constants: put_new_strict(schema.constants, const.name, const)}
    end

    defp merge(schema, %TEnum{} = enum) do
      %Schema{
        schema
        | enums:
            put_new_strict(schema.enums, enum.name, add_namespace_to_name(schema.module, enum))
      }
    end

    defp merge(schema, %Exception{} = exc) do
      fixed_fields =
        schema.module
        |> add_namespace_to_name(exc)
        |> add_namespace_to_fields()

      %Schema{schema | exceptions: put_new_strict(schema.exceptions, exc.name, fixed_fields)}
    end

    defp merge(schema, %Struct{} = s) do
      fixed_fields =
        schema.module
        |> add_namespace_to_name(s)
        |> add_namespace_to_fields()

      %Schema{schema | structs: put_new_strict(schema.structs, s.name, fixed_fields)}
    end

    defp merge(schema, %Union{} = union) do
      fixed_fields =
        schema.module
        |> add_namespace_to_name(union)
        |> add_namespace_to_fields()

      %Schema{schema | unions: put_new_strict(schema.unions, union.name, fixed_fields)}
    end

    defp merge(schema, %Service{} = service) do
      %Schema{
        schema
        | services:
            put_new_strict(
              schema.services,
              service.name,
              add_namespace_to_name(schema.module, service)
            )
      }
    end

    defp merge(schema, {:typedef, actual_type, type_alias}) do
      %Schema{
        schema
        | typedefs:
            put_new_strict(
              schema.typedefs,
              atomify(type_alias),
              add_namespace_to_type(schema.module, actual_type)
            )
      }
    end

    defp add_namespace_to_name(nil, model) do
      model
    end

    defp add_namespace_to_name(module, %{name: name} = model) do
      %{model | name: add_namespace_to_type(module, name)}
    end

    defp add_namespace_to_type(module, %TypeRef{referenced_type: t} = type) do
      %TypeRef{type | referenced_type: add_namespace_to_type(module, t)}
    end

    defp add_namespace_to_type(module, {:set, elem_type}) do
      {:set, add_namespace_to_type(module, elem_type)}
    end

    defp add_namespace_to_type(module, {:list, elem_type}) do
      {:list, add_namespace_to_type(module, elem_type)}
    end

    defp add_namespace_to_type(module, {:map, {key_type, val_type}}) do
      {:map, {add_namespace_to_type(module, key_type), add_namespace_to_type(module, val_type)}}
    end

    for type <- Thrift.primitive_names() do
      defp add_namespace_to_type(_, unquote(type)) do
        unquote(type)
      end
    end

    defp add_namespace_to_type(module, type_name) when is_atom(type_name) do
      split_type_name =
        type_name
        |> Atom.to_string()
        |> String.split(".")

      case split_type_name do
        [_module, _type | _rest] ->
          # this case accounts for types that already have a module in them
          type_name

        _ ->
          :"#{module}.#{type_name}"
      end
    end

    defp add_namespace_to_value(module, {:set, elem_type}) do
      {:set, add_namespace_to_value(module, elem_type)}
    end

    defp add_namespace_to_value(module, {:list, elem_type}) do
      {:list, add_namespace_to_value(module, elem_type)}
    end

    defp add_namespace_to_value(module, {:map, {key_type, val_type}}) do
      {:map, {add_namespace_to_value(module, key_type), add_namespace_to_value(module, val_type)}}
    end

    for type <- Thrift.primitive_names() do
      defp add_namespace_to_value(_, unquote(type)) do
        unquote(type)
      end
    end

    defp add_namespace_to_value(module, type_name) when is_atom(type_name) do
      split_type_name =
        type_name
        |> Atom.to_string()
        |> String.split(".")

      case split_type_name do
        [^module | _rest] ->
          # this case accounts for values that already have the current module in them
          type_name

        _ ->
          :"#{module}.#{type_name}"
      end
    end

    defp add_namespace_to_fields(%{fields: fields} = model) do
      %{model | fields: Enum.map(fields, &add_namespace_to_field/1)}
    end

    defp add_namespace_to_field(%Field{default: nil} = field) do
      field
    end

    defp add_namespace_to_field(%Field{default: default, type: type} = field) do
      %Field{field | default: add_namespace_to_defaults(type, default)}
    end

    defp add_namespace_to_defaults({:list, elem_type}, defaults) when is_list(defaults) do
      for elem <- defaults do
        add_namespace_to_defaults(elem_type, elem)
      end
    end

    defp add_namespace_to_defaults({:set, elem_type}, %MapSet{} = defaults) do
      for elem <- defaults, into: MapSet.new() do
        add_namespace_to_defaults(elem_type, elem)
      end
    end

    defp add_namespace_to_defaults({:map, {_, _}}, %ValueRef{} = val) do
      val
    end

    defp add_namespace_to_defaults({:map, {key_type, val_type}}, defaults)
         when is_map(defaults) do
      for {key, val} <- defaults, into: %{} do
        {add_namespace_to_defaults(key_type, key), add_namespace_to_defaults(val_type, val)}
      end
    end

    defp add_namespace_to_defaults(
           %TypeRef{referenced_type: referenced_type},
           %ValueRef{referenced_value: referenced_value} = val_ref
         ) do
      %ValueRef{val_ref | referenced_value: namespaced_value(referenced_type, referenced_value)}
    end

    defp add_namespace_to_defaults(%TypeRef{} = type, defaults) when is_list(defaults) do
      for default_value <- defaults do
        add_namespace_to_defaults(type, default_value)
      end
    end

    defp add_namespace_to_defaults(ref, {key_type, val_type}) do
      # this is used for a remote typedef that defines a map
      {add_namespace_to_defaults(ref, key_type), add_namespace_to_defaults(ref, val_type)}
    end

    defp add_namespace_to_defaults(_t, val) do
      val
    end

    defp namespaced_value(type, value) do
      with string_val <- Atom.to_string(type),
           [module, _value | _rest] <- String.split(string_val, ".") do
        add_namespace_to_value(module, value)
      else
        _ ->
          value
      end
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
end
