defmodule Thrift.Parser.Types do
  @moduledoc """
  A container module for modules containing typespecs for Thrift files.
  """
  defmodule Primitive do
    @moduledoc """
    Typespec for Thrift primitives
    """
    @type t :: :bool | :i8 | :i16 | :i64 | :binary | :double | :byte | :string
  end

  defmodule Ident do
    @moduledoc """
    A Thrift identifier
    """
    @type t :: String.t
  end

  defmodule Standalone do
    @moduledoc """
    A Thrift type that isn't a container
    """
    @type t :: Ident.t | Primitive.t
  end

  defmodule List do
    @moduledoc """
    A Thrift list.
    """
    @type t :: {:list, Thrift.Parser.Types.t}
  end

  defmodule Map do
    @moduledoc """
    A Thrift map
    """
    @type t  :: {:map, {Thrift.Parser.Types.t, Thrift.Parser.Types.t}}
  end

  defmodule Set do
    @moduledoc """
    A Thrift set
    """
    @type t :: {:set, Thrift.Parser.Types.t}
  end

  defmodule Container do
    @moduledoc """
    A Thrift contianer type
    """
    @type t :: List.t | Map.t | Set.t
  end

  @type t :: Container.t | Standalone.t
end

defmodule Thrift.Parser.Literals do
  @moduledoc """
  A module containing types for defining Thrift literals
  Thrift literals are used when setting default values and constants.
  """
  defmodule Primitive do
    @moduledoc """
    A Thrift primitive type
    """
    @type t :: integer | boolean | String.t | float
  end

  defmodule List do
    @moduledoc """
    A Thrift list
    """
    @type t :: [Thrift.Parser.Literals.t]
  end

  defmodule Map do
    @moduledoc """
    A Thrift map
    """
    @type t :: %{Thrift.Parser.Literals.t => Thrift.Parser.Literals.t}
  end

  defmodule Container do
    @moduledoc """
    A Thrift container type
    """
    @type t :: Map.t | List.t
  end

  @type t :: Container.t | Primitive.t
  @type s :: atom
end

  defmodule Thrift.Parser.Conversions do
    @moduledoc """
    Conversion utilities useful for parsing Thrift.
    """

    @doc """
    Ensures that the argument is an atom.
    """
    def atomify(nil), do: nil
    def atomify(l) when is_list(l) do
      List.to_atom(l)
    end

    def cast(_, nil) do
      nil
    end

    def cast(:double, val) do
      val
    end

    def cast(:string, val) do
      List.to_string(val)
    end

    def cast({:set, type}, val) do
      MapSet.new(val, &cast(type, &1))
    end

    def cast({:map, {key_type, val_type}}, val) do
      Enum.into(val, %{}, fn {k, v} ->
        {cast(key_type, k), cast(val_type, v)}
      end)
    end

    def cast(_, val) do
      val
    end
  end

  defmodule Thrift.Parser.Models do
    @moduledoc """
    Models used by the Thrift parser that represent different Thrift components.
    The models defined here are returned by the parse functions in the
    `Thrift.Parser` module.
    """

    alias Thrift.Parser.Types
    alias Thrift.Parser.Literals

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

      @type t :: %Service{name: String.t, extends: String.t, functions: [%Function{}]}
      defstruct name: nil, extends: nil, functions: []

      import Thrift.Parser.Conversions

      @spec new(char_list, [%Function{}, ...], char_list) :: %Service{}
      def new(name, functions, extends) do
        %Service{name: atomify(name), extends: atomify(extends), functions: functions}
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
      defstruct thrift_namespace: nil,
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
      alias Thrift.Parser.Models.Include
      alias Thrift.Parser.Models.Namespace
      alias Thrift.Parser.Models.Constant
      alias Thrift.Parser.Models.Exception
      alias Thrift.Parser.Models.Struct
      alias Thrift.Parser.Models.TEnum
      alias Thrift.Parser.Models.Union

      @doc """
      Constructs a schema with both headers and definitions.
      """
      @spec new([header], [definition]) :: t
      def new(headers, defs) do
        schema = headers
        |> Enum.reverse
        |> Enum.reduce(%Schema{}, &merge(&2, &1))

        defs
        |> Enum.reverse
        |> Enum.reduce(schema, &merge(&2, &1))
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
        %Schema{schema | enums: Map.put(schema.enums, enum.name, enum)}
      end

      defp merge(schema, %Exception{} = exc) do
        %Schema{schema | exceptions: Map.put(schema.exceptions, exc.name, exc)}
      end

      defp merge(schema, %Struct{} = s) do
        %Schema{schema | structs: Map.put(schema.structs, s.name, s)}
      end

      defp merge(schema, %Union{} = union) do
        %Schema{schema | unions: Map.put(schema.unions, union.name, union)}
      end

      defp merge(schema, %Service{} = service) do
        %Schema{schema | services: Map.put(schema.services, service.name, service)}
      end

      defp merge(schema, {:typedef, actual_type, type_alias}) do
        %Schema{schema | typedefs: Map.put(schema.typedefs, atomify(type_alias), actual_type)}
      end
    end

    @type all :: %Namespace{} | %Include{} | %Constant{} | %TEnum{} | %Field{} | %Exception{} | %Struct{} | %Union{} | %Function{} | %Service{} | %Schema{}
  end
