defmodule Thrift.TApplicationException do
  @moduledoc """
  Application-level exception
  """

  @enforce_keys [:message, :type]
  defexception message: nil, type: :unknown

  # This list represents the set of well-known TApplicationException types.
  # We primarily use their atom names, but we also need their standardized
  # integer values for representing these values in their serialized form.
  @exception_types [
    unknown:                  0,
    unknown_method:           1,
    invalid_message_type:     2,
    wrong_method_name:        3,
    bad_sequence_id:          4,
    missing_result:           5,
    internal_error:           6,
    protocol_error:           7,
    invalid_transform:        8,
    invalid_protocol:         9,
    unsupported_client_type:  10,
    loadshedding:             11,
    timeout:                  12,
    injected_failure:         13,
  ]

  def exception(args) when is_list(args) do
    %__MODULE__{message: args[:message], type: normalize_type(args[:type])}
  end

  @doc """
  Converts an exception type to its integer identifier.
  """
  @spec type_id(atom) :: non_neg_integer
  def type_id(type)

  for {type, id} <- @exception_types do
    def type_id(unquote(type)), do: unquote(id)
    defp normalize_type(unquote(id)), do: unquote(type)
    defp normalize_type(unquote(type)), do: unquote(type)
  end
  defp normalize_type(type) when is_integer(type), do: :unknown
end

defmodule Thrift.Union.TooManyFieldsSetException do
  @moduledoc """
  This exception occurs when a Union is serialized and more than one
  field is set.
  """
  defexception message: nil, set_fields: nil
end

defmodule Thrift.FileParseException do
  @moduledoc """
  This exception occurs when a thrift file fails to parse
  """

  defexception message: nil

  @doc false  # Exception callback, should not be called by end user
  @spec exception({Thrift.Parser.FileRef.t, term}) :: Exception.t
  def exception({file_ref, error}) do
    msg = "Error parsing thrift file #{file_ref.path} #{format_error(error)}"
    %Thrift.FileParseException{message: msg}
  end

  # display the line number if we get it
  defp format_error({line_no, :thrift_parser, errors}) do
    "on line #{line_no}: #{errors}"
  end
  defp format_error({{line_no, :thrift_lexer, errors}, _}) do
    "on line #{line_no}: #{inspect errors}"
  end
  defp format_error(error) do
    ": #{inspect error}"
  end
end

defmodule Thrift.InvalidValueException do
  defexception message: nil
end
