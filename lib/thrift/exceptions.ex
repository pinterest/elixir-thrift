defmodule Thrift.TApplicationException do
  defexception message: nil, type: nil

  @exception_mappings %{
    unknown_method: 1,
    invalid_message_type: 2,
    wrong_method_name: 3,
    bad_sequence_id: 4,
    missing_result: 5,
    internal_error: 6,
    protocol_error: 7,
    invalid_transform: 8,
    invalid_protocol: 9,
    unsupported_client_type: 10
  }

  for {atom_name, type_id} <- @exception_mappings do
    def exception_type(unquote(type_id)), do: unquote(atom_name)
  end
  def exception_type(_), do: :unknown

  for {atom_name, type_id} <- @exception_mappings do
    def exception_type_to_int(unquote(atom_name)), do: unquote(type_id)
  end

  def exception_type_to_int(_), do: 0

  def serialize(exception, :binary) do
    Protocol.Binary.serialize(:application_exception, exception)
  end
  def deserialize(binary) do
    Protocol.Binary.deserialize(:application_exception, binary)
  end
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
