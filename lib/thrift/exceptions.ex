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

defmodule Thrift.InvalidValueException do
  defexception message: nil
end
