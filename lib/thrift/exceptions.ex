defmodule Thrift.TApplicationException do
  defexception message: nil, type: nil


  def exception_type(1), do: :unknown_method
  def exception_type(2), do: :invalid_message_type
  def exception_type(3), do: :wrong_method_name
  def exception_type(4), do: :bad_sequence_id
  def exception_type(5), do: :missing_result
  def exception_type(6), do: :internal_error
  def exception_type(7), do: :protocol_error
  def exception_type(8), do: :invalid_transform
  def exception_type(9), do: :invalid_protocol
  def exception_type(10), do: :unsupported_client_type
  def exception_type(_), do: :unknown
end

defmodule Thrift.Union.TooManyFieldsSetException do
  @moduledoc """
  This exception occurs when a Union is serialized and more than one
  field is set.
  """
  defexception message: nil, set_fields: nil
end

defmodule Thrift.MissingFieldException do
  defexception message: nil
end
