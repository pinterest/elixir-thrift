defprotocol Thrift.Serializable do
  @moduledoc """
  Protocol to serialize and deserialize thrift structs.
  """

  @doc """
  Serialize a struct to a payload
  """
  @spec serialize(thrift_struct, payload) :: payload when thrift_struct: struct, payload: var
  def serialize(thrift_struct, payload)

  @doc """
  Deserialize a struct from a payload.
  """
  @spec deserialize(thrift_struct, payload) ::
    {thrift_struct, payload} | :error when thrift_struct: struct, payload: var
  def deserialize(thrift_struct, payload)
end
