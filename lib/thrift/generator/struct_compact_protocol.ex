defmodule Thrift.Generator.StructCompactProtocol do
  @moduledoc """
  Generates a code for Thrift Compact Protocol serialisation and deserialisation.

  This is based somewhat on `Thrift.Generator.StructBinaryProtocol`, but implemented
  for the compact protocol.

  https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md has been
  used as the description of the binary protocol, verified by testing with the Thrift
  Ruby and Python implementations. There are a few discrepancies between the document and
  the discovered implementation; where these were found the discovered implementation
  was used. These are:

  * Doubles appear to be actually encodes as little endian
  * The document states different codes to indicate types contained in maps, lists, and sets to
  those for Structs; in fact the same codes are used and 1 is used to indicate a Bool.
  * For element boolean types, false is encoded as 2 rather than 0

  The compact protocol introduces some complications which necessitate a divergence from
  the approach taken for the binary protocol.

  Field headers may be of two forms, depending on whether the type and field id information
  can fit into a single byte; this means that two deserialisation matches must be created for each
  field.

  In the short form field header, the field id is not used directly; rather the difference
  between the previously encoded field id and the field id is used, termed the _delta_. This means
  that we must maintain information about previous fields while serialising and deserialising.

  List and Set header information may also be in two forms, depending on whether the size and
  element-type information can fit into a single byte, complicating deserialisation.

  Use of varint encoding for size in various places reduces the scope for using binary matching in
  many places.

  """

  alias Thrift.Generator.Compact.{StructDeserialize, StructSerialize}

  defdelegate struct_deserializer(struct_def, name, file_group), to: StructDeserialize
  defdelegate struct_serializer(struct_def, name, file_group), to: StructSerialize
end
