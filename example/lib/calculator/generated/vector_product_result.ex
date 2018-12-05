defmodule(Calculator.Generated.VectorProductResult) do
  @moduledoc(false)
  _ = "Auto-generated Thrift union calculator.VectorProductResult"
  _ = "1: double scalar"
  _ = "2: calculator.Vector vector"
  defstruct(scalar: nil, vector: nil)
  @type(t :: %__MODULE__{})
  def(new) do
    %__MODULE__{}
  end
  defmodule(BinaryProtocol) do
    @moduledoc(false)
    def(deserialize(binary)) do
      deserialize(binary, %Calculator.Generated.VectorProductResult{})
    end
    defp(deserialize(<<0, rest::binary>>, %Calculator.Generated.VectorProductResult{} = acc)) do
      {acc, rest}
    end
    defp(deserialize(<<4, 1::16-signed, 0::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | scalar: :inf})
    end
    defp(deserialize(<<4, 1::16-signed, 1::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | scalar: :"-inf"})
    end
    defp(deserialize(<<4, 1::16-signed, sign::1, 2047::11, frac::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | scalar: %Thrift.NaN{sign: sign, fraction: frac}})
    end
    defp(deserialize(<<4, 1::16-signed, value::float-signed, rest::binary>>, acc)) do
      deserialize(rest, %{acc | scalar: value})
    end
    defp(deserialize(<<12, 2::16-signed, rest::binary>>, acc)) do
      case(Elixir.Calculator.Generated.Vector.BinaryProtocol.deserialize(rest)) do
        {value, rest} ->
          deserialize(rest, %{acc | vector: value})
        :error ->
          :error
      end
    end
    defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
      rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
    end
    defp(deserialize(_, _)) do
      :error
    end
    def(serialize(%Calculator.Generated.VectorProductResult{scalar: nil, vector: nil})) do
      <<0>>
    end
    def(serialize(%Calculator.Generated.VectorProductResult{scalar: scalar, vector: nil})) do
      [<<4, 1::16-signed>>, case(scalar) do
        :inf ->
          <<0::1, 2047::11, 0::52>>
        :"-inf" ->
          <<1::1, 2047::11, 0::52>>
        %Thrift.NaN{sign: sign, fraction: frac} ->
          <<sign::1, 2047::11, frac::52>>
        _ ->
          <<scalar::float-signed>>
      end | <<0>>]
    end
    def(serialize(%Calculator.Generated.VectorProductResult{scalar: nil, vector: vector})) do
      [<<12, 2::16-signed>>, Calculator.Generated.Vector.serialize(vector) | <<0>>]
    end
    def(serialize(%Calculator.Generated.VectorProductResult{} = value)) do
      set_fields = value |> Map.from_struct() |> Enum.flat_map(fn
        {_, nil} ->
          []
        {key, _} ->
          [key]
      end)
      raise(%Thrift.Union.TooManyFieldsSetError{message: "Thrift union has more than one field set", set_fields: set_fields})
    end
  end
  def(serialize(struct)) do
    BinaryProtocol.serialize(struct)
  end
  def(serialize(struct, :binary)) do
    BinaryProtocol.serialize(struct)
  end
  def(deserialize(binary)) do
    BinaryProtocol.deserialize(binary)
  end
end