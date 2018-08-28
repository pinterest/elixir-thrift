defmodule(Calculator.Generated.Vector) do
  _ = "Auto-generated Thrift struct calculator.Vector"
  _ = "1: double x"
  _ = "2: double y"
  _ = "3: double z"
  defstruct(x: 0.0, y: 0.0, z: 0.0)
  @type t :: %__MODULE__{}
  def(new) do
    %__MODULE__{}
  end

  defmodule(BinaryProtocol) do
    @moduledoc false
    def(deserialize(binary)) do
      deserialize(binary, %Calculator.Generated.Vector{})
    end

    defp(deserialize(<<0, rest::binary>>, %Calculator.Generated.Vector{} = acc)) do
      {acc, rest}
    end

    defp(deserialize(<<4, 1::16-signed, 0::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | x: :inf})
    end

    defp(deserialize(<<4, 1::16-signed, 1::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | x: :"-inf"})
    end

    defp(deserialize(<<4, 1::16-signed, sign::1, 2047::11, frac::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | x: %Thrift.NaN{sign: sign, fraction: frac}})
    end

    defp(deserialize(<<4, 1::16-signed, value::float-signed, rest::binary>>, acc)) do
      deserialize(rest, %{acc | x: value})
    end

    defp(deserialize(<<4, 2::16-signed, 0::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | y: :inf})
    end

    defp(deserialize(<<4, 2::16-signed, 1::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | y: :"-inf"})
    end

    defp(deserialize(<<4, 2::16-signed, sign::1, 2047::11, frac::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | y: %Thrift.NaN{sign: sign, fraction: frac}})
    end

    defp(deserialize(<<4, 2::16-signed, value::float-signed, rest::binary>>, acc)) do
      deserialize(rest, %{acc | y: value})
    end

    defp(deserialize(<<4, 3::16-signed, 0::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | z: :inf})
    end

    defp(deserialize(<<4, 3::16-signed, 1::1, 2047::11, 0::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | z: :"-inf"})
    end

    defp(deserialize(<<4, 3::16-signed, sign::1, 2047::11, frac::52, rest::binary>>, acc)) do
      deserialize(rest, %{acc | z: %Thrift.NaN{sign: sign, fraction: frac}})
    end

    defp(deserialize(<<4, 3::16-signed, value::float-signed, rest::binary>>, acc)) do
      deserialize(rest, %{acc | z: value})
    end

    defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
      rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
    end

    defp(deserialize(_, _)) do
      :error
    end

    def(serialize(%Calculator.Generated.Vector{x: x, y: y, z: z})) do
      [
        case(x) do
          nil ->
            <<>>

          _ ->
            [
              <<4, 1::16-signed>>
              | case(x) do
                  :inf ->
                    <<0::1, 2047::11, 0::52>>

                  :"-inf" ->
                    <<1::1, 2047::11, 0::52>>

                  %Thrift.NaN{sign: sign, fraction: frac} ->
                    <<sign::1, 2047::11, frac::52>>

                  _ ->
                    <<x::float-signed>>
                end
            ]
        end,
        case(y) do
          nil ->
            <<>>

          _ ->
            [
              <<4, 2::16-signed>>
              | case(y) do
                  :inf ->
                    <<0::1, 2047::11, 0::52>>

                  :"-inf" ->
                    <<1::1, 2047::11, 0::52>>

                  %Thrift.NaN{sign: sign, fraction: frac} ->
                    <<sign::1, 2047::11, frac::52>>

                  _ ->
                    <<y::float-signed>>
                end
            ]
        end,
        case(z) do
          nil ->
            <<>>

          _ ->
            [
              <<4, 3::16-signed>>
              | case(z) do
                  :inf ->
                    <<0::1, 2047::11, 0::52>>

                  :"-inf" ->
                    <<1::1, 2047::11, 0::52>>

                  %Thrift.NaN{sign: sign, fraction: frac} ->
                    <<sign::1, 2047::11, frac::52>>

                  _ ->
                    <<z::float-signed>>
                end
            ]
        end
        | <<0>>
      ]
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
