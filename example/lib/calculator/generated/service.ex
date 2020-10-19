defmodule(Calculator.Generated.Service) do
  @moduledoc false
  defmodule(AddArgs) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.AddArgs"
    _ = "1: i64 left"
    _ = "2: i64 right"
    defstruct(left: nil, right: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %AddArgs{})
      end

      defp(deserialize(<<0, rest::binary>>, %AddArgs{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 1::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | left: value})
      end

      defp(deserialize(<<10, 2::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | right: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%AddArgs{left: left, right: right})) do
        [
          case(left) do
            nil ->
              <<>>

            _ ->
              <<10, 1::16-signed, left::64-signed>>
          end,
          case(right) do
            nil ->
              <<>>

            _ ->
              <<10, 2::16-signed, right::64-signed>>
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

  defmodule(DivideArgs) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.DivideArgs"
    _ = "1: i64 left"
    _ = "2: i64 right"
    defstruct(left: nil, right: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %DivideArgs{})
      end

      defp(deserialize(<<0, rest::binary>>, %DivideArgs{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 1::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | left: value})
      end

      defp(deserialize(<<10, 2::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | right: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%DivideArgs{left: left, right: right})) do
        [
          case(left) do
            nil ->
              <<>>

            _ ->
              <<10, 1::16-signed, left::64-signed>>
          end,
          case(right) do
            nil ->
              <<>>

            _ ->
              <<10, 2::16-signed, right::64-signed>>
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

  defmodule(MultiplyArgs) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.MultiplyArgs"
    _ = "1: i64 left"
    _ = "2: i64 right"
    defstruct(left: nil, right: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %MultiplyArgs{})
      end

      defp(deserialize(<<0, rest::binary>>, %MultiplyArgs{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 1::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | left: value})
      end

      defp(deserialize(<<10, 2::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | right: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%MultiplyArgs{left: left, right: right})) do
        [
          case(left) do
            nil ->
              <<>>

            _ ->
              <<10, 1::16-signed, left::64-signed>>
          end,
          case(right) do
            nil ->
              <<>>

            _ ->
              <<10, 2::16-signed, right::64-signed>>
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

  defmodule(SubtractArgs) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.SubtractArgs"
    _ = "1: i64 left"
    _ = "2: i64 right"
    defstruct(left: nil, right: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %SubtractArgs{})
      end

      defp(deserialize(<<0, rest::binary>>, %SubtractArgs{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 1::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | left: value})
      end

      defp(deserialize(<<10, 2::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | right: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%SubtractArgs{left: left, right: right})) do
        [
          case(left) do
            nil ->
              <<>>

            _ ->
              <<10, 1::16-signed, left::64-signed>>
          end,
          case(right) do
            nil ->
              <<>>

            _ ->
              <<10, 2::16-signed, right::64-signed>>
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

  defmodule(VectorProductArgs) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.VectorProductArgs"
    _ = "1: calculator.Vector left"
    _ = "2: calculator.Vector right"
    _ = "3: calculator.VectorProductType type"
    defstruct(left: nil, right: nil, type: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %VectorProductArgs{})
      end

      defp(deserialize(<<0, rest::binary>>, %VectorProductArgs{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<12, 1::16-signed, rest::binary>>, acc)) do
        case(Calculator.Generated.Vector.BinaryProtocol.deserialize(rest)) do
          {value, rest} ->
            deserialize(rest, %{acc | left: value})

          :error ->
            :error
        end
      end

      defp(deserialize(<<12, 2::16-signed, rest::binary>>, acc)) do
        case(Calculator.Generated.Vector.BinaryProtocol.deserialize(rest)) do
          {value, rest} ->
            deserialize(rest, %{acc | right: value})

          :error ->
            :error
        end
      end

      defp(deserialize(<<8, 3::16-signed, value::32-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | type: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%VectorProductArgs{left: left, right: right, type: type})) do
        [
          case(left) do
            nil ->
              <<>>

            _ ->
              [<<12, 1::16-signed>> | Calculator.Generated.Vector.serialize(left)]
          end,
          case(right) do
            nil ->
              <<>>

            _ ->
              [<<12, 2::16-signed>> | Calculator.Generated.Vector.serialize(right)]
          end,
          case(type) do
            nil ->
              <<>>

            _ ->
              <<8, 3::16-signed, type::32-signed>>
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

  defmodule(AddResponse) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.AddResponse"
    _ = "0: i64 success"
    defstruct(success: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %AddResponse{})
      end

      defp(deserialize(<<0, rest::binary>>, %AddResponse{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 0::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | success: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%AddResponse{success: success})) do
        [
          case(success) do
            nil ->
              <<>>

            _ ->
              <<10, 0::16-signed, success::64-signed>>
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

  defmodule(DivideResponse) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.DivideResponse"
    _ = "0: i64 success"
    _ = "1: calculator.DivideByZeroError e"
    defstruct(success: nil, e: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %DivideResponse{})
      end

      defp(deserialize(<<0, rest::binary>>, %DivideResponse{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 0::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | success: value})
      end

      defp(deserialize(<<12, 1::16-signed, rest::binary>>, acc)) do
        case(Calculator.Generated.DivideByZeroError.BinaryProtocol.deserialize(rest)) do
          {value, rest} ->
            deserialize(rest, %{acc | e: value})

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

      def(serialize(%DivideResponse{success: success, e: e})) do
        [
          case(success) do
            nil ->
              <<>>

            _ ->
              <<10, 0::16-signed, success::64-signed>>
          end,
          case(e) do
            nil ->
              <<>>

            _ ->
              [<<12, 1::16-signed>> | Calculator.Generated.DivideByZeroError.serialize(e)]
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

  defmodule(MultiplyResponse) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.MultiplyResponse"
    _ = "0: i64 success"
    defstruct(success: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %MultiplyResponse{})
      end

      defp(deserialize(<<0, rest::binary>>, %MultiplyResponse{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 0::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | success: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%MultiplyResponse{success: success})) do
        [
          case(success) do
            nil ->
              <<>>

            _ ->
              <<10, 0::16-signed, success::64-signed>>
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

  defmodule(SubtractResponse) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.SubtractResponse"
    _ = "0: i64 success"
    defstruct(success: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %SubtractResponse{})
      end

      defp(deserialize(<<0, rest::binary>>, %SubtractResponse{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<10, 0::16-signed, value::64-signed, rest::binary>>, acc)) do
        deserialize(rest, %{acc | success: value})
      end

      defp(deserialize(<<field_type, _id::16-signed, rest::binary>>, acc)) do
        rest |> Thrift.Protocol.Binary.skip_field(field_type) |> deserialize(acc)
      end

      defp(deserialize(_, _)) do
        :error
      end

      def(serialize(%SubtractResponse{success: success})) do
        [
          case(success) do
            nil ->
              <<>>

            _ ->
              <<10, 0::16-signed, success::64-signed>>
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

  defmodule(VectorProductResponse) do
    @moduledoc false
    _ = "Auto-generated Thrift struct Elixir.VectorProductResponse"
    _ = "0: calculator.VectorProductResult success"
    defstruct(success: nil)
    @type t :: %__MODULE__{}
    def(new) do
      %__MODULE__{}
    end

    defmodule(BinaryProtocol) do
      @moduledoc false
      def(deserialize(binary)) do
        deserialize(binary, %VectorProductResponse{})
      end

      defp(deserialize(<<0, rest::binary>>, %VectorProductResponse{} = acc)) do
        {acc, rest}
      end

      defp(deserialize(<<12, 0::16-signed, rest::binary>>, acc)) do
        case(Calculator.Generated.VectorProductResult.BinaryProtocol.deserialize(rest)) do
          {value, rest} ->
            deserialize(rest, %{acc | success: value})

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

      def(serialize(%VectorProductResponse{success: success})) do
        [
          case(success) do
            nil ->
              <<>>

            _ ->
              [<<12, 0::16-signed>> | Calculator.Generated.VectorProductResult.serialize(success)]
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

  defmodule(Binary.Framed.Client) do
    @moduledoc false
    alias(Thrift.Binary.Framed.Client, as: ClientImpl)
    defdelegate(close(conn), to: ClientImpl)
    defdelegate(connect(conn, opts), to: ClientImpl)
    defdelegate(start_link(host, port, opts \\ []), to: ClientImpl)

    def(unquote(:add)(client, left, right, rpc_opts \\ [])) do
      args = %AddArgs{left: left, right: right}
      serialized_args = AddArgs.BinaryProtocol.serialize(args)
      ClientImpl.call(client, "add", serialized_args, AddResponse.BinaryProtocol, rpc_opts)
    end

    def(unquote(:add!)(client, left, right, rpc_opts \\ [])) do
      case(unquote(:add)(client, left, right, rpc_opts)) do
        {:ok, rsp} ->
          rsp

        {:error, {:exception, ex}} ->
          raise(ex)

        {:error, reason} ->
          raise(Thrift.ConnectionError, reason: reason)
      end
    end

    def(unquote(:divide)(client, left, right, rpc_opts \\ [])) do
      args = %DivideArgs{left: left, right: right}
      serialized_args = DivideArgs.BinaryProtocol.serialize(args)
      ClientImpl.call(client, "divide", serialized_args, DivideResponse.BinaryProtocol, rpc_opts)
    end

    def(unquote(:divide!)(client, left, right, rpc_opts \\ [])) do
      case(unquote(:divide)(client, left, right, rpc_opts)) do
        {:ok, rsp} ->
          rsp

        {:error, {:exception, ex}} ->
          raise(ex)

        {:error, reason} ->
          raise(Thrift.ConnectionError, reason: reason)
      end
    end

    def(unquote(:multiply)(client, left, right, rpc_opts \\ [])) do
      args = %MultiplyArgs{left: left, right: right}
      serialized_args = MultiplyArgs.BinaryProtocol.serialize(args)

      ClientImpl.call(
        client,
        "multiply",
        serialized_args,
        MultiplyResponse.BinaryProtocol,
        rpc_opts
      )
    end

    def(unquote(:multiply!)(client, left, right, rpc_opts \\ [])) do
      case(unquote(:multiply)(client, left, right, rpc_opts)) do
        {:ok, rsp} ->
          rsp

        {:error, {:exception, ex}} ->
          raise(ex)

        {:error, reason} ->
          raise(Thrift.ConnectionError, reason: reason)
      end
    end

    def(unquote(:subtract)(client, left, right, rpc_opts \\ [])) do
      args = %SubtractArgs{left: left, right: right}
      serialized_args = SubtractArgs.BinaryProtocol.serialize(args)

      ClientImpl.call(
        client,
        "subtract",
        serialized_args,
        SubtractResponse.BinaryProtocol,
        rpc_opts
      )
    end

    def(unquote(:subtract!)(client, left, right, rpc_opts \\ [])) do
      case(unquote(:subtract)(client, left, right, rpc_opts)) do
        {:ok, rsp} ->
          rsp

        {:error, {:exception, ex}} ->
          raise(ex)

        {:error, reason} ->
          raise(Thrift.ConnectionError, reason: reason)
      end
    end

    def(unquote(:vector_product)(client, left, right, type, rpc_opts \\ [])) do
      args = %VectorProductArgs{left: left, right: right, type: type}
      serialized_args = VectorProductArgs.BinaryProtocol.serialize(args)

      ClientImpl.call(
        client,
        "vectorProduct",
        serialized_args,
        VectorProductResponse.BinaryProtocol,
        rpc_opts
      )
    end

    def(unquote(:vector_product!)(client, left, right, type, rpc_opts \\ [])) do
      case(unquote(:vector_product)(client, left, right, type, rpc_opts)) do
        {:ok, rsp} ->
          rsp

        {:error, {:exception, ex}} ->
          raise(ex)

        {:error, reason} ->
          raise(Thrift.ConnectionError, reason: reason)
      end
    end
  end

  defmodule(Binary.Framed.Server) do
    @moduledoc false
    require(Logger)
    alias(Thrift.Binary.Framed.Server, as: ServerImpl)
    defdelegate(stop(name), to: ServerImpl)

    def(start_link(handler_module, port, opts \\ [])) do
      ServerImpl.start_link(__MODULE__, port, handler_module, opts)
    end

    def(handle_thrift("add", binary_data, handler_module)) do
      case(Calculator.Generated.Service.AddArgs.BinaryProtocol.deserialize(binary_data)) do
        {%Calculator.Generated.Service.AddArgs{left: left, right: right}, ""} ->
          try do
            result = handler_module.add(left, right)
            response = %Calculator.Generated.Service.AddResponse{success: result}
            {:reply, Calculator.Generated.Service.AddResponse.BinaryProtocol.serialize(response)}
          catch
            kind, reason ->
              formatted_exception = Exception.format(kind, reason, System.stacktrace())

              Logger.error(
                "Exception not defined in thrift spec was thrown: #{formatted_exception}"
              )

              error =
                Thrift.TApplicationException.exception(
                  type: :internal_error,
                  message: "Server error: #{formatted_exception}"
                )

              {:server_error, error}
          end

        {_, extra} ->
          raise(Thrift.TApplicationException,
            type: :protocol_error,
            message: "Could not decode #{inspect(extra)}"
          )
      end
    end

    def(handle_thrift("divide", binary_data, handler_module)) do
      case(Calculator.Generated.Service.DivideArgs.BinaryProtocol.deserialize(binary_data)) do
        {%Calculator.Generated.Service.DivideArgs{left: left, right: right}, ""} ->
          try do
            result = handler_module.divide(left, right)
            response = %Calculator.Generated.Service.DivideResponse{success: result}

            {:reply,
             Calculator.Generated.Service.DivideResponse.BinaryProtocol.serialize(response)}
          catch
            :error, %Calculator.Generated.DivideByZeroError{} = e ->
              response = %Calculator.Generated.Service.DivideResponse{e: e}

              {:reply,
               Calculator.Generated.Service.DivideResponse.BinaryProtocol.serialize(response)}

            kind, reason ->
              formatted_exception = Exception.format(kind, reason, System.stacktrace())

              Logger.error(
                "Exception not defined in thrift spec was thrown: #{formatted_exception}"
              )

              error =
                Thrift.TApplicationException.exception(
                  type: :internal_error,
                  message: "Server error: #{formatted_exception}"
                )

              {:server_error, error}
          end

        {_, extra} ->
          raise(Thrift.TApplicationException,
            type: :protocol_error,
            message: "Could not decode #{inspect(extra)}"
          )
      end
    end

    def(handle_thrift("multiply", binary_data, handler_module)) do
      case(Calculator.Generated.Service.MultiplyArgs.BinaryProtocol.deserialize(binary_data)) do
        {%Calculator.Generated.Service.MultiplyArgs{left: left, right: right}, ""} ->
          try do
            result = handler_module.multiply(left, right)
            response = %Calculator.Generated.Service.MultiplyResponse{success: result}

            {:reply,
             Calculator.Generated.Service.MultiplyResponse.BinaryProtocol.serialize(response)}
          catch
            kind, reason ->
              formatted_exception = Exception.format(kind, reason, System.stacktrace())

              Logger.error(
                "Exception not defined in thrift spec was thrown: #{formatted_exception}"
              )

              error =
                Thrift.TApplicationException.exception(
                  type: :internal_error,
                  message: "Server error: #{formatted_exception}"
                )

              {:server_error, error}
          end

        {_, extra} ->
          raise(Thrift.TApplicationException,
            type: :protocol_error,
            message: "Could not decode #{inspect(extra)}"
          )
      end
    end

    def(handle_thrift("subtract", binary_data, handler_module)) do
      case(Calculator.Generated.Service.SubtractArgs.BinaryProtocol.deserialize(binary_data)) do
        {%Calculator.Generated.Service.SubtractArgs{left: left, right: right}, ""} ->
          try do
            result = handler_module.subtract(left, right)
            response = %Calculator.Generated.Service.SubtractResponse{success: result}

            {:reply,
             Calculator.Generated.Service.SubtractResponse.BinaryProtocol.serialize(response)}
          catch
            kind, reason ->
              formatted_exception = Exception.format(kind, reason, System.stacktrace())

              Logger.error(
                "Exception not defined in thrift spec was thrown: #{formatted_exception}"
              )

              error =
                Thrift.TApplicationException.exception(
                  type: :internal_error,
                  message: "Server error: #{formatted_exception}"
                )

              {:server_error, error}
          end

        {_, extra} ->
          raise(Thrift.TApplicationException,
            type: :protocol_error,
            message: "Could not decode #{inspect(extra)}"
          )
      end
    end

    def(handle_thrift("vectorProduct", binary_data, handler_module)) do
      case(
        Calculator.Generated.Service.VectorProductArgs.BinaryProtocol.deserialize(binary_data)
      ) do
        {%Calculator.Generated.Service.VectorProductArgs{left: left, right: right, type: type},
         ""} ->
          try do
            result = handler_module.vector_product(left, right, type)
            response = %Calculator.Generated.Service.VectorProductResponse{success: result}

            {:reply,
             Calculator.Generated.Service.VectorProductResponse.BinaryProtocol.serialize(response)}
          catch
            kind, reason ->
              formatted_exception = Exception.format(kind, reason, System.stacktrace())

              Logger.error(
                "Exception not defined in thrift spec was thrown: #{formatted_exception}"
              )

              error =
                Thrift.TApplicationException.exception(
                  type: :internal_error,
                  message: "Server error: #{formatted_exception}"
                )

              {:server_error, error}
          end

        {_, extra} ->
          raise(Thrift.TApplicationException,
            type: :protocol_error,
            message: "Could not decode #{inspect(extra)}"
          )
      end
    end
  end
end
