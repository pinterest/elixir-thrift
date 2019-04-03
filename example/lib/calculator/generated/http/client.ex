defmodule(Calculator.Generated.HTTP.Client) do
  require(Logger)
  @default_headers Accept: "application/x-thrift", "Content-Type": "application/x-thrift"
  @http_client_options hackney: [pool: :default]
  @service_endpoint Application.get_env(
                      Mix.Project.config()[:app],
                      Calculator.Generated.HTTP.Client,
                      %{}
                    )[:endpoint]
  def(post_thrift(serialized_binary)) do
    if(@service_endpoint == nil) do
      Logger.error(
        "service endpoint not defined for #{Macro.to_string(__MODULE__)}!! Please add it to config file!"
      )
    end

    case(
      HTTPoison.post(@service_endpoint, serialized_binary, @default_headers, @http_client_options)
    ) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}

      {:ok, %HTTPoison.Response{status_code: code, body: body}} ->
        {:service_err, code, body}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:http_err, inspect(reason)}
    end
  end
end

defmodule(Calculator.Generated.HTTP.Service) do
  @behaviour Calculator.Generated.Service.Handler
  alias(Calculator.Generated.Service)
  alias(Calculator.Generated.HTTP)
  alias(Thrift.Protocol.Binary)

  (
    @doc "left: i64\n\nright: i64"
    def(add(left, right)) do
      args = %Service.AddArgs{left: left, right: right}
      serialized_args = Service.AddArgs.BinaryProtocol.serialize(args)
      tcall = Binary.serialize(:message_begin, {:call, 0, "add"})
      payload = [tcall | serialized_args] |> IO.iodata_to_binary()

      with(
        {:ok, body} <- HTTP.Client.post_thrift(payload),
        {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
        {response, _} <- Service.AddResponse.deserialize(ser_resp)
      ) do
        response
      else
        {:service_err, code, body} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "service error [#{code}] #{body}"
          )

        {:http_err, err_str} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "http error: #{err_str}"
          )

        {:ok, {:exception, _, _, ser_ex}} ->
          Binary.deserialize(:application_exception, ser_ex)
      end
    end
  )

  (
    @doc "left: i64\n\nright: i64"
    def(divide(left, right)) do
      args = %Service.DivideArgs{left: left, right: right}
      serialized_args = Service.DivideArgs.BinaryProtocol.serialize(args)
      tcall = Binary.serialize(:message_begin, {:call, 0, "divide"})
      payload = [tcall | serialized_args] |> IO.iodata_to_binary()

      with(
        {:ok, body} <- HTTP.Client.post_thrift(payload),
        {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
        {response, _} <- Service.DivideResponse.deserialize(ser_resp)
      ) do
        response
      else
        {:service_err, code, body} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "service error [#{code}] #{body}"
          )

        {:http_err, err_str} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "http error: #{err_str}"
          )

        {:ok, {:exception, _, _, ser_ex}} ->
          Binary.deserialize(:application_exception, ser_ex)
      end
    end
  )

  (
    @doc "left: i64\n\nright: i64"
    def(multiply(left, right)) do
      args = %Service.MultiplyArgs{left: left, right: right}
      serialized_args = Service.MultiplyArgs.BinaryProtocol.serialize(args)
      tcall = Binary.serialize(:message_begin, {:call, 0, "multiply"})
      payload = [tcall | serialized_args] |> IO.iodata_to_binary()

      with(
        {:ok, body} <- HTTP.Client.post_thrift(payload),
        {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
        {response, _} <- Service.MultiplyResponse.deserialize(ser_resp)
      ) do
        response
      else
        {:service_err, code, body} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "service error [#{code}] #{body}"
          )

        {:http_err, err_str} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "http error: #{err_str}"
          )

        {:ok, {:exception, _, _, ser_ex}} ->
          Binary.deserialize(:application_exception, ser_ex)
      end
    end
  )

  (
    @doc "left: i64\n\nright: i64"
    def(subtract(left, right)) do
      args = %Service.SubtractArgs{left: left, right: right}
      serialized_args = Service.SubtractArgs.BinaryProtocol.serialize(args)
      tcall = Binary.serialize(:message_begin, {:call, 0, "subtract"})
      payload = [tcall | serialized_args] |> IO.iodata_to_binary()

      with(
        {:ok, body} <- HTTP.Client.post_thrift(payload),
        {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
        {response, _} <- Service.SubtractResponse.deserialize(ser_resp)
      ) do
        response
      else
        {:service_err, code, body} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "service error [#{code}] #{body}"
          )

        {:http_err, err_str} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "http error: #{err_str}"
          )

        {:ok, {:exception, _, _, ser_ex}} ->
          Binary.deserialize(:application_exception, ser_ex)
      end
    end
  )

  (
    @doc "left: `Calculator.Generated.Vector`\n\nright: `Calculator.Generated.Vector`\n\ntype: `Calculator.Generated.VectorProductType`"
    def(vector_product(left, right, type)) do
      args = %Service.VectorProductArgs{left: left, right: right, type: type}
      serialized_args = Service.VectorProductArgs.BinaryProtocol.serialize(args)
      tcall = Binary.serialize(:message_begin, {:call, 0, "vectorProduct"})
      payload = [tcall | serialized_args] |> IO.iodata_to_binary()

      with(
        {:ok, body} <- HTTP.Client.post_thrift(payload),
        {:ok, {:reply, _, _, ser_resp}} <- Binary.deserialize(:message_begin, body),
        {response, _} <- Service.VectorProductResponse.deserialize(ser_resp)
      ) do
        response
      else
        {:service_err, code, body} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "service error [#{code}] #{body}"
          )

        {:http_err, err_str} ->
          Thrift.TApplicationException.exception(
            type: :missing_result,
            message: "http error: #{err_str}"
          )

        {:ok, {:exception, _, _, ser_ex}} ->
          Binary.deserialize(:application_exception, ser_ex)
      end
    end
  )
end
