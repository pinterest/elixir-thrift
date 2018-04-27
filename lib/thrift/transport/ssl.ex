defmodule Thrift.Transport.SSL do
  @moduledoc """
  SSL configuration helpers.

  Clients and servers support SSL with the `:ssl_opts` start_link option. There are additional options from `:ssl` but
  otherwise configuration is the same:

  ## Options
      * :enabled - Whether ssl is enabled (default: `false`)
      * :configure - Get extra configuration at handshake time (default: `nil`)

  ## Delayed configure option

  The value can be of the form `{module, function, args}` or a zero arity fun. The function should return
  `{:ok, opts}` to *add* options or `{:error, Exception.t} to abort the handshake with an exception. This
  option should be used when it is beneficial to delay configuring the client or server, perhaps to protect
  credentials or to change the configuration during run time.
  """

  @type configure :: {module, function, list} | (() -> ({:ok, [option]} | {:error, Exception.t}))
  @type option :: :ssl.ssloption | {:enabled, boolean} | {:configure, configure}

  @spec configuration([option]) :: {:ok, [:ssl.ssloption]} | nil | {:error, Exceptiont.t}
  def configuration(opts) do
    case Keyword.pop(opts, :enabled, false) do
      {true, opts} ->
        update_configuration(opts)
      {false, _} ->
        nil
    end
  end

  defp update_configuration(opts) do
    {configure, opts} = Keyword.pop(opts, :configure)
    case apply_configure(configure) do
      {:ok, extra_opts} ->
        {:ok, extra_opts ++ opts}
      {:error, _} = error ->
        error
    end
  end

  defp apply_configure({module, fun, args}), do: apply(module, fun, args)
  defp apply_configure(fun) when is_function(fun, 0), do: fun.()
  defp apply_configure(nil), do: {:ok, []}
end
