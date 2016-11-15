defmodule ParserUtils do
  alias Thrift.Parser
  alias Thrift.Protocols.Binary

  def parse_thrift(file_path) do
    Parser.parse_file(file_path)
  end

  def compile_module(file_group) do
    quoted = quote do
      defmodule Testing.Simple do
        defmodule User do
          defstruct is_evil: false,
          user_id: 1234,
          number_of_hairs_on_head: 26,
          amount_of_red: 254,
          nineties_era_color: 655236,
          mint_gum: 12.6,
          username: "frink",
          friends: [%{username: "frank"}],
          my_map: %{2 => "good", 29 => "bad"},
          blocked_user_ids: MapSet.new([442, 8281])
        end

        defmodule BinaryProtocol do
          unquote(Binary.primitive_serializers)
          unquote(Binary.build(file_group, Testing.Simple.User))
        end
      end
    end
    #    quoted |> Macro.to_string |> IO.puts
    quoted |> Code.compile_quoted
  end

  def user(type, opts \\ [])
  def user(:erlang, opts) do
    is_evil = Keyword.get(opts, :is_evil, :undefined)
    user_id = Keyword.get(opts, :user_id, :undefined)
    number_of_hairs_on_head = Keyword.get(opts, :number_of_hairs_on_head, :undefined)
    amount_of_red = Keyword.get(opts, :amount_of_red, :undefined)
    nineties_era_color = Keyword.get(opts, :nineties_era_color, :undefined)
    mint_gum = Keyword.get(opts, :mint_gum, :undefined)
    username = Keyword.get(opts, :username, :undefined)
    friends = Keyword.get(opts, :friends, :undefined)
    my_map = Keyword.get(opts, :my_map, :undefined)
    blocked_user_ids = Keyword.get(opts, :blocked_user_ids, :undefined)
    optional_integers = Keyword.get(opts, :optional_integers, :undefined)

    {:User, is_evil, user_id, number_of_hairs_on_head,
     amount_of_red, nineties_era_color, mint_gum, username,
     friends, my_map, blocked_user_ids, optional_integers}
  end

  def user(:elixir, opts) do
    %{__struct__: Testing.Simple.User,
      is_evil: Keyword.get(opts, :is_evil),
      user_id: Keyword.get(opts, :user_id),
      number_of_hairs_on_head: Keyword.get(opts, :number_of_hairs_on_head),
      amount_of_red: Keyword.get(opts, :amount_of_red),
      nineties_era_color: Keyword.get(opts, :nineties_era_color),
      mint_gum: Keyword.get(opts, :mint_gum),
      friends: Keyword.get(opts, :friends),
      my_map: Keyword.get(opts, :my_map),
      blocked_user_ids: Keyword.get(opts, :blocked_user_ids),
      username: Keyword.get(opts, :username),
      optional_integers: Keyword.get(opts, :optional_integers)
     }
  end

  def serialize_user(user, opts \\ [])
  def serialize_user(user, opts) when is_map(user) do
    alias Testing.Simple.BinaryProtocol
    serialized = BinaryProtocol.serialize(:struct, user)

    if Keyword.get(opts, :convert_to_binary, true) do
      IO.iodata_to_binary(serialized)
    else
      serialized
    end
  end

  def serialize_user(erlang_user, _opts) when is_tuple(erlang_user) do
    struct_info = {:struct, {:simple_types, :User}}
    with({:ok, tf} <- :thrift_memory_buffer.new_transport_factory(),
         {:ok, pf} <- :thrift_binary_protocol.new_protocol_factory(tf, []),
         {:ok, binary_protocol} <- pf.()) do

      {proto, :ok} = :thrift_protocol.write(binary_protocol, {struct_info, erlang_user})
      {_, data} = :thrift_protocol.flush_transport(proto)
      :erlang.iolist_to_binary(data)
    end
  end

  def deserialize_user(binary_data, :erlang) do
    struct_definition = {:struct, {:simple_types, :User}}
    try do

      with({:ok, memory_buffer_transport} <- :thrift_memory_buffer.new(binary_data),
           {:ok, binary_protocol} <- :thrift_binary_protocol.new(memory_buffer_transport),
           {_, {:ok, record}} <- :thrift_protocol.read(binary_protocol, struct_definition)) do

        record
      end
    rescue _ ->
        {:error, :cant_decode}
    end
  end

  def serialize_user_to_erlang(opts) do
    user(:elixir, opts)
    |> serialize_user
    |> deserialize_user(:erlang)
  end
end
