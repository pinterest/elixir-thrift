defmodule User do
  defstruct is_evil: false, user_id: 0, number_of_hairs_on_head: 0, amount_of_red: 0, nineties_era_color: 0, mint_gum: 0.0, username: "", friends: [], my_map: %{}, blocked_user_ids: MapSet.new(), optional_integers: []
end

defmodule Nesting do
  defstruct user: nil, nested: nil
end

defmodule Shared.SharedStruct do
  defstruct key: nil, value: nil
end

defmodule ParserUtils do
  alias Thrift.Parser

  def parse_thrift(file_path) do
    Parser.parse_file(file_path)
  end

  def compile_module(file_group) do
    Thrift.Generator.Models.generate_to_string!(file_group)
    |> Code.compile_string
  end

  # Debugging aid. Non-private in order to mute "function is unused" warning.
  def print_compiled_code(code_string) do
    code_string
    |> String.split("\n")
    |> Enum.with_index
    |> Enum.each(fn {line, idx} ->
      IO.puts "#{idx + 1}   #{line}"
    end)

    code_string
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
    blocked_user_ids = case Keyword.get(opts, :blocked_user_ids) do
      nil -> :undefined
      list when is_list(list) -> :sets.from_list(list)
    end
    optional_integers = Keyword.get(opts, :optional_integers, :undefined)

    {:User, is_evil, user_id, number_of_hairs_on_head,
     amount_of_red, nineties_era_color, mint_gum, username,
     friends, my_map, blocked_user_ids, optional_integers}
  end
  def user(:elixir, opts) do
    %{__struct__: User,
      is_evil: Keyword.get(opts, :is_evil),
      user_id: Keyword.get(opts, :user_id),
      number_of_hairs_on_head: Keyword.get(opts, :number_of_hairs_on_head),
      amount_of_red: Keyword.get(opts, :amount_of_red),
      nineties_era_color: Keyword.get(opts, :nineties_era_color),
      mint_gum: Keyword.get(opts, :mint_gum),
      friends: Keyword.get(opts, :friends),
      my_map: Keyword.get(opts, :my_map),
      blocked_user_ids: case Keyword.get(opts, :blocked_user_ids) do
        nil -> nil
        list when is_list(list) -> MapSet.new(list)
      end,
      username: Keyword.get(opts, :username),
      optional_integers: Keyword.get(opts, :optional_integers)
     }
  end

  def serialize_user(user, opts \\ [])
  def serialize_user(user, opts) when is_map(user) do
    alias User.BinaryProtocol
    serialized = BinaryProtocol.serialize(:struct, user)

    if Keyword.get(opts, :convert_to_binary, true) do
      IO.iodata_to_binary(serialized)
    else
      serialized
    end
  end
  def serialize_user(erlang_user, opts) when is_tuple(erlang_user) do
    struct_info = {:struct, {:simple_types, :User}}
    serialize_to_erlang(erlang_user, struct_info, opts)
  end

  def serialize_user2(user, opts) when is_map(user) do
    alias User.BinaryProtocol
    serialized = BinaryProtocol.serialize(user)

    if Keyword.get(opts, :convert_to_binary, true) do
      IO.iodata_to_binary(serialized)
    else
      serialized
    end
  end

  def deserialize_user(binary_data, :elixir) do
    {%User{}, ""} = User.BinaryProtocol.deserialize(binary_data)
  end

  def deserialize_user(binary_data, :erlang) do
    struct_definition = {:struct, {:simple_types, :User}}
    deserialize_to_erlang(binary_data, struct_definition)
  end

  def serialize_user_to_erlang(opts) do
    user(:elixir, opts)
    |> serialize_user
    |> deserialize_user(:erlang)
  end

  def shared_struct(:elixir, opts \\ []) do
    %Shared.SharedStruct{
      key: Keyword.get(opts, :key, 44291),
      value: Keyword.get(opts, :value, "Look at my value...")
    }
  end

  def nesting(:elixir, opts) do
    %Nesting{
      user: Keyword.get(opts, :user, user(:elixir)),
      nested: Keyword.get(opts, :nested, shared_struct(:elixir))}
  end

  def serialize_nesting_to_erlang(opts) do
    nesting(:elixir, opts)
    |> serialize_nesting
    |> deserialize_nesting(:erlang)
  end

  def serialize_nesting(nesting, opts \\ [])
  def serialize_nesting(nesting, opts) when is_map(nesting) do
    alias Nesting.BinaryProtocol
    serialized = BinaryProtocol.serialize(:struct, nesting)
    if Keyword.get(opts, :convert_to_binary, true) do
      IO.iodata_to_binary(serialized)
    else
      serialized
    end
  end

  def serialize_nesting(erlang_nesting, opts) when is_tuple(erlang_nesting) do
    struct_info = {:struct, {:simple_types, :Nesting}}
    serialize_to_erlang(erlang_nesting, struct_info, opts)
  end

  def deserialize_nesting(nesting_binary, :erlang) do
    struct_definition = {:struct, {:simple_types, :Nesting}}
    deserialize_to_erlang(nesting_binary, struct_definition)
  end

  defp serialize_to_erlang(thrift_structure, struct_info, opts) do
    iolist_struct = with({:ok, tf} <- :thrift_memory_buffer.new_transport_factory(),
                         {:ok, pf} <- :thrift_binary_protocol.new_protocol_factory(tf, []),
                         {:ok, binary_protocol} <- pf.()) do

      {proto, :ok} = :thrift_protocol.write(binary_protocol, {struct_info, thrift_structure})
      {_, data} = :thrift_protocol.flush_transport(proto)
      data
    end

    if Keyword.get(opts, :convert_to_binary, true) do
      :erlang.iolist_to_binary(iolist_struct)
    else
      iolist_struct
    end
  end

  def deserialize_to_erlang(binary_data, struct_definition) do
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
end
