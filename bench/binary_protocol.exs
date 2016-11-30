defmodule BinaryProtocolBenchmark do
  use Benchfella

  @thrift_file_path "./test/fixtures/app/thrift/simple.thrift"
  import ParserUtils

  setup_all do
    parse_thrift(@thrift_file_path)
    |> compile_module

    {:ok, :ok}
  end

  before_each_bench _ do
    user_options = [
      is_evil: true,
      user_id: 1234567,
      number_of_hairs_on_head: 26482,
      amount_of_red: 182,
      nineties_era_color: 24345,
      mint_gum: 28.282,
      username: "esteban",
      friends: [],
      # # my_map: %{1 => "abc", 2 => "def", 3 => "asldfkjlasdkjf"},
      # blocked_user_ids: [2234, 2345, 654365, 4356, 3456, 1234, 234, 2345, 3456, 4567],
      optional_integers: [2234, 2345, 654365, 4356, 3456, 1234, 234, 2345, 3456, 4567],
    ]

    erlang_users = for _ <- 1..1000 do
      user(:erlang, user_options)
    end

    elixir_users = for _ <- 1..1000 do
      user(:elixir, user_options)
    end

    user_binary = user(:elixir, user_options)
    |> serialize_user_elixir(convert_to_binary: true)

    context = [
      elixir_users: elixir_users,
      erlang_users: erlang_users,
      user_binary: user_binary,
    ]
    {:ok, context}
  end

  bench "erlang serialization (converted to binary)" do
    for user <- bench_context[:erlang_users] do
      serialize_user_erlang(user, convert_to_binary: true)
    end
    :ok
  end

  bench "erlang serialization left as IOList" do
    for user <- bench_context[:erlang_users] do
      serialize_user_erlang(user, convert_to_binary: false)
    end
    :ok
  end

  bench "elixir serialization (iolist_size)" do
    for user <- bench_context[:elixir_users] do
      serialize_user_elixir(user, convert_to_binary: false)
      |> :erlang.iolist_size
    end
    :ok
  end

  bench "elixir serialization (converted to binary)" do
    for user <- bench_context[:elixir_users] do
      serialize_user_elixir(user, convert_to_binary: true)
    end
    :ok
  end

  bench "elixir serialization (left as IOList)" do
    for user <- bench_context[:elixir_users] do
      serialize_user_elixir(user, convert_to_binary: false)
    end
    :ok
  end

  bench "erlang deserialization" do
    for _ <- 1..1000 do
      deserialize_user_erlang(bench_context[:user_binary])
    end
    :ok
  end

  bench "elixir deserialization" do
    for _ <- 1..1000 do
      deserialize_user_elixir(bench_context[:user_binary])
    end
    :ok
  end
end
