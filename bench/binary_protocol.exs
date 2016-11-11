defmodule BinaryProtocolBenchmark do
  use Benchfella

  @thrift_file_path "./test/fixtures/app/thrift/simple.thrift"
  alias Thrift.Parser
  alias Thrift.Protocols.Binary
  import ParserUtils

  setup_all do
    file_group = parse_thrift(@thrift_file_path)
    |> compile_module

    {:ok, :ok}
  end

  before_each_bench _ do
    user_options = [username: "esteban",
                    is_evil: true,
                    number_of_hairs_on_head: 26482,
                    amount_of_red: 382,
                    mint_gum: 28.282
                   ]

    erlang_users = for _ <- 1..1000 do
      user(:erlang, user_options)
    end

    elixir_users = for _ <- 1..1000 do
      user(:elixir, user_options)
    end

    {:ok, elixir_users: elixir_users, erlang_users: erlang_users}
  end

  bench "erlang serialization" do
    for user <- bench_context[:erlang_users] do
      serialize_user(user)
    end

    :ok
  end

  bench "elixir serializtion (converted to binary)" do
    for user <- bench_context[:elixir_users] do
      serialize_user(user)
    end
    :ok
  end

  bench "elixir serializtion (left as IOList)" do
    for user <- bench_context[:elixir_users] do
      serialize_user(user, convert_to_binary: false)
    end
    :ok
  end
end
