defmodule FramedServerBenchmark do
  use Benchfella

  @thrift_file_path "./test/fixtures/app/thrift/simple.thrift"
  import ParserUtils

  def define_handler do
    defmodule Simple.Handler do
      def echo_user(user) do
        user
      end

      def ping, do: true
    end
  end

  setup_all do
    @thrift_file_path
    |> parse_thrift
    |> compile_module

    Application.start(:ranch)

    {:module, mod_name, _, _} = define_handler

    {:ok, server_pid} = SimpleService.Binary.Framed.Server.start_link(mod_name, 12345, [])

    user = %User {
      is_evil: false,
      user_id: 2841204,
      number_of_hairs_on_head: 1029448,
      amount_of_red: 23,
      nineties_era_color: 381221,
      mint_gum: 24421.024,
      username: "Stinkypants",
      my_map: %{1 => "foo", 2 => "bar", 3 => "baz"},
      optional_integers: Enum.to_list(1..100)
    }

    {:ok, client} = SimpleService.Binary.Framed.Client.start_link("localhost", 12345, [])

    {:ok, user: user, client: client}
  end

  bench "echoing a struct" do
    user = bench_context[:user]
    client = bench_context[:client]
    SimpleService.Binary.Framed.Client.echo_user(client, user)
  end

  bench "return boolean" do
    client = bench_context[:client]
    SimpleService.Binary.Framed.Client.ping(client)
  end
end
