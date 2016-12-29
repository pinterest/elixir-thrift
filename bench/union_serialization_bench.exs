defmodule UnionSerializationBenchmark do
  use Benchfella

  @thrift_file_path "./test/fixtures/app/thrift/simple.thrift"
  import ParserUtils

  setup_all do
    parse_thrift(@thrift_file_path)
    |> compile_module

    {:ok, :ok}
  end

  before_each_bench _ do
    user = %User{u: %TestUnion{s: "foo"}}
    {:ok, user: user}
  end

  bench "serialize union" do
    for _ <- 1..1000 do
      User.BinaryProtocol.serialize(bench_context[:user])
    end
    :ok
  end
end
