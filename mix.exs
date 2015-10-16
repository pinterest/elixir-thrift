defmodule Thrift.Mixfile do
  use Mix.Project

  def project do
    [app: :thrift,
     version: "1.0.0",
     description: "Thrift utilities for Elixir",
     elixir: "~> 1.0",
     erlc_paths: ["ext/thrift/lib/erl/src"],
     erlc_include_path: "ext/thrift/lib/erl/include",
     compilers: [:erlang, :elixir, :app]]
  end

  def application do
    []
  end
end
