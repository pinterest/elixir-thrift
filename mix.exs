defmodule Thrift.Mixfile do
  use Mix.Project

  @version "1.0.0"
  @project_url "https://github.com/pinterest/elixir-thrift"

  def project do
    [app: :thrift,
     version: @version,
     elixir: "~> 1.0",
     deps: deps,

     # Build Environment
     erlc_paths: ["src", "ext/thrift/lib/erl/src"],
     erlc_include_path: "ext/thrift/lib/erl/include",
     compilers: [:leex, :erlang, :elixir, :app],

     # Testing
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.detail": :test, "coveralls.post": :test],

     # URLs
     source_url: @project_url,
     homepage_url: @project_url,

     # Hex
     description: description,
     package: package,
     ]
  end

  def application do
     []
  end

  defp deps do
     [{:excoveralls, github: "parroty/excoveralls", tag: "v0.4.3", override: true, only: :test}]
  end

  defp description do
     """
     A collection of utilities for working with Thrift in Elixir.

     Provides a copy of the Erlang Thrift runtime.
     """
  end

  defp package do
     [maintainers: ["Jon Parise", "Steve Cohen"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => @project_url},
      files: ~w(README.md LICENSE mix.exs ext lib) ++
             ~w(src/thrift_lexer.xrl)]
  end
end
