defmodule Thrift.Mixfile do
  use Mix.Project

  @version "1.3.1"
  @project_url "https://github.com/pinterest/elixir-thrift"

  def project do
    [app: :thrift,
     version: @version,
     elixir: "~> 1.2",
     deps: deps(),

     # Build Environment
     erlc_paths: erlc_paths(Mix.env),
     erlc_include_path: "ext/thrift/lib/erl/include",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:leex, :yecc, :erlang, :elixir, :app],

     # Testing
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: [
       "coveralls": :test,
       "coveralls.detail": :test,
       "coveralls.html": :test,
       "coveralls.post": :test],

     # URLs
     source_url: @project_url,
     homepage_url: @project_url,

     # Hex
     description: description(),
     package: package(),

     # Docs
     name: "Thrift",
     docs: [
       main: "readme",
       extras: ["README.md": [group: "Documents", title: "README"]],
       extra_section: "Overview",
       source_ref: @version,
       source_url: @project_url]]
  end

  def application do
    [
      applications: [:logger],
    ]
  end

  defp erlc_paths(:prod) do
    ["src", "ext/thrift/lib/erl/src"]
  end

  defp erlc_paths(_) do
    erlc_paths(:prod) ++ ["test/support/src"]
  end

  defp elixirc_paths(:prod) do
    ["lib"]
  end

  defp elixirc_paths(_) do
    elixirc_paths(:prod) ++ ["test/support/lib"]
  end

  defp deps do
     [{:ex_doc, "~> 0.14", only: :dev},
      {:excoveralls, "~> 0.5.7", only: [:dev, :test]},
      {:credo, "~> 0.5", only: [:dev, :test]},
      {:dialyxir, "~> 0.4.0", only: [:dev, :test]},
      {:benchfella, "~> 0.3.0", only: [:dev, :test]},
      {:connection, "~> 1.0"},
     ]
  end

  defp description do
     """
     A collection of utilities for working with Thrift in Elixir.

     Provides a copy of the Apache Thrift Erlang runtime.
     """
  end

  defp package do
     [maintainers: ["Jon Parise", "Steve Cohen"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => @project_url},
      files: ~w(README.md LICENSE mix.exs lib) ++
             ~w(ext/thrift/CHANGES ext/thrift/LICENSE ext/thrift/NOTICE) ++
             ~w(ext/thrift/README.md ext/thrift/doc ext/thrift/lib/erl) ++
             ~w(src/thrift_lexer.xrl src/thrift_parser.yrl)
     ]
  end
end
