defmodule Thrift.Mixfile do
  use Mix.Project

  @description """
  Elixir implementation of the Thrift service framework

  This package includes support for parsing Thrift IDL files, working with the
  Thrift binary protocol, and building high-performance clients and servers.
  """

  @version "2.0.0-dev"
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
       "bench": :test,
       "coveralls": :test,
       "coveralls.detail": :test,
       "coveralls.html": :test,
       "coveralls.post": :test],

     # URLs
     source_url: @project_url,
     homepage_url: @project_url,

     # Hex
     description: @description,
     package: package(),

     # Dialyzer
     dialyzer: [plt_add_deps: :transitive, plt_add_apps: [:ex_unit, :mix]],

     # Docs
     name: "Thrift",
     docs: [
       main: "readme",
       extras: ["README.md": [group: "Documents", title: "README"]],
       extra_section: "Overview",
       source_ref: "thrift_tng",
       source_url: @project_url]]
  end

  def application do
    [
      applications: [:logger, :connection],
    ]
  end

  defp erlc_paths(:test), do: ["src", "ext/thrift/lib/erl/src", "test/support/src"]
  defp erlc_paths(_),     do: ["src", "ext/thrift/lib/erl/src"]

  defp elixirc_paths(:test), do: ["lib", "test/support/lib"]
  defp elixirc_paths(_),     do: ["lib"]

  defp deps do
     [{:ex_doc, "~> 0.14", only: :dev},
      {:excoveralls, "~> 0.5.7", only: [:dev, :test]},
      {:credo, "~> 0.5", only: [:dev, :test]},
      {:dialyxir, "~> 0.4", only: :dev, runtime: false},
      {:benchfella, "~> 0.3", only: [:dev, :test]},
      {:connection, "~> 1.0"},
     ]
  end

  defp package do
     [maintainers: ["Jon Parise", "Steve Cohen", "Preston Guillory"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => @project_url},
      files: ~w(README.md LICENSE mix.exs lib) ++
             ~w(ext/thrift/CHANGES ext/thrift/LICENSE ext/thrift/NOTICE) ++
             ~w(ext/thrift/README.md ext/thrift/doc ext/thrift/lib/erl) ++
             ~w(src/thrift_lexer.xrl src/thrift_parser.yrl)
     ]
  end
end
