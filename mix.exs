defmodule Thrift.Mixfile do
  @moduledoc false
  use Mix.Project

  @description """
  Elixir implementation of the Thrift service framework

  This package includes support for parsing Thrift IDL files, working with the
  Thrift binary protocol, and building high-performance clients and servers.
  """

  @version "2.0.0-dev"
  @project_url "https://github.com/pinterest/elixir-thrift"

  def project do
    [
      app: :thrift,
      version: @version,
      elixir: "~> 1.5",
      deps: deps(),

      # Build Environment
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:leex, :yecc, :erlang, :elixir, :app],

      # Testing
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.html": :test,
        "coveralls.post": :test
      ],

      # URLs
      source_url: @project_url,
      homepage_url: @project_url,

      # Hex
      description: @description,
      package: package(),

      # Dialyzer
      dialyzer: [
        plt_add_deps: :app_tree,
        plt_add_apps: [:mix]
      ],

      # Docs
      name: "Thrift",
      docs: [
        main: "Thrift",
        extras: ["ADOPTERS.md": [title: "Adopters"], "CONTRIBUTING.md": [title: "Contributing"]],
        source_url: @project_url,
        groups_for_modules: [
          "Abstract Syntax Tree": ~r"Thrift.AST.*",
          Clients: ["Thrift.Binary.Framed.Client"],
          Servers: ["Thrift.Binary.Framed.Server"]
        ]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support/lib"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Development
      {:ex_doc, "~> 0.19", only: :dev, runtime: false},
      {:excoveralls, "~> 0.9", only: :test, runtime: false},
      {:credo, "~> 0.10", only: :dev, runtime: false},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false},

      # Runtime
      {:connection, "~> 1.0"},
      {:ranch, "~> 1.6"}
    ]
  end

  defp package do
    [
      maintainers: [
        "Steve Cohen",
        "James Fish",
        "Preston Guillory",
        "Michael Oliver",
        "Jon Parise",
        "Dan Swain"
      ],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => @project_url},
      files:
        ~w(README.md ADOPTERS.md CONTRIBUTING.md LICENSE mix.exs lib) ++
          ~w(src/thrift_lexer.xrl src/thrift_parser.yrl)
    ]
  end
end
