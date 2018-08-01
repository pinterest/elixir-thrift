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
      elixir: "~> 1.3",
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
        plt_add_deps: :transitive,
        plt_add_apps: [:mix],
        ignore_warnings: ".dialyzerignore"
      ],

      # Docs
      name: "Thrift",
      docs: [
        main: "Thrift",
        extras: ["CONTRIBUTING.md": [title: "Contributing"]],
        source_url: @project_url,
        groups_for_modules: [
          "Abstract Syntax Tree": ~r"Thrift.AST.*"
        ]
      ]
    ]
  end

  def application do
    [
      applications: [:logger, :connection, :ranch, :ssl]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support/lib"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Development
      {:ex_doc, "~> 0.19", only: :dev, runtime: false},
      {:excoveralls, "~> 0.9", only: [:dev, :test], runtime: false},
      {:credo, "~> 0.10", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false},

      # Runtime
      {:connection, "~> 1.0"},
      {:ranch, "~> 1.5"}
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
        ~w(README.md CONTRIBUTING.md LICENSE mix.exs lib) ++
          ~w(src/thrift_lexer.xrl src/thrift_parser.yrl)
    ]
  end
end
