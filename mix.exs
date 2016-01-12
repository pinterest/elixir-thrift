defmodule Thrift.Mixfile do
  use Mix.Project

  def project do
    [app: :thrift,
     version: "1.0.0",
     description: "Thrift utilities for Elixir",
     elixir: "~> 1.0",
     description: description,
     source_url: project_url,
     homepage_url: project_url,
     package: package,
     deps: deps,
     erlc_paths: ["ext/thrift/lib/erl/src"],
     erlc_include_path: "ext/thrift/lib/erl/include",
     compilers: [:erlang, :elixir, :app],
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.detail": :test, "coveralls.post": :test],
     ]
  end

  def application do
     []
  end

  defp description do
     """
     elixir-thrift contains a set of useful utilities to work with Thrift in Elixir. Erlang Thrift runtime library is embedded.
     """
  end

  defp project_url do
    """
     https://github.com/pinterest/elixir-thrift
     """
  end

  defp deps do
     [{:excoveralls, github: "parroty/excoveralls", tag: "v0.4.3", override: true, only: :test}]
  end

  defp package do
     [files: ["ext", "lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Jon Parise", "Steve Cohen"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => project_url}]
  end
end
