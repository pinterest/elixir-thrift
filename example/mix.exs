defmodule Calculator.MixProject do
  use Mix.Project

  def project do
    [
      app: :calculator,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      deps_path: "../deps",
      lockfile: "../mix.lock",

      # Thrift configuration
      compilers: [:thrift | Mix.compilers()],
      thrift: [
        files: Path.wildcard("thrift/*.thrift"),
        output_path: "lib/"
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Calculator.Application, []}
    ]
  end

  defp deps do
    [
      # Note: you will want to replace the next line to get elixir-thrift from either
      # Hex or GitHub in your own project.
      {:thrift, path: ".."}
    ]
  end
end
