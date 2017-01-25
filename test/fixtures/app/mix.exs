defmodule App.Mixfile do
  use Mix.Project

  def project do
    [app: :app,
     version: "1.0.0",
     thrift: [files: Path.wildcard("thrift/**/*.thrift")]]
  end
end
