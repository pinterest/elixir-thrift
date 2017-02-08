defmodule App.Mixfile do
  use Mix.Project

  def project do
    [app: :app,
     version: "1.0.0",
     thrift: [files: ~w(thrift/StressTest.thrift thrift/ThriftTest.thrift)]]
  end
end
