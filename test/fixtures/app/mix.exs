defmodule App.Mixfile do
  use Mix.Project

  def project do
    [app: :app,
     version: "1.0.0",
     thrift: [
       files: [
         "thrift/StressTest.thrift",
         "thrift/ThriftTest.thrift",
         "thrift/numbers.thrift"
       ]
     ]]
  end
end
