use Mix.Config

config :calculator,
  port: 9090

config :calculator, Calculator.Generated.HTTP.Client,
  endpoint: "http://localhost:8080/TCalculatorServlet/Calculator"

config :calculator, CommonDataStream.Generated.HTTP.Client,
  endpoint: "http://192.168.100.168:8080/CommonDataInjectionStreamer/CommonTagServletBin"
