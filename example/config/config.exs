use Mix.Config

config :calculator,
  port: 9090

config :calculator, Calculator.Generated.HTTP.Client,
  endpoint: "http://localhost:8080/TCalculatorServlet/Calculator"
