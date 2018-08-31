# Example

The [`example/`][ex] directory of elixir-thrift contains an implementation of a simple calculator service. It serves two purposes:

1. How to use this library.
2. Example of what elixir-thrift generated code looks like.

[ex]: https://github.com/pinterest/elixir-thrift/tree/master/example

## Calculator Service

The service is very simple, and implements the four basic arithmetic operations: addition, subtraction, multiplication, and division. It demonstrates [how to add a Thrift server to a supervision tree](supervisor), and [how to write a handler](handler) for a [service defined in Thrift](thrift-defs). It also demonstrates how to use exceptions by implementing a division-by-zero exception.

For client usage, you should consult the [test cases](tests) to see how to make requests to a Thrift service.

[handler]: https://github.com/pinterest/elixir-thrift/example/lib/calculator/service_handler.ex
[supervisor]: https://github.com/pinterest/elixir-thrift/example/lib/calculator/application.ex
[tests]: https://github.com/pinterest/elixir-thrift/example/test/calculator_test.exs
[thrift-defs]: https://github.com/pinterest/elixir-thrift/example/thrift/calculator.thrift
