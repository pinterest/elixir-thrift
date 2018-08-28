#@namespace elixir Calculator.Generated

exception DivideByZeroError {
  1: string message
}

service Service {
  # Adds two integers
  i64 add(1: i64 left, 2: i64 right),

  # Subtracts two integers
  i64 subtract(1: i64 left, 2: i64 right),

  # Multiplies two integers
  i64 multiply(1: i64 left, 2: i64 right),

  # Divides two integers, throwing an exception for zero division
  i64 divide(1: i64 left, 2: i64 right) throws (1: DivideByZeroError e),
}
