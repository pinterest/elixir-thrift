#@namespace elixir Calculator.Generated

exception DivideByZeroError {
  1: string message
}

struct Vector {
  1: double x = 0.0,
  2: double y = 0.0,
  3: double z = 0.0,
}

enum VectorProductType {
  DOT_PRODUCT = 1,
  CROSS_PRODUCT = 2,
}

union VectorProductResult {
  1: double scalar,
  2: Vector vector,
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

  # Can perform dot products and cross products of vectors
  VectorProductResult vectorProduct(1: Vector left, 2: Vector right, 3: VectorProductType type)
}
