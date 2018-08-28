defmodule(Calculator.Generated.Service.Handler) do
  @callback(add(left :: Thrift.i64(), right :: Thrift.i64()) :: Thrift.i64())
  @callback(divide(left :: Thrift.i64(), right :: Thrift.i64()) :: Thrift.i64())
  @callback(multiply(left :: Thrift.i64(), right :: Thrift.i64()) :: Thrift.i64())
  @callback(subtract(left :: Thrift.i64(), right :: Thrift.i64()) :: Thrift.i64())
end