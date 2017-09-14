defmodule Thrift.Parser.AnnotationTest do
  use ExUnit.Case, async: true
  import Thrift.Parser, only: [parse: 1]

  setup_all do
    {:ok, schema} =
      "test/fixtures/app/thrift/AnnotationTest.thrift"
      |> File.read!
      |> parse
    {:ok, [schema: schema]}
  end

  defp find_field(fields, name) do
    Enum.find(fields, &match?(%{name: ^name}, &1))
  end

  test "enum annotations", context do
    assert %{enums: %{weekdays: enum}} = context[:schema]
    assert enum.annotations == %{:"foo.bar" => "baz"}
  end

  test "struct annotations", context do
    assert %{structs: %{foo: struct}} = context[:schema]
    assert struct.annotations == %{
      :"cpp.type" => "DenseFoo",
      :"python.type" => "DenseFoo",
      :"java.final" => "",
      :"annotation.without.value" => "1"}

    assert bar = find_field(struct.fields, :bar)
    assert bar.annotations == %{:presence => "required"}
    assert baz = find_field(struct.fields, :baz)
    assert baz.annotations == %{:presence => "manual", :"cpp.use_pointer" => ""}
  end

  test "service annotations", context do
    assert %{services: %{foo_service: service}} = context[:schema]
    assert service.annotations == %{:"a.b" => "c"}
  end

  test "function annotations", context do
    assert %{services: %{foo_service: service}} = context[:schema]
    assert %{functions: %{foo: function}} = service
    assert function.annotations == %{:foo => "bar"}
  end

  test "exception annotations", context do
    assert %{exceptions: %{foo_error: exception}} = context[:schema]
    assert exception.annotations == %{:foo => "bar"}
    assert field = find_field(exception.fields, :error_code)
    assert field.annotations == %{:foo => "bar"}
  end
end
