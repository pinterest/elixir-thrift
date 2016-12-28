defmodule Thrift.Generator.UtilsTest do
  use ThriftTestCase
  import Thrift.Generator.Utils

  defmacro check(input, output) do
    input_source = optimize_iolist(input) |> Macro.to_string
    output_source = output |> Macro.to_string
    assert input_source == output_source
  end

  test "optimize_iolist" do
    check <<0>>,                  <<0>>
    check [<<0>>],                <<0>>
    check [<<1>>, <<2>>],         <<1, 2>>
    check [<<1>>, [<<2>>]],       <<1, 2>>
    check [[<<1>>], <<2>>],       <<1, 2>>
    check [[[[<<1>>]], [<<2>>]]], <<1, 2>>
    check [<<1>>, x, [<<2>>, y]], [<<1>>, x, <<2>> | y]
    check [x, <<1>>, [<<2>>, y]], [x, <<1, 2>> | y]
    check [<<1, 2>>, <<0>>],      <<1, 2, 0>>
    check [<<1, 2>>, "foo"],      <<1, 2, "foo">>
  end
end
