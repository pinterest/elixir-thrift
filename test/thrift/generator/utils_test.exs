defmodule Thrift.Generator.UtilsTest do
  use ExUnit.Case, async: true
  import Thrift.Generator.Utils

  defmacro check(input, expected_output) do
    input_source = optimize_iolist(input) |> Macro.to_string
    expected_output_source = expected_output |> Macro.to_string
    assert input_source == expected_output_source
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
