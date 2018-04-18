defmodule Thrift.Protocol.CompactTest do
  use ExUnit.Case
  alias Thrift.Protocol.Compact
  doctest Compact

  describe "field_header" do
    test "short form when id delta is under 16" do
      assert Compact.field_header({0, 1}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({0, 15}, 3) == <<15::size(4), 3::size(4)>>
    end

    test "negative delta" do
      assert Compact.field_header({0, -1}, 3) == <<3, 1>>
    end

    test "long form when id delta is over 15" do
      assert Compact.field_header({0, 16}, 3) == <<3, 32>>
      assert Compact.field_header({0, 64}, 3) == <<3, 128, 1>>
    end

    test "based on deltas" do
      assert Compact.field_header({0, 1}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({1, 2}, 2) == <<1::size(4), 2::size(4)>>
      assert Compact.field_header({1, 3}, 2) == <<2::size(4), 2::size(4)>>
    end
  end

  test "type id" do
    assert Compact.type_id({:bool, true}) == 1
    assert Compact.type_id({:bool, false}) == 2
  end

  test "deserialize binary" do
    assert Compact.deserialize_binary(<<5, "hello"::binary, 1, 2, 3>>) == {"hello", <<1, 2, 3>>}
    assert Compact.deserialize_binary(<<7, "hêllÕ"::binary, 1, 2, 3>>) == {"hêllÕ", <<1, 2, 3>>}

    assert Compact.deserialize_binary(<<5, "hell"::binary>>) == :error
    # assert Compact.deserialize_binary(<<255>>) == :error # todo
  end

  describe "skip field" do
    test "skip field with longer header" do
      assert Compact.skip_field(<<1, 32, "the rest">>) == "the rest"
      assert Compact.skip_field(<<2, 32, "more">>) == "more"
      assert Compact.skip_field(<<2, 128>>) == :error
    end

    test "skip boolean field" do
      assert Compact.skip_field(<<18, "ping">>) == "ping"
      assert Compact.skip_field(<<17, "pang">>) == "pang"
    end

    test "skip byte field" do
      assert Compact.skip_field(<<19, 255, "etc">>) == "etc"
    end

    test "skip i16 field" do
      assert Compact.skip_field(<<20, 128, 128, 8, "etc">>) == "etc"
    end

    test "skip i32 field" do
      assert Compact.skip_field(<<21, 128, 128, 8, "etc">>) == "etc"
    end

    test "skip i64 field" do
      assert Compact.skip_field(<<22, 128, 128, 8, "etc">>) == "etc"
    end

    test "skip double field" do
      assert Compact.skip_field(<<23, 174, 71, 225, 122, 20, 174, 243, 63, "etc">>) == "etc"
    end

    test "skip binary/string field" do
      assert Compact.skip_field(<<24, 3, "abc", "etc">>) == "etc"

      a_128_times = String.duplicate("a", 128)
      var_int_128 = <<128, 1>>

      assert Compact.skip_field(<<24, var_int_128::binary, a_128_times::binary, "etc">>) == "etc"

      larger_than_actual_field = 7
      assert Compact.skip_field(<<24, larger_than_actual_field, "abc", "etc">>) == :error
    end

    test "skip list with fixed size elements" do
      bool_list_three_elements = <<73, 49, 1, 2, 1, "the_rest">>
      assert Compact.skip_field(bool_list_three_elements) == "the_rest"
      assert Compact.skip_field(<<73, 49, 1, 2>>) == :error

      fifteen_true = String.duplicate(<<1>>, 15)
      bool_list_long_list_header = <<73, 241, 15, fifteen_true::binary, "the_rest">>
      assert Compact.skip_field(bool_list_long_list_header) == "the_rest"

      byte_list_three_elements = <<73, 51, 9, 9, 9, "the rest">>
      assert Compact.skip_field(byte_list_three_elements) == "the rest"
      assert Compact.skip_field(<<73, 51, 1, 2>>) == :error

      double_list_three_elements =
        <<73, 55, 174, 71, 225, 122, 20, 174, 243, 63, 123, 20, 174, 71, 225, 122, 2, 64, 10, 215,
          163, 112, 61, 10, 1, 64, "yada yada">>

      assert Compact.skip_field(double_list_three_elements) == "yada yada"
    end

    test "skip set" do
      byte_set_three_elements = <<58, 51, 1, 2, 3, "more stuff">>
      assert Compact.skip_field(byte_set_three_elements) == "more stuff"

      # set uses the same code as list so we're done
    end

    test "skip list with variable size elements" do
      list_of_three_i32s = <<73, 53, 128, 4, 128, 128, 8, 130, 128, 8, "more things">>
      assert Compact.skip_field(list_of_three_i32s) == "more things"

      list_of_three_i16s = <<73, 52, 128, 4, 128, 128, 8, 130, 1, "stuff">>
      assert Compact.skip_field(list_of_three_i16s) == "stuff"

      list_of_four_strings =
        <<73, 72, 6, "sphinx", 2, "of", 5, "black", 6, "quartz", "judge my vow">>

      assert Compact.skip_field(list_of_four_strings) == "judge my vow"
    end

    test "skip list with variable size elements that is too short" do
      list_of_two_i32s_reporting_that_it_has_3 = <<73, 53, 128, 128, 8, 2>>
      assert Compact.skip_field(list_of_two_i32s_reporting_that_it_has_3) == :error
    end

    test "skip maps with fixed length key and value types" do
      byte_to_byte_map_with_two_elements = <<43, 2, 51, 25, 42, 123, 124, "the rest">>
      assert Compact.skip_field(byte_to_byte_map_with_two_elements) == "the rest"

      byte_to_bool_map_with_one_elements = <<43, 1, 49, 25, 1, "things">>
      assert Compact.skip_field(byte_to_bool_map_with_one_elements) == "things"
    end

    test "skip empty map" do
      empty_map = <<43, 0, "other things">>
      assert Compact.skip_field(empty_map) == "other things"
    end

    test "truncated maps (fixed size)" do
      map_with_no_size = <<43>>
      assert Compact.skip_field(map_with_no_size) == :error

      map_with_no_types = <<43, 1>>
      assert Compact.skip_field(map_with_no_types) == :error

      map_with_wrong_number_of_elements = <<43, 2, 51, 25, 42>>
      assert Compact.skip_field(map_with_wrong_number_of_elements) == :error

      map_with_missing_value = <<43, 1, 51, 25>>
      assert Compact.skip_field(map_with_missing_value) == :error
    end

    test "maps with variable length key and/or value types" do
      string_to_string_map_2_elements =
        <<43, 2, 136, 5, "hello", 5, "matey", 6, "watcha", 6, "sailor", "and the rest">>

      assert Compact.skip_field(string_to_string_map_2_elements) == "and the rest"

      string_to_bool_map_2_elements = <<43, 1, 129, 10, "accounting", 1, "things">>
      assert Compact.skip_field(string_to_bool_map_2_elements) == "things"

      bool_to_string_map_2_elements = <<43, 1, 24, 1, 10, "accounting", "things">>
      assert Compact.skip_field(bool_to_string_map_2_elements) == "things"
    end

    test "truncated maps with variable length elements" do
      string_to_string_map_fewer_elements = <<43, 2, 136, 5, "hello", 5, "matey">>
      assert Compact.skip_field(string_to_string_map_fewer_elements) == :error

      i64_to_i64_map_too_few_elements = <<43, 2, 102, 46, 128, 128, 8>>
      assert Compact.skip_field(i64_to_i64_map_too_few_elements) == :error

      string_to_bool_map_missing_value = <<43, 1, 129, 10, "accounting">>
      assert Compact.skip_field(string_to_bool_map_missing_value) == :error

      bool_to_string_map_missing_value = <<43, 1, 24, 1>>
      assert Compact.skip_field(bool_to_string_map_missing_value) == :error
    end

    test "struct field" do
      empty_struct_field = <<28, 0, "more things">>
      assert Compact.skip_field(empty_struct_field) == "more things"

      struct_with_one_field = <<28, 19, 23, 0, "continues">>
      assert Compact.skip_field(struct_with_one_field) == "continues"

      struct_with_two_fields = <<28, 19, 23, 19, 28, 0, "even more">>
      assert Compact.skip_field(struct_with_two_fields) == "even more"

      truncated_struct = <<28, 19, 23, 19, 28>>
      assert Compact.skip_field(truncated_struct) == :error

      struct_with_missing_terminator = <<28, 19, 23, 19, 28, "it all goes wrong">>
      assert Compact.skip_field(struct_with_missing_terminator) == :error
    end

    test "skip composite fields, lists of lists etc..." do
      map_of_maps = <<27, 1, 187, 1, 51, 91, 92, 2, 51, 93, 94, 95, 96, "maps no more">>
      assert Compact.skip_field(map_of_maps) == "maps no more"

      list_of_maps = <<121, 27, 1, 51, 91, 92, "lists no more">>
      assert Compact.skip_field(list_of_maps) == "lists no more"
    end
  end
end
