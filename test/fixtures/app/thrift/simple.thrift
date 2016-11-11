struct Primitives {
}

struct User {
  1: bool is_evil,
  2: i64 user_id,
  3: i32 number_of_hairs_on_head,
  4: byte amount_of_red,
  5: i16 nineties_era_color,
  6: double mint_gum,
  7: string username,
  8: list<User> friends,
  9: map<i8, string> my_map,
  10: set<i32> blocked_user_ids,
  11: optional list<i32> optional_integers,
}

/*
struct TestStruct {
  1: bool bool_field,
  2: byte byte_field,
  3: double double_field,
  4: i16 i_sixteen,
  5: i32 i_thirty_two,
  6: i64 i_sixty_four,
  7: string string_field,
  8: map<string, i16> map_field,
  9: list<User> user_list,
  10: set<i32> number_set,
}
*/
