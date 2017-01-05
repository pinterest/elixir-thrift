include "shared.thrift"

struct User {
  1: bool is_evil,
  2: i64 user_id,
  3: i32 number_of_hairs_on_head,
  4: byte amount_of_red,
  5: i16 nineties_era_color,
  6: double mint_gum,
  7: string username,
  8: list<User> friends,
  9: map<byte, string> my_map,
  10: set<i32> blocked_user_ids,
  11: optional list<i32> optional_integers,
}

struct Nesting {
  1: User user,
  2: shared.SharedStruct nested
}


service SimpleService {
  User echo_user(1: User user);
  bool ping();
}
