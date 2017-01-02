defmodule BehaviourTest do
  use ThriftTestCase

  @thrift_file name: "behaviour.thrift", contents: """
  struct S {
    1: string username
  }

  struct T {
    1: i64 id
  }

  union SorT {
    1: S s_value,
    2: T t_value
  }

  enum WaitStates {
    WAITING
    ACCEPTING
    BLOCKED
  }

  service BehaviourService {
    void ping(1: i64 my_int),
    void my_bool(1: bool my_bool),
    void numbers(1: byte b, 2: i16 i, 3: i32 eye32, 4: i64 eye64, 5: double dub),
    void my_set(1: set<string> my_set),
    void my_list(1: list<string> my_string),
    void my_map(1: map<string, string> my_map)
    map<string, bool> my_map2(1: map<string, map<string, string>> my_map)
    void struct_param(1: S my_struct)
    void myCamelCasedFunction(1: string camelParam);
    WaitStates get_state();
    SorT getSorT();
  }
  """

  thrift_test "that behaviour callbacks exist" do
    behaviour_specs = Behaviour.behaviour_info(:callbacks)

    assert {:ping, 1} in behaviour_specs
    assert {:my_bool, 1} in behaviour_specs
    assert {:numbers, 5} in behaviour_specs
    assert {:my_set, 1} in behaviour_specs
    assert {:my_list, 1} in behaviour_specs
    assert {:my_map, 1} in behaviour_specs
    assert {:my_map2, 1} in behaviour_specs
    assert {:struct_param, 1} in behaviour_specs
    assert {:my_camel_cased_function, 1} in behaviour_specs
    assert {:get_state, 0} in behaviour_specs
    assert {:get_sor_t, 0} in behaviour_specs
  end
end
