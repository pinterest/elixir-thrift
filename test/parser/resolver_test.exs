defmodule ResolverTest do
  use ExUnit.Case

  alias Thrift.Parser.FileGroup
  alias Thrift.Parser.Models.{
    Field,
    TEnum,
    Service,
    Struct,
    Union
  }

  use ThriftTestHelpers

  test "it should be able resolve Struct Refs and fields" do
    with_thrift_files([
      "core/shared.thrift": """
      struct User {
        1: i64 id,
        2: string username,
        3: string email
      }
      """,
      "utils.thrift": """
       include "core/shared.thrift"
       service Users {
         shared.User find_by_id(1: i64 user_id);
         void delete_user(1: shared.User user);
       }
       """, as: :file_group, parse: "utils.thrift"]) do

      service = file_group.schemas["utils"].services[:"Users"]
      return_ref = service.functions[:find_by_id].return_type

      refute is_nil(return_ref)

      resolved_struct = FileGroup.resolve(file_group, return_ref)

      assert resolved_struct == file_group.schemas["shared"].structs[:User]

      [field] = service.functions[:delete_user].params
      resolved = FileGroup.resolve(file_group, field)

      assert %Field{} = resolved
      assert resolved.type == file_group.schemas["shared"].structs[:User]
    end
  end

  test "resolving non-resolvable types is a no-op" do
    with_thrift_files([
      "utils.thrift": """
       service NoOp {
       }
       """, as: :file_group, parse: "utils.thrift"]) do

      assert 43 == FileGroup.resolve(file_group, 43)
      assert [1, 2, 3] == FileGroup.resolve(file_group, [1, 2, 3])
    end
  end

  test "it should be able to resolve services" do

    with_thrift_files(
      "core/shared.thrift": """
      service Shared {
        bool get_shared(1: i64 id);
      }
      """,

      "extendo.thrift": """
      include "core/shared.thrift"

      service Extend extends shared.Shared {
        i64 get_extendo_value();
      }
      """, parse: "extendo.thrift") do

      shared = FileGroup.resolve(file_group, :"shared.Shared")

      assert %Service{} = shared
      assert :get_shared in Map.keys(shared.functions)

      extendo = FileGroup.resolve(file_group, :"extendo.Extend")
      assert %Service{} = extendo
      assert :get_extendo_value in Map.keys(extendo.functions)
    end

  end

  test "it should handle following includes through several files" do
    with_thrift_files(
      "core/states.thrift": """
      enum UserState {
        ACTIVE,
        LAPSED,
        DISABLED
      }
      """,

      "core/models.thrift": """
      include "states.thrift"

      exception UserNotFound {
       1: i64 user_id;
      }

      struct User {
        1: i64 user_id,
        2: string username,
        3: string first_name,
        4: string last_name,
        5: states.UserState state;
      }
      """,

      "user_service/user_service.thrift": """
      include "../core/models.thrift"
      service UserService {
        models.User get_by_id(1: i64 user_id) throws (1: models.UserNotFound unf),
        void set_username(1: models.User user, 2: string username);
      }
      """, parse: "user_service/user_service.thrift") do

      user_state = FileGroup.resolve(file_group, :"states.UserState")

      assert %TEnum{values: [ACTIVE: 1, LAPSED: 2, DISABLED: 3]} = user_state
      assert user_state.name == :"states.UserState"
    end

  end

  test "it should be able to resolve complex includes" do
    with_thrift_files(
      "includes/enums.thrift": """
      enum JobStatus {
        STOPPED,
        RUNNING,
        FAILED
      }
      """,

      "includes/unions.thrift": """
      include "structs.thrift"

      union JobSubject {
        1: structs.User user,
        2: structs.System sys;
      }
      """,

      "includes/exceptions.thrift": """
      """,

      "includes/structs.thrift": """
      struct User {
        1: i64 id,
        2: string username,
        3: string first_name,
        4: string last_name;
      }

      struct System {
        1: string name,
        2: string hostname
      }
      """,

      "job_service.thrift": """
      include "includes/unions.thrift"
      include "includes/enums.thrift"
      include "includes/structs.thrift"

      struct Job {
        1: i64 job_id,
        2: unions.JobSubject subject,
        3: structs.User requester;
      }

      service JobService {
        i64 submit(1: Job job),
        enums.JobStatus get_status(1: i64 job_id),
        boolean cancel(1: i64 job_id);
      }

      """, parse: "job_service.thrift") do

      job = FileGroup.resolve(file_group, :"job_service.Job")

      assert %Struct{name: :"job_service.Job"} = job
      assert %Struct{name: :"structs.User"} = FileGroup.resolve(file_group, :"structs.User")
      assert %Union{} = FileGroup.resolve(file_group, :"unions.JobSubject")
      assert %TEnum{} = FileGroup.resolve(file_group, :"enums.JobStatus")
    end
  end


  test "name collisions between types in the same thrift file" do
    assert_raise RuntimeError, "Name collision: dupes.Foo", fn ->
      with_thrift_files(
        "dupes.thrift": """
        struct Foo {}
        service Foo {}
        """, parse: "dupes.thrift") do

        _ = file_group
      end
    end
  end
end
