from thrift.protocol import TBinaryProtocol
from thrift.transport import TTransport


def serialize(thrift_obj, filename):
    with open(filename, 'wb') as f:
        transport = TTransport.TFileObjectTransport(f)
        protocol = TBinaryProtocol.TBinaryProtocolAccelerated(transport)
        thrift_obj.write(protocol)
        transport.flush()


def write():
    """
    Writes static test data. The thrift structures created and serialized here
    should match up with the ones in the Elixir test cases.
    """
    from generated.across.ttypes import User
    from generated.containers.ttypes import Containers, Friend, Weather
    from generated.enums.ttypes import Status, StructWithEnum
    from generated.scalars.ttypes import Scalars

    #
    # across.thrift
    #

    # across.thriftbin
    user = User(id=1234, best_friend=Friend(id=3282, username='stinkypants'))
    serialize(user, 'binary/across/across.thriftbin')


    #
    # containers.thrift
    #

    # empty_list.thriftbin
    containers = Containers(users=[])
    serialize(containers, 'binary/containers/empty_list.thriftbin')

    # enums_list.thriftbin
    containers = Containers(weekly_forecast=[
        Weather.SUNNY,
        Weather.SUNNY,
        Weather.SUNNY,
        Weather.SUNNY,
        Weather.CLOUDY,
        Weather.SUNNY,
        Weather.SUNNY
    ])
    serialize(containers, 'binary/containers/enums_list.thriftbin')

    # enums_map.thriftbin
    containers = Containers(user_forecasts={
        1: Weather.SUNNY,
        -1: Weather.SUNNY,
        12345: Weather.CLOUDY
    })
    serialize(containers, 'binary/containers/enums_map.thriftbin')

    # strings_set.thriftbin
    containers = Containers(taken_usernames={'scohen', 'pguillory'})
    serialize(containers, 'binary/containers/strings_set.thriftbin')

    # structs_list.thriftbin
    containers = Containers(friends=[
        Friend(id=1, username='scohen'),
        Friend(id=2, username='pguillory'),
        Friend(id=3, username='dantswain')
    ])
    serialize(containers, 'binary/containers/structs_list.thriftbin')

    # structs_map.thriftbin
    containers = Containers(friends_by_username={
        'scohen': Friend(id=1, username='scohen'),
        'pguillory': Friend(id=2, username='pguillory'),
        'dantswain': Friend(id=3, username='dantswain')
    })
    serialize(containers, 'binary/containers/structs_map.thriftbin')

    # unset.thriftbin
    containers = Containers()
    serialize(containers, 'binary/containers/unset.thriftbin')


    #
    # enums.thrift
    #

    # banned.thriftbin
    struct_with_enum = StructWithEnum(status=Status.BANNED)
    serialize(struct_with_enum, 'binary/enums/banned.thriftbin')

    # evil.thriftbin
    struct_with_enum = StructWithEnum(status=Status.EVIL)
    serialize(struct_with_enum, 'binary/enums/evil.thriftbin')


    #
    # scalars.thrift
    #

    # bool.thriftbin
    scalars = Scalars(is_true=True)
    serialize(scalars, 'binary/scalars/bool.thriftbin')

    # byte.thriftbin
    scalars = Scalars(byte_value=127)
    serialize(scalars, 'binary/scalars/byte.thriftbin')

    # i16.thriftbin
    scalars = Scalars(sixteen_bits=12723)
    serialize(scalars, 'binary/scalars/i16.thriftbin')

    # i32.thriftbin
    scalars = Scalars(thirty_two_bits=18362832)
    serialize(scalars, 'binary/scalars/i32.thriftbin')

    # i64.thriftbin
    scalars = Scalars(sixty_four_bits=8872372)
    serialize(scalars, 'binary/scalars/i64.thriftbin')

    # double.thriftbin
    scalars = Scalars(double_value=2.37219)
    serialize(scalars, 'binary/scalars/double.thriftbin')

    # string.thriftbin
    scalars = Scalars(string_value='I am a string')
    serialize(scalars, 'binary/scalars/string.thriftbin')

    # binary.thriftbin
    scalars = Scalars(raw_binary=b'\xE0\xBA\x02\x01\x00')
    serialize(scalars, 'binary/scalars/binary.thriftbin')
