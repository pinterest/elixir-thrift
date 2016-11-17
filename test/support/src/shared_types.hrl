-ifndef(_shared_types_included).
-define(_shared_types_included, yeah).

%% struct 'SharedStruct'

-record('SharedStruct', {'key' :: integer(),
                         'value' :: string() | binary()}).
-type 'SharedStruct'() :: #'SharedStruct'{}.

%% struct 'SharedException'

-record('SharedException', {'message' :: string() | binary(),
                            'code' :: integer()}).
-type 'SharedException'() :: #'SharedException'{}.

-endif.
