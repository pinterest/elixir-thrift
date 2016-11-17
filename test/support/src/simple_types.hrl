-ifndef(_simple_types_included).
-define(_simple_types_included, yeah).
-include("shared_types.hrl").


%% struct 'User'

-record('User', {'is_evil' :: boolean(),
                 'user_id' :: integer(),
                 'number_of_hairs_on_head' :: integer(),
                 'amount_of_red' :: integer(),
                 'nineties_era_color' :: integer(),
                 'mint_gum' :: float(),
                 'username' :: string() | binary(),
                 'friends' :: list(),
                 'my_map' :: dict:dict(),
                 'blocked_user_ids' :: sets:set(),
                 'optional_integers' :: list()}).
-type 'User'() :: #'User'{}.

%% struct 'Nesting'

-record('Nesting', {'user' :: 'User'(),
                    'nested' :: 'SharedStruct'()}).
-type 'Nesting'() :: #'Nesting'{}.

-endif.
