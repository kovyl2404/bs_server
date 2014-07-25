-module(database_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

-define(COUCH_HOST, "http://localhost:5984/").
-define(COUCH_DB, "test").


fixture(Inst) ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        Inst
    }.


setup() ->
    ok = couchbeam:start(),
    ok = ensure_db_absent(?COUCH_HOST, ?COUCH_DB),
    ok = ensure_db_present(?COUCH_HOST, ?COUCH_DB),
    ok = application:load(database),
    ok = application:set_env(database, backend, couchdb_backend),
    ok = application:set_env(
        database, backend_config, [
            {host, ?COUCH_HOST},
            {dbname, ?COUCH_DB}
        ]
    ),
    ok = application:start(database).

cleanup(_) ->
    ok = application:stop(database),
    ok = application:unload(database),
    ok = couchbeam:stop().



no_server_test_() ->
    ok = database:start_deps(),
    ok = application:load(database),
    ok = application:set_env(database, backend, couchdb_backend),
    ok = application:set_env(
        database, backend_config, [
            {host, "http://localhost:1234"},
            {dbname, "test"}
        ]
    ),
    StartResult = application:start(database),
    FailRegister = database:register(<<"user">>, <<"123">>),
    FailLogin = database:register(<<"user">>, <<"123">>),
    FailUpdate = database:update_profile(<<"used">>, []),
    StopResult = application:stop(database),


    ok = application:unload(database),
    ok = database:stop_deps(),
    [
        ?_assertMatch(ok, StartResult),
        ?_assertMatch(ok, StopResult),
        ?_assertEqual({error, econnrefused}, FailLogin),
        ?_assertEqual({error, econnrefused}, FailUpdate),
        ?_assertEqual({error, econnrefused}, FailRegister)
    ].

createdb_on_start_test_() ->
    ok = database:start_deps(),
    ok = ensure_db_absent(?COUCH_HOST, ?COUCH_DB),
    ok = application:load(database),
    ok = application:set_env(database, backend, couchdb_backend),
    ok = application:set_env(
        database, backend_config, [
            {host, ?COUCH_HOST},
            {dbname, ?COUCH_DB}
        ]
    ),
    ok = application:start(database),
    ok = application:stop(database),
    ok = application:unload(database),

    Server = couchbeam:server_connection(?COUCH_HOST),
    DatabaseCreated = couchbeam:db_exists(Server, ?COUCH_DB),
    {ok, Db} = couchbeam:open_db(Server, ?COUCH_DB),
    ViewsExists = couchbeam:doc_exists(Db, <<"_design/users">>),
    ok = database:stop_deps(),

    [
        ?_assert(DatabaseCreated),
        ?_assert(ViewsExists)
    ].

login_no_user_test_() ->
    fixture(
        fun(_) ->
            LoginResult = database:login(<<"some_user">>, <<"some_password">>),
            ?_assertEqual({error, not_found}, LoginResult)
        end
    ).

register_test_() ->
    fixture(
        fun(_) ->
            RegisterResult = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>)),
            LoginResult = maybe_sort_list(database:login(<<"kovyl">>, <<"qwerty">>)),
            [
                ?_assertMatch({ok, _}, RegisterResult),
                ?_assertMatch({ok, _}, LoginResult),
                ?_assertMatch(RegisterResult, LoginResult)
            ]
        end
    ).

register_existed_login_test_() ->
    fixture(
        fun(_) ->
            {ok, _} = database:register(<<"kovyl">>, <<"qwerty">>),
            RegisterAgain = database:register(<<"kovyl">>, <<"ytrewq">>),
            [
                ?_assertEqual({error, already_registered}, RegisterAgain)
            ]
        end
    ).

login_invalid_password_test_() ->
    fixture(
        fun(_) ->
            {ok, _} = database:register(<<"kovyl">>, <<"qwerty">>),
            RegisterAgain = database:login(<<"kovyl">>, <<"ytrewq">>),
            [
                ?_assertEqual({error, not_found}, RegisterAgain)
            ]
        end
    ).

get_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, Profile} = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>)),
            GetProfile = maybe_sort_list(database:get_by_id(<<"kovyl">>)),
            [
                ?_assertEqual({ok, Profile}, GetProfile)
            ]
        end
    ).

set_field_non_existent_profile_test_() ->
    fixture(
        fun(_) ->
            Result = database:set_field(<<"rank">>, 1, <<"kovyl">>),
            [
                ?_assertEqual({error, not_found}, Result)
            ]
        end
    ).

set_field_existent_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, _Profile} = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>)),
            {ok, NewProfile} = maybe_sort_list(database:set_field(<<"rank">>, 1, <<"kovyl">>)),
            GetProfile = maybe_sort_list(database:get_by_id(<<"kovyl">>)),
            NewRank = proplists:get_value(<<"rank">>, NewProfile),
            [
                ?_assertEqual({ok, NewProfile}, GetProfile),
                ?_assertEqual(1, NewRank)
            ]
        end
    ).

increase_field_existent_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, _Profile} = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>)),
            {ok, NewProfile} = maybe_sort_list(database:increase_field(<<"rank">>, <<"kovyl">>)),
            GetProfile = maybe_sort_list(database:get_by_id(<<"kovyl">>)),
            NewRank = proplists:get_value(<<"rank">>, NewProfile),
            [
                ?_assertEqual({ok, NewProfile}, GetProfile),
                ?_assertEqual(1, NewRank)
            ]
        end
    ).


increase_field_non_existent_profile_test_() ->
    fixture(
        fun(_) ->
            IncreaseResult = maybe_sort_list(database:increase_field(<<"rank">>, <<"kovyl">>)),
            GetProfile = maybe_sort_list(database:get_by_id(<<"kovyl">>)),
            [
                ?_assertEqual({error, not_found}, IncreaseResult),
                ?_assertEqual({error, not_found},    GetProfile)
            ]
        end
    ).

update_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, _} = database:register(<<"user1">>, <<"qwerty">>),
            {ok, UpdatedProfile} =
                database:update_profile(
                    [
                        {<<"achievements">>, [1,2,3,4,5,6,7,8]},
                        {<<"rank">>, 10},
                        {<<"password">>, <<"ytrewq">>},
                        {<<"score">>, 100500}
                    ],
                    <<"user1">>
                ),
            PasswordChanged = database:login(<<"user1">>, <<"ytrewq">>),
            PasswordChangeIgnored = database:login(<<"user1">>, <<"qwerty">>),
            {ok, ActualProfile} = database:get_by_id(<<"user1">>),
            [
                ?_assertEqual({error, not_found}, PasswordChanged),
                ?_assertMatch({ok, _}, PasswordChangeIgnored),
                ?_assertEqual(UpdatedProfile, ActualProfile),
                ?_assertEqual(10, proplists:get_value(<<"rank">>, ActualProfile)),
                ?_assertEqual([1,2,3,4,5,6,7,8], proplists:get_value(<<"achievements">>, ActualProfile)),
                ?_assertEqual(0, proplists:get_value(<<"score">>, ActualProfile))
            ]
        end
    ).

get_top_test_() ->
    fixture(
        fun(_) ->

            database:register(<<"user1">>, <<"qwerty">>),
            database:register(<<"user2">>, <<"qwerty">>),
            database:register(<<"user3">>, <<"qwerty">>),
            database:register(<<"user4">>, <<"qwerty">>),
            database:register(<<"user5">>, <<"qwerty">>),
            database:register(<<"user6">>, <<"qwerty">>),
            database:register(<<"user7">>, <<"qwerty">>),
            database:register(<<"user8">>, <<"qwerty">>),
            database:register(<<"user9">>, <<"qwerty">>),

            database:set_field(<<"score">>, 5, <<"user1">>),
            database:set_field(<<"score">>, 3, <<"user2">>),
            database:set_field(<<"score">>, 2, <<"user3">>),
            database:set_field(<<"score">>, 27, <<"user4">>),
            database:set_field(<<"score">>, 8, <<"user5">>),

            Result = database:get_top(4),
            ExpectedTop = [
                {<<"user4">>, 27},
                {<<"user5">>, 8},
                {<<"user1">>, 5},
                {<<"user2">>, 3}
            ],

            ?_assertEqual({ok, ExpectedTop}, Result)
        end
    ).

empty_top_test_() ->
    fixture(
        fun(_) ->
            [
                ?_assertEqual({ok, []}, database:get_top(0))
            ]
        end
    ).



ensure_db_present(Host, Database) ->
    Server = couchbeam:server_connection(Host),
    couchbeam:open_or_create_db(Server, Database),
    ok.

ensure_db_absent(Host, Database) ->
    Server = couchbeam:server_connection(Host),
    couchbeam:delete_db(Server, Database),
    ok.



maybe_sort_list({ok, [_|_] = List}) ->
    {ok, lists:sort(List)};
maybe_sort_list(X) -> X.