-module(database_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

-define(COUCH_HOST, "http://localhost:5984/").
-define(COUCH_DB, "test").


before_test() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [
        {lager_file_backend, [{file, "../../../test_log/database_tests.log"}]}
    ]),
    ok = application:set_env(lager, error_logger_hwm, 1000),
    ok = application:start(lager).

fixture(Inst) ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        Inst
    }.


setup() ->
    ok = application:start(folsom),
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
    ok = couchbeam:stop(),
    ok = application:stop(folsom).



no_server_test_() ->
    ok = application:start(folsom),
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
    FailRegister = database:register(<<"user">>, <<"123">>, <<"somewho@example.com">>),
    FailLogin = database:register(<<"user">>, <<"123">>, <<"another@example.com">>),
    FailUpdate = database:update_profile(<<"used">>, []),
    StopResult = application:stop(database),


    ok = application:unload(database),
    ok = database:stop_deps(),
    ok = application:stop(folsom),
    [
        ?_assertMatch(ok, StartResult),
        ?_assertMatch(ok, StopResult),
        ?_assertEqual({error, econnrefused}, FailLogin),
        ?_assertEqual({error, econnrefused}, FailUpdate),
        ?_assertEqual({error, econnrefused}, FailRegister)
    ].

createdb_on_start_test_() ->
    ok = application:start(folsom),
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
    ok = application:stop(folsom),
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
            RegisterResult = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>, <<"somewho@example.com">>)),
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
            {ok, _} = database:register(<<"kovyl">>, <<"qwerty">>, <<"somewho@example.com">>),
            RegisterAgain = database:register(<<"kovyl">>, <<"ytrewq">>, <<"somewho@example.com">>),
            [
                ?_assertEqual({error, already_registered}, RegisterAgain)
            ]
        end
    ).

login_invalid_password_test_() ->
    fixture(
        fun(_) ->
            {ok, _} = database:register(<<"kovyl">>, <<"qwerty">>, <<"somewho@example.com">>),
            RegisterAgain = database:login(<<"kovyl">>, <<"ytrewq">>),
            [
                ?_assertEqual({error, incorrect_password}, RegisterAgain)
            ]
        end
    ).

get_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, Profile} = maybe_sort_list(database:register(<<"kovyl">>, <<"qwerty">>, <<"somewho@example.com">>)),
            GetProfile = maybe_sort_list(database:get_by_id(<<"kovyl">>)),
            [
                ?_assertEqual({ok, Profile}, GetProfile)
            ]
        end
    ).


update_profile_test_() ->
    fixture(
        fun(_) ->
            {ok, _} = database:register(<<"user1">>, <<"qwerty">>, <<"somewho@example.com">>),
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
                ?_assertEqual({error, incorrect_password}, PasswordChanged),
                ?_assertMatch({ok, _}, PasswordChangeIgnored),
                ?_assertEqual(UpdatedProfile, ActualProfile),
                ?_assertEqual(10, proplists:get_value(<<"rank">>, ActualProfile)),
                ?_assertEqual([1,2,3,4,5,6,7,8], proplists:get_value(<<"achievements">>, ActualProfile)),
                ?_assertEqual(100500, proplists:get_value(<<"score">>, ActualProfile))
            ]
        end
    ).

get_top_test_() ->
    fixture(
        fun(_) ->

            database:register(<<"user1">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user2">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user3">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user4">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user5">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user6">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user7">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user8">>, <<"qwerty">>, <<"somewho@example.com">>),
            database:register(<<"user9">>, <<"qwerty">>, <<"somewho@example.com">>),

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

after_test() ->
    ok = application:stop(lager),
    ok = application:unload(lager),
    ok = application:stop(goldrush),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler).

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