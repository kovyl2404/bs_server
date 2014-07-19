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


before_test() ->
    error_logger:tty(false).

basic_create_lookup_test_disabled() ->
    fixture(
        fun(_) ->
            SaveResult =
                database:create_profile(
                    <<"kovyl2404">>, <<"password">>, []
                ),
            SucceededLookupResult = database:lookup_profile(<<"kovyl2404">>),
            FailedLookupResult = database:lookup_profile(<<"somewho">>),

            [
                ?_assertEqual(ok, SaveResult),
                ?_assertMatch({ok, _}, SucceededLookupResult),
                ?_assertMatch({error, not_found}, FailedLookupResult)
            ]
        end
    ).

lookup_test_disabled() ->
    Id = <<"kovyl2404">>,
    Password = <<"password">>,
    fixture(
        fun(_) ->
            ok =
                database:create_profile(
                    Id, Password, [
                        {<<"some_field">>, 123},
                        {<<"another_meaningless_field">>, <<"data">>}
                    ]
                ),
            {ok, Profile} = database:lookup_profile(<<"kovyl2404">>),

            [
                ?_assertEqual(Id, proplists:get_value(<<"id">>, Profile)),
                ?_assertEqual(123, proplists:get_value(<<"some_field">>, Profile)),
                ?_assertEqual(<<"data">>, proplists:get_value(<<"another_meaningless_field">>, Profile))
            ]
        end
    ).




ensure_db_absent(Host, Database) ->
    Server = couchbeam:server_connection(Host),
    couchbeam:delete_db(Server, Database),
    ok.

after_test() ->
    error_logger:tty(true).