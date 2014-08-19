
-module(profile_manager_integration_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

before_test() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [
        {lager_file_backend, [{file, "../../../test_log/profile_manager_integration_tests.log"}]}
    ]),
    ok = application:set_env(lager, error_logger_hwm, 1000),
    ok = application:start(lager),
    ok = application:start(folsom),
    ok = application:start(gproc),
    ok = database:start(),
    ok = database:reinitialize(),
    ok = application:start(game_lobby),
    ok = application:start(ranch),
    ok = application:load(game_server),
    ok = application:set_env(game_server, profile, database),
    ok = application:start(game_server).


register_test_() ->
    {setup,
        fun() ->
            {ok, FirstEmulator} = client_emulator:start("localhost", 7890),
            FirstEmulator
        end,
        fun(Emulator) ->
            client_emulator:stop(Emulator)
        end,
        fun(Emulator) ->
            ok = client_emulator:register(Emulator, <<"user1">>, <<"qwerty">>, <<"somewho@example.com">>),
            RegisterOk = lobby_utils:wait_from_pid(Emulator, 1000),
            Profile = lobby_utils:wait_from_pid(Emulator, 1000),

            {ok, AnotherEmulator} = client_emulator:start("localhost", 7890),
            ok = client_emulator:login(AnotherEmulator, <<"user1">>, <<"qwerty">>),
            LoginOk = lobby_utils:wait_from_pid(AnotherEmulator, 1000),
            Profile2 = lobby_utils:wait_from_pid(AnotherEmulator, 1000),

            ok = client_emulator:stop(AnotherEmulator),

            [
                ?_assertEqual({ok, {register, ok}}, RegisterOk),
                ?_assertMatch({ok, _}, Profile),
                ?_assertEqual(Profile, Profile2),
                ?_assertEqual({ok, {login, ok}}, LoginOk)
            ]
        end
    }.

already_registered_test_() ->
    {setup,
        fun() ->
            {ok, FirstEmulator} = client_emulator:start("localhost", 7890),
            FirstEmulator
        end,
        fun(Emulator) ->
            client_emulator:stop(Emulator)
        end,
        fun(Emulator) ->
            ok = client_emulator:register(Emulator, <<"user2">>, <<"qwerty">>, <<"somewho@example.com">>),
            {ok, _} = lobby_utils:wait_from_pid(Emulator, 1000),
            {ok, _} = lobby_utils:wait_from_pid(Emulator, 1000),

            {ok, AnotherEmulator} = client_emulator:start("localhost", 7890),
            ok = client_emulator:register(AnotherEmulator, <<"user2">>, <<"qwerty">>, <<"another@email.com">>),
            RegisterFail = lobby_utils:wait_from_pid(AnotherEmulator, 1000),
            NoProfile = lobby_utils:wait_from_pid(AnotherEmulator, 1000),
            ok = client_emulator:stop(AnotherEmulator),

            [
                ?_assertEqual({ok, {register, incorrect_login}}, RegisterFail),
                ?_assertEqual({error, timeout}, NoProfile)
            ]
        end
    }.

update_profile_test_() ->
    {setup,
        fun() ->
            {ok, FirstEmulator} = client_emulator:start("localhost", 7890),
            FirstEmulator
        end,
        fun(Emulator) ->
            client_emulator:stop(Emulator)
        end,
        fun(Emulator) ->
            ok = client_emulator:register(Emulator, <<"user3">>, <<"qwerty">>, <<"somewho@example.com">>),
            {ok, {register, ok}} = lobby_utils:wait_from_pid(Emulator, 1000),
            {ok, {profile, RawProfile}} = lobby_utils:wait_from_pid(Emulator, 1000),
            Profile = orddict:from_list(RawProfile),
            UpdatedProfile1 = orddict:store(<<"rank">>, 2, Profile),
            UpdatedProfile2 = orddict:store(<<"achievements">>, [1,2,3,4,5,6,7,8], UpdatedProfile1),
            UpdatedProfile = orddict:store(<<"score">>, 5, UpdatedProfile2),

            ok = client_emulator:update_profile(Emulator, UpdatedProfile),

            Res = lobby_utils:wait_from_pid(Emulator, 1000),
            [
                ?_assertMatch({ok, {profile, _}}, Res)
            ]
        end
    }.



after_test() ->
    ok = application:stop(game_server),
    ok = application:unload(game_server),
    ok = application:stop(ranch),
    ok = application:stop(game_lobby),
    ok = database:stop(),
    ok = application:stop(gproc),
    ok = application:stop(folsom),
    ok = application:stop(lager),
    ok = application:unload(lager),
    ok = application:stop(goldrush),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler).
