
-module(password_recovery_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").

before_test() ->
    ok = application:start(folsom).

guest_recovery_test_() ->
    {setup,
        fun() ->
            ok = meck:new(password_manager),
            ok = meck:new(database),
            ok = meck:new(mock_session_writer),
            ok = application:load(game_server),
            ok = application:set_env(game_server, profile, database)
        end,
        fun(_) ->
            ok = application:unload(game_server),
            ok = meck:unload(mock_session_writer),
            ok = meck:unload(database),
            ok = meck:unload(password_manager)
        end,
        fun(_) ->
            {ok, guest, State} = client_session:init({mock, mock_session_writer, test}),
            ResetPasswordRequest =
                iolist_to_binary([
                    ?RESET_PASSWORD_REQUEST_TAG,
                    session_utils:encode_password_reset_request(<<"login">>)
                ]),
            MockProfile = [
                {<<"email">>, <<"some@example.com">>},
                {<<"login">>, <<"somewho">>}
            ],
            ok = meck:expect( database, get_by_id, 1, {ok, MockProfile} ),
            ok = meck:expect( password_manager, request, 1, meck:val(ok)),
            ok = meck:expect( mock_session_writer, send, 2, meck:val(ok)),

            {next_state, NewStateName, _NewState} = client_session:guest({data, ResetPasswordRequest}, State),

            ResetRequested = meck:called(password_manager, request, [MockProfile]),
            ResponseSent = meck:called(mock_session_writer, send, [mock, '_']),

            [
                ?_assertEqual(true, ResetRequested),
                ?_assertEqual(true, ResponseSent),
                ?_assertEqual(guest, NewStateName)
            ]
        end
    }.

idle_recovery_test_() ->
    {setup,
        fun() ->
            ok = meck:new(password_manager),
            ok = meck:new(database),
            ok = meck:new(mock_session_writer),
            ok = application:load(game_server),
            ok = application:set_env(game_server, profile, database)
        end,
        fun(_) ->
            ok = application:unload(game_server),
            ok = meck:unload(mock_session_writer),
            ok = meck:unload(database),
            ok = meck:unload(password_manager)
        end,
        fun(_) ->
            {ok, guest, State} = client_session:init({mock, mock_session_writer, test}),
            ResetPasswordRequest =
                iolist_to_binary([
                    ?RESET_PASSWORD_REQUEST_TAG,
                    session_utils:encode_password_reset_request(<<"login">>)
                ]),
            MockProfile = [
                {<<"email">>, <<"some@example.com">>},
                {<<"login">>, <<"somewho">>}
            ],
            ok = meck:expect( database, get_by_id, 1, {ok, MockProfile} ),
            ok = meck:expect( password_manager, request, 1, meck:val(ok)),
            ok = meck:expect( mock_session_writer, send, 2, meck:val(ok)),

            {next_state, NewStateName, _NewState} = client_session:idle({data, ResetPasswordRequest}, State),

            ResetRequested = meck:called(password_manager, request, [MockProfile]),
            ResponseSent = meck:called(mock_session_writer, send, [mock, '_']),

            [
                ?_assertEqual(true, ResetRequested),
                ?_assertEqual(true, ResponseSent),
                ?_assertEqual(idle, NewStateName)
            ]
        end
    }.

after_test() ->
    ok = application:stop(folsom).