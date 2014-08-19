
-module(client_session_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_lobby/include/common.hrl").
-include_lib("game_server/include/client_protocol.hrl").

fixture(Inst) ->
    {setup,
        fun() ->
            ok = application:start(folsom),
            ok = game_lobby:start(),
            ok = application:start(gproc),
            ok = meck:new(mock_session_writer, [passthrough]),
            ok = application:load(game_server),
            flush_messages()
        end,
        fun(_) ->
            ok = application:unload(game_server),
            ok = meck:unload(mock_session_writer),
            ok = game_lobby:stop(),
            ok = application:stop(gproc),
            ok = application:stop(folsom)
        end,
        Inst
    }.

flush_messages() ->
    receive
        _ -> flush_messages()
    after
        0 -> ok
    end.


before_test() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [
        {lager_file_backend, [{file, "../../../test_log/client_sessions_tests.log"}]}
    ]),
    ok = application:set_env(lager, error_logger_hwm, 1000),
    ok = application:start(lager).

start_game_no_auth_test_() ->
    fixture(
        fun(_) ->
            ok = application:set_env(game_server, profile, database),
            TestHost = self(),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),

            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 1000),

            [
                ?_assertEqual({ok, not_auth}, SessionDown)
            ]

        end
    ).

register_test_() ->
    Profile = [
        {<<"rank">>, 1},
        {<<"experience">>, 2},
        {<<"reserved1">>, 1},
        {<<"reserved2">>, 2},
        {<<"reserved3">>, 3},
        {<<"reserved4">>, 5},
        {<<"reserved5">>, 5},
        {<<"reserved6">>, 6},
        {<<"reserved7">>, 7},
        {<<"score">>, 100},
        {<<"achievements">>, [1,2,3,4,5,6,7,8]},
        {<<"timestamp">>, 12345},
        {<<"email">>, <<"somewho@example.net">>}
    ],
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, register,
                    fun(Login, Password, Email) ->
                        TestHost ! {self(), {register, Login, Password, Email}},
                        {ok, Profile}
                    end
                ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest =
                iolist_to_binary([
                    <<?REGISTER_TAG>>,
                    session_utils:encode_register_request(<<"login">>, <<"password">>, <<"somewho@example.net">>)
                ]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionDown = lobby_utils:wait_process_down(MonitorRef, 100),
            RegisterInDatabase = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RegisterOkResponse = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = meck:unload(database),

            EncodedProfile = session_utils:encode_profile_request(Profile),
            ExpectedProfileFrame = <<?PROFILE_TAG, EncodedProfile/binary>>,

            [
                ?_assertEqual({error, timeout}, SessionDown),
                ?_assertEqual({ok, {register, <<"login">>, <<"password">>, <<"somewho@example.net">>}}, RegisterInDatabase),
                ?_assertEqual({ok, [
                    {data, <<?REGISTER_TAG, 0>>}, {data, ExpectedProfileFrame}
                ]}, RegisterOkResponse)
            ]

        end
    ).

invalid_register_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, register,
                    fun(Login, Password, Email) ->
                        TestHost ! {self(), {register, Login, Password, Email}},
                        {error, already_registered}
                    end
                ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest = iolist_to_binary([
                <<?REGISTER_TAG>>,
                session_utils:encode_register_request(<<"login">>, <<"password">>, <<"somewho@example.net">>)
            ]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionAlive = lobby_utils:wait_process_down(MonitorRef, 100),
            RegisterInDatabase = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RegisterOkResponse = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 1000),
            ok = meck:unload(database),
            [
                ?_assertEqual({error, timeout}, SessionAlive),
                ?_assertEqual({ok, {register, <<"login">>, <<"password">>, <<"somewho@example.net">>}}, RegisterInDatabase),
                ?_assertEqual({ok, [{data, <<?REGISTER_TAG, 1>>}]}, RegisterOkResponse),
                ?_assertEqual({ok, not_auth}, SessionDown)
            ]

        end
    ).

register_in_idle_state_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, undefined),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),


            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest = iolist_to_binary([
                <<?REGISTER_TAG>>, session_utils:encode_register_request(<<"login">>, <<"password">>, <<"somewho@example.net">>)
            ]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 100),

            [
                ?_assertEqual({ok, protocol_violation}, SessionDown)
            ]

        end
    ).

request_top_test_() ->
    Top = [
        {<<"user1">>, 10},
        {<<"user2">>, 5},
        {<<"user5">>, 3},
        {<<"user3">>, 2},
        {<<"user4">>, 1}
    ],
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, get_top,
                    fun(Count) ->
                        TestHost ! {self(), {get_top, Count}},
                        {ok, Top}
                    end
                ),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            TopRequest = iolist_to_binary(<<?TOP_TAG, 5>>),
            ok = client_session:send_command( RemoteClientPid, {data, TopRequest} ),

            GetTop = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ok = meck:unload(database),
            Res = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ExpectedTopData = iolist_to_binary(session_utils:encode_top_response(Top)),
            [
                ?_assertEqual({ok, {get_top, 5}}, GetTop),
                ?_assertEqual({ok, [{data, <<?TOP_TAG, ExpectedTopData/binary>>}]}, Res)
            ]
        end
    ).

request_top_few_data_test_() ->
    Top = [
        {<<"user1">>, 10},
        {<<"user2">>, 5},
        {<<"user5">>, 3},
        {<<"user3">>, 2},
        {<<"user4">>, 1}
    ],
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, get_top,
                    fun(Count) ->
                        TestHost ! {self(), {get_top, Count}},
                        {ok, Top}
                    end
                ),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            TopRequest = iolist_to_binary(<<?TOP_TAG, 10>>),
            ok = client_session:send_command( RemoteClientPid, {data, TopRequest} ),

            GetTop = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ok = meck:unload(database),
            Res = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ExpectedTopData = iolist_to_binary(session_utils:encode_top_response(Top)),
            [
                ?_assertEqual({ok, {get_top, 10}}, GetTop),
                ?_assertEqual({ok, [{data, <<?TOP_TAG, ExpectedTopData/binary>>}]}, Res)
            ]
        end
    ).

login_test_() ->
    Profile = [
        {<<"rank">>, 1},
        {<<"experience">>, 2},
        {<<"reserved1">>, 1},
        {<<"reserved2">>, 2},
        {<<"reserved3">>, 3},
        {<<"reserved4">>, 5},
        {<<"reserved5">>, 5},
        {<<"reserved6">>, 6},
        {<<"reserved7">>, 7},
        {<<"score">>, 100},
        {<<"achievements">>, [1,2,3,4,5,6,7,8]},
        {<<"timestamp">>, 12345},
        {<<"email">>, <<"somewho@example.net">>}
    ],
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, login,
                    fun(Login, Password) ->
                        TestHost ! {self(), {login, Login, Password}},
                        {ok, Profile}
                    end
                ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest = iolist_to_binary([
                <<?LOGIN_TAG>>,
                session_utils:encode_auth_request(<<"login">>, <<"password">>)
            ]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionDown = lobby_utils:wait_process_down(MonitorRef, 100),
            RegisterInDatabase = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RegisterOkResponse = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            EncodedProfile = session_utils:encode_profile_request(Profile),
            ExpectedProfileFrame = <<?PROFILE_TAG, EncodedProfile/binary>>,
            ok = meck:unload(database),

            [
                ?_assertEqual({error, timeout}, SessionDown),
                ?_assertEqual({ok, {login, <<"login">>, <<"password">>}}, RegisterInDatabase),
                ?_assertEqual({ok, [
                    {data, <<?LOGIN_TAG, 0>>}, {data, ExpectedProfileFrame}
                ]}, RegisterOkResponse)
            ]

        end
    ).

update_profile_not_auth_test_() ->
    Profile = [
        {<<"rank">>, 1},
        {<<"experience">>, 2},
        {<<"reserved1">>, 1},
        {<<"reserved2">>, 2},
        {<<"reserved3">>, 3},
        {<<"reserved4">>, 5},
        {<<"reserved5">>, 5},
        {<<"reserved6">>, 6},
        {<<"reserved7">>, 7},
        {<<"score">>, 100},
        {<<"achievements">>, [1,2,3,4,5,6,7,8]},
        {<<"timestamp">>, 7890},
        {<<"email">>, <<"somewho@example.net">>}
    ],
    fixture(
        fun(_) ->
            ok = application:set_env(game_server, profile, database),
            TestHost = self(),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            ProfileRequest = session_utils:make_server_frame(
                [?PROFILE_TAG, session_utils:encode_profile_request(Profile)]
            ),
            ok = client_session:send_command(
                RemoteClientPid, {data, ProfileRequest}
            ),

            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 1000),

            [
                ?_assertEqual({ok, not_auth}, SessionDown)
            ]

        end
    ).

update_profile_test_() ->
    Profile = [
        {<<"rank">>, 1},
        {<<"experience">>, 2},
        {<<"reserved1">>, 1},
        {<<"reserved2">>, 2},
        {<<"reserved3">>, 3},
        {<<"reserved4">>, 5},
        {<<"reserved5">>, 5},
        {<<"reserved6">>, 6},
        {<<"reserved7">>, 7},
        {<<"score">>, 100},
        {<<"achievements">>, [1,2,3,4,5,6,7,8]},
        {<<"timestamp">>, 12345},
        {<<"email">>, <<"somewho@example.net">>}
    ],
    fixture(
        fun(_) ->
            ok = application:set_env(game_server, profile, database),
            TestHost = self(),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, login,
                    fun(Login, Password) ->
                        TestHost ! {self(), {login, Login, Password}},
                        {ok, Profile}
                    end
                ),

            ok = meck:expect(
                database, update_profile,
                fun(UpdatedProfile, Login) ->
                    TestHost ! {self(), {update_profile, Login, UpdatedProfile}},
                    {ok, UpdatedProfile}
                end
            ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),

            LoginRequest = iolist_to_binary([
                <<?LOGIN_TAG>>,
                session_utils:encode_auth_request(<<"login">>, <<"password">>)
            ]),
            ok = client_session:send_command( RemoteClientPid, {data, LoginRequest} ),
            {ok, {login, <<"login">>, <<"password">>}} =
                lobby_utils:wait_from_pid(RemoteClientPid, 100),
            {ok, _} = lobby_utils:wait_from_pid(RemoteClientPid, 100),


            ProfileRequest = [?PROFILE_TAG, session_utils:encode_profile_request(Profile)],

            ok = client_session:send_command( RemoteClientPid, {data, iolist_to_binary(ProfileRequest)} ),
            UpdateProfile = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ProfileResponse = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = meck:unload(database),

            SessionDown = lobby_utils:wait_process_down(MonitorRef, 100),

            [
                ?_assertMatch({ok, {update_profile, <<"login">>, _}}, UpdateProfile),
                ?_assertMatch({ok, [{data, _}]}, ProfileResponse),
                ?_assertEqual({error, timeout}, SessionDown)
            ]

        end
    ).


invalid_login_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, database),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),
            ok = meck:new(database),
            ok =
                meck:expect(
                    database, login,
                    fun(Login, Password) ->
                        TestHost ! {self(), {login, Login, Password}},
                        {error, not_found}
                    end
                ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest = iolist_to_binary([
                <<?LOGIN_TAG>>, session_utils:encode_auth_request(<<"login">>, <<"password">>)
            ]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionAlive = lobby_utils:wait_process_down(MonitorRef, 100),
            RegisterInDatabase = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RegisterOkResponse = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 1000),
            ok = meck:unload(database),
            [
                ?_assertEqual({error, timeout}, SessionAlive),
                ?_assertEqual({ok, {login, <<"login">>, <<"password">>}}, RegisterInDatabase),
                ?_assertEqual({ok, [{data, <<?LOGIN_TAG, 1>>}]}, RegisterOkResponse),
                ?_assertEqual({ok, not_auth}, SessionDown)
            ]

        end
    ).

login_in_idle_state_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ok = application:set_env(game_server, profile, undefined),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),


            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            MonitorRef = monitor(process, RemoteClientPid),
            RegisterRequest = iolist_to_binary([ <<?LOGIN_TAG>>, session_utils:encode_auth_request(<<"login">>, <<"password">>)]),
            ok = client_session:send_command(
                RemoteClientPid, {data, RegisterRequest}
            ),

            SessionDown = lobby_utils:wait_process_down_reason(MonitorRef, 100),

            [
                ?_assertEqual({ok, protocol_violation}, SessionDown)
            ]

        end
    ).

start_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self(), "red"),
                    GameStartResult =
                        case lobby_utils:wait_game_start(100) of
                            {ok, GameStart} ->
                                 GameStart;
                            Other -> Other
                        end,
                    TestHost ! {self(), GameStartResult} ,
                    receive _ -> ok end
                end,
            LocalClientPid = spawn(ClientFun),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),

            LocalGameStartResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemoteGameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            [
                ?_assertMatch({ok, #game_start{turn = true}}, LocalGameStartResult),
                ?_assertMatch({ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]}, RemoteGameStartResult)
            ]

        end
    ).

fail_to_start_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _Token} = game_lobby:checkin(self(), "red"),
                    {ok, GameStartResult} = lobby_utils:wait_game_start(100),
                    TestHost ! {self(), GameStartResult},
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    {error, closed}
                end
            ),

            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),

            LocalGameStartResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            LocalGameStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),

            RemoteGameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            RemoteClientDown = lobby_utils:wait_process_down_reason(RemoteClientRef, 100),

            exit(RemoteClientPid, normal),
            exit(LocalClientPid, normal),
            [
                ?_assertMatch({ok, #game_start{turn = true}}, LocalGameStartResult),
                ?_assertMatch({ok, #game_stop{}}, LocalGameStopResult),
                ?_assertMatch({ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]}, RemoteGameStartResult),
                ?_assertMatch({ok, normal}, RemoteClientDown)
            ]

        end
    ).

start_game_twice_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            RemoteClientMonitor = monitor(process, RemoteClientPid),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ClientDownResult = lobby_utils:wait_process_down_reason(RemoteClientMonitor, 100),

            [
                ?_assertEqual({ok, protocol_violation}, ClientDownResult)
            ]

        end
    ).

cancel_not_started_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            RemoteClientMonitor = monitor(process, RemoteClientPid),

            ok = client_session:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            ClientDownResult = lobby_utils:wait_process_down_reason(RemoteClientMonitor, 100),

            [
                ?_assertEqual({ok, protocol_violation}, ClientDownResult)
            ]

        end
    ).

cancel_waiting_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ok = client_session:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            GameStopped = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            NoOtherMessages = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            [
                ?_assertEqual({ok, [{command, ?CANCEL_GAME_PACKET}]}, GameStopped),
                ?_assertEqual({error, timeout}, NoOtherMessages)
            ]

        end
    ).


cancel_running_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = true }} = lobby_utils:wait_game_start(),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = client_session:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            LocalStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemoteStopResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalStopResult),
                ?_assertMatch({ok, [{command, ?CANCEL_GAME_PACKET}]}, RemoteStopResult)
            ]

        end
    ).

reconnect_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = true, session_pid = SessionPid }} = lobby_utils:wait_game_start(),
                    PeerLostResult = lobby_utils:wait_peer_lost(SessionPid),
                    TestHost ! {self(), PeerLostResult},
                    PeerResetResult = lobby_utils:wait_peer_reset(SessionPid),
                    TestHost ! {self(), PeerResetResult},
                    lobby_utils:wait_game_stop()
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, TokenAndATag}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            exit(RemoteClientPid, kill),
            ok = lobby_utils:wait_process_down(RemoteClientRef, 100),

            PeerLostResult = lobby_utils:wait_from_pid(LocalClientPid, 100),

            {ok, NewRemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = client_session:send_command(NewRemoteClientPid, {command, ?START_GAME_PACKET(1)}),
            ok = client_session:send_command(NewRemoteClientPid, {data, TokenAndATag}),

            GameReconnectResult = lobby_utils:wait_from_pid(NewRemoteClientPid, 100),
            PeerResetResult = lobby_utils:wait_from_pid(LocalClientPid, 100),


            [
                ?_assertMatch({ok, #peer_lost{}}, PeerLostResult),
                ?_assertMatch(
                    {ok, [{command, ?START_GAME_PACKET(0)}, {data, TokenAndATag}]},
                    GameReconnectResult
                ),
                ?_assertMatch({ok, #peer_reset{}}, PeerResetResult)
            ]

        end
    ).

reconnect_game_with_invalid_tag_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = true, session_pid = SessionPid }} = lobby_utils:wait_game_start(),
                    PeerLostResult = lobby_utils:wait_peer_lost(SessionPid),
                    TestHost ! {self(), PeerLostResult},
                    PeerResetResult = lobby_utils:wait_peer_reset(SessionPid),
                    TestHost ! {self(), PeerResetResult},
                    lobby_utils:wait_game_stop()
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, TokenAndTag}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            exit(RemoteClientPid, kill),
            ok = lobby_utils:wait_process_down(RemoteClientRef, 100),

            PeerLostResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            {Token, _Tag} = binary_to_term(TokenAndTag),

            TokenAndInvalidTag = {Token, make_ref()},

            {ok, InvalidRemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = client_session:send_command(InvalidRemoteClientPid, {command, ?START_GAME_PACKET(1)}),
            ok = client_session:send_command(InvalidRemoteClientPid, {data, term_to_binary(TokenAndInvalidTag)}),
            InvalidRemoteClientRef = monitor(process, InvalidRemoteClientPid),

            InvalidGameReconnectResult = lobby_utils:wait_from_pid(InvalidRemoteClientPid, 100),
            PeerResetResult1 = lobby_utils:wait_from_pid(LocalClientPid, 100),
            InvalidClientDown = lobby_utils:wait_process_down_reason(InvalidRemoteClientRef, 100),


            [
                ?_assertMatch({ok, #peer_lost{}}, PeerLostResult),
                ?_assertMatch(
                    {error, timeout},
                    InvalidGameReconnectResult
                ),
                ?_assertMatch({error, timeout}, PeerResetResult1),
                ?_assertEqual({ok, reconnection_token_corrupted}, InvalidClientDown)
            ]

        end
    ).

reconnect_game_with_invalid_token_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = true, session_pid = SessionPid }} = lobby_utils:wait_game_start(),
                    PeerLostResult = lobby_utils:wait_peer_lost(SessionPid),
                    TestHost ! {self(), PeerLostResult},
                    PeerResetResult = lobby_utils:wait_peer_reset(SessionPid),
                    TestHost ! {self(), PeerResetResult},
                    lobby_utils:wait_game_stop()
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            exit(RemoteClientPid, kill),
            ok = lobby_utils:wait_process_down(RemoteClientRef, 100),

            PeerLostResult = lobby_utils:wait_from_pid(LocalClientPid, 100),

            InvalidTokenAndTag = term_to_binary({make_ref(), make_ref()}),

            {ok, InvalidRemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = client_session:send_command(InvalidRemoteClientPid, {command, ?START_GAME_PACKET(1)}),
            ok = client_session:send_command(InvalidRemoteClientPid, {data, InvalidTokenAndTag}),
            InvalidRemoteClientRef = monitor(process, InvalidRemoteClientPid),

            InvalidGameStartResult = lobby_utils:wait_from_pid(InvalidRemoteClientPid, 100),
            PeerResetResult1 = lobby_utils:wait_from_pid(LocalClientPid, 100),
            InvalidClientDown = lobby_utils:wait_process_down_reason(InvalidRemoteClientRef, 100),


            [
                ?_assertMatch({ok, #peer_lost{}}, PeerLostResult),
                ?_assertEqual(
                    {ok, [{command, ?START_GAME_PACKET(0)}, {data, InvalidTokenAndTag}, {command, ?CANCEL_GAME_PACKET}]},
                    InvalidGameStartResult
                ),
                ?_assertMatch({error, timeout}, PeerResetResult1),
                ?_assertEqual({error, timeout}, InvalidClientDown)
            ]

        end
    ).


game_stopped_by_peer_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            timer:sleep(100),


            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = false}} = lobby_utils:wait_game_start(),
                    {ok, Token} = game_lobby:cancel(Token)
                end,
            _LocalClientPid = spawn(ClientFun),

            GameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            GameStopResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            [
                ?_assertMatch({ok, [{command, ?START_GAME_PACKET(1)}, {data, _}]}, GameStartResult),
                ?_assertMatch({ok, [{command, ?CANCEL_GAME_PACKET}]}, GameStopResult)
            ]
        end
    ).

peer_lost_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            timer:sleep(100),

            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = false}} = lobby_utils:wait_game_start()
                end,
            spawn(ClientFun),

            GameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RemoteClientDownResult1 = lobby_utils:wait_process_down(RemoteClientRef, 1000),

            NewClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = false}} = lobby_utils:wait_game_start(),
                    {ok, #game_stop{ token = Token }} = lobby_utils:wait_game_stop()
                end,
            spawn(NewClientFun),

            RemoteClientDownResult2 = lobby_utils:wait_process_down(RemoteClientRef, 1000),

            [
                ?_assertMatch({ok, [{command, ?START_GAME_PACKET(1)}, {data, _}]}, GameStartResult),
                ?_assertEqual({error, timeout},RemoteClientDownResult1),
                ?_assertEqual({error, timeout},RemoteClientDownResult2)
            ]
        end
    ).


peer_first_turn_test_() ->
    fixture(
        fun(_) ->
            TurnData =
                <<
                    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4,
                    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4
                >>,
            AnotherTurnData  =
                <<
                    4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1,4, 3, 2, 1,
                    4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1,4, 3, 2, 1
                >>,
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{ token = Token, turn = true, session_pid = SessionPid, tag = PeerTag}}
                        = lobby_utils:wait_game_start(),
                    ok = game_session:make_turn( SessionPid, PeerTag, TurnData),
                    PeerTurnResult = lobby_utils:wait_peer_turn(SessionPid, 1000),
                    TestHost ! {self(), PeerTurnResult}
                end,
            LocalClientPid = spawn(ClientFun),
            timer:sleep(100),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, _}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            LocalPeerTurnResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = client_session:send_command(RemoteClientPid, {command, AnotherTurnData}),
            RemotePeerTurnResult = lobby_utils:wait_from_pid(LocalClientPid, 1000),

            [
                ?_assertEqual({ok, [{command, TurnData}]}, LocalPeerTurnResult),
                ?_assertMatch({ok, {ok, #peer_turn{data = AnotherTurnData}}}, RemotePeerTurnResult)
            ]

        end
    ).

peer_surrender_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} = lobby_utils:wait_game_start(),
                    ok = game_session:surrender(SessionPid, Tag, ?SURRENDER_PACKET_NIL()),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    ParseResult = protocol_parser:parse(iolist_to_binary(Data)),
                    TestHost ! {self(), ParseResult},
                    ok
                end
            ),

            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, _}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            {ok, [{command, ?SURRENDER_PACKET = SurrenderPacket}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ok = client_session:send_command(RemoteClientPid, {command, SurrenderPacket}),


            LocalPeerGameStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemotePeerGameStopResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalPeerGameStopResult),
                ?_assertEqual({ok, [{command, ?CANCEL_GAME_PACKET}]}, RemotePeerGameStopResult)
            ]
        end
     ).

turn_instead_of_surrender_ack_test_() ->
    fixture(
        fun(_) ->
            TurnData =
                <<
                    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4,
                    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4
                >>,
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} = lobby_utils:wait_game_start(),
                    ok = game_session:surrender(SessionPid, Tag, ?SURRENDER_PACKET_NIL()),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn( ClientFun ),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            {ok, RemoteClientPid} = client_session:start(mock, mock_session_writer),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, _}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            {ok, [{command, ?SURRENDER_PACKET_NIL()}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ok = client_session:send_command(RemoteClientPid, {command, TurnData}),
            LocalPeerGameStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemotePeerDownResult = lobby_utils:wait_process_down_reason(RemoteClientRef, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalPeerGameStopResult),
                ?_assertMatch({ok, protocol_violation}, RemotePeerDownResult)
            ]
        end
    ).




after_test() ->
    ok = application:stop(lager),
    ok = application:unload(lager),
    ok = application:stop(goldrush),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler).


