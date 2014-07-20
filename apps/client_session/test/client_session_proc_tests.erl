
-module(client_session_proc_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_lobby/include/common.hrl").
-include_lib("client_session/include/client_protocol.hrl").

fixture(Inst) ->
    {setup,
        fun() ->
            ok = game_lobby:start(),
            ok = meck:new(mock_session_writer)
        end,
        fun(_) ->
            ok = meck:unload(mock_session_writer),
            ok = game_lobby:stop()
        end,
        Inst
    }.

before_test() ->
    error_logger:tty(false).

start_game_test_() ->
    fixture(
        fun() ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self()),
                    GameStartResult = lobby_utils:wait_game_start(100),
                    TestHost ! {self(), GameStartResult} ,
                    receive _ -> ok end
                end,
            LocalClientPid = spawn(ClientFun),
            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),

            LocalGameStartResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemoteGameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            exit(RemoteClientPid, normal),
            exit(LocalClientPid, normal),
            [
                ?_assertMatch({ok, #game_start{turn = true}}, LocalGameStartResult),
                ?_assertMatch({ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]}, RemoteGameStartResult)
            ]

        end
    ).

fail_to_start_game_test_() ->
    fixture(
        fun() ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self()),
                    GameStartResult = lobby_utils:wait_game_start(100),
                    TestHost ! {self(), GameStartResult} ,
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),
            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    {error, closed}
                end
            ),

            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),

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
        fun() ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            RemoteClientMonitor = monitor(process, RemoteClientPid),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ClientDownResult = lobby_utils:wait_process_down_reason(RemoteClientMonitor, 100),

            [
                ?_assertEqual({ok, game_already_started}, ClientDownResult)
            ]

        end
    ).

cancel_not_started_game_test_() ->
    fixture(
        fun() ->
            TestHost = self(),
            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            RemoteClientMonitor = monitor(process, RemoteClientPid),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            ClientDownResult = lobby_utils:wait_process_down_reason(RemoteClientMonitor, 100),

            [
                ?_assertEqual({ok, game_not_started}, ClientDownResult)
            ]

        end
    ).

cancel_waiting_game_test_() ->
    fixture(
        fun() ->
            TestHost = self(),

            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_Socket, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            ok = client_session_proc:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

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
        fun() ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
                    {ok, #game_start{ token = Token, turn = true }} = lobby_utils:wait_game_start(),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            LocalStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemoteStopResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalStopResult),
                ?_assertMatch({ok, [{command, ?CANCEL_GAME_PACKET}]}, RemoteStopResult)
            ]

        end
    ).

fail_cancel_running_game_test_() ->
    fixture(
        fun() ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
                    {ok, #game_start{ token = Token, turn = true }} = lobby_utils:wait_game_start(),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),

            {ok, RemoteClientPid} = client_session_proc:start(mock, mock_session_writer),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    ok
                end
            ),
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session_proc:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    {error, closed}
                end
            ),

            ok = client_session_proc:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

            LocalStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemoteStopResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RemoteDownResult = lobby_utils:wait_process_down_reason(RemoteClientRef, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalStopResult),
                ?_assertMatch({ok, [{command, ?CANCEL_GAME_PACKET}]}, RemoteStopResult),
                ?_assertMatch({ok, normal}, RemoteDownResult)
            ]

        end
    ).


after_test() ->
    error_logger:tty(true).



