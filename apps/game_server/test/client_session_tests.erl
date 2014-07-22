
-module(client_session_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_lobby/include/common.hrl").
-include_lib("game_server/include/client_protocol.hrl").

fixture(Inst) ->
    {setup,
        fun() ->
            ok = game_lobby:start(),
            ok = meck:new(mock_session_writer, [passthrough]),
            flush_messages()
        end,
        fun(_) ->
            ok = meck:unload(mock_session_writer),
            ok = game_lobby:stop()
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
    error_logger:tty(false).

start_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self()),
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
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
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
                    {ok, _Token} = game_lobby:checkin(self()),
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
                    {ok, Token} = game_lobby:checkin(self()),
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

fail_cancel_running_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
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
            RemoteClientRef = monitor(process, RemoteClientPid),
            ok = client_session:send_command(RemoteClientPid, {command, ?START_GAME_PACKET(0)}),
            {ok, [{command, ?START_GAME_PACKET(0)}, {data, _}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),

            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
                    {error, closed}
                end
            ),

            ok = client_session:send_command(RemoteClientPid, {command, ?CANCEL_GAME_PACKET}),

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

reconnect_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
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
                    {ok, Token} = game_lobby:checkin(self()),
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
                    {ok, Token} = game_lobby:checkin(self()),
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
                    {ok, Token} = game_lobby:checkin(self()),
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
                    {ok, Token} = game_lobby:checkin(self()),
                    {ok, #game_start{ token = Token, turn = false}} = lobby_utils:wait_game_start()
                end,
            spawn(ClientFun),

            GameStartResult = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            RemoteClientDownResult1 = lobby_utils:wait_process_down(RemoteClientRef, 1000),

            NewClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
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
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self()),
                    {ok, #game_start{ token = Token, turn = true, session_pid = SessionPid, tag = PeerTag}}
                        = lobby_utils:wait_game_start(),
                    ok = game_session:make_turn(SessionPid, PeerTag, <<1,2,3,4>>),
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

            ok = client_session:send_command(RemoteClientPid, {command, <<4,3,2,1>>}),
            RemotePeerTurnResult = lobby_utils:wait_from_pid(LocalClientPid, 1000),

            [
                ?_assertEqual({ok, [{command, <<1,2,3,4>>}]}, LocalPeerTurnResult),
                ?_assertMatch({ok, {ok, #peer_turn{data = <<4,3,2,1>>}}}, RemotePeerTurnResult)
            ]

        end
    ).

peer_surrender_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self()),
                    {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} = lobby_utils:wait_game_start(),
                    ok = game_session:surrender(SessionPid, Tag, ?SURRENDER_PACKET(0, 0, 0)),
                    GameStopResult = lobby_utils:wait_game_stop(),
                    TestHost ! {self(), GameStopResult}
                end,
            LocalClientPid = spawn(ClientFun),
            ok = meck:expect(
                mock_session_writer, send,
                fun(_, Data) ->
                    TestHost ! {self(), protocol_parser:parse(iolist_to_binary(Data))},
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
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, _} = game_lobby:checkin(self()),
                    {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} = lobby_utils:wait_game_start(),
                    ok = game_session:surrender(SessionPid, Tag, ?SURRENDER_PACKET(0, 0, 0)),
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
            {ok, [{command, <<123, 0, 0, 0>>}]} = lobby_utils:wait_from_pid(RemoteClientPid, 100),
            ok = client_session:send_command(RemoteClientPid, {command, <<1,2,3,4>>}),
            LocalPeerGameStopResult = lobby_utils:wait_from_pid(LocalClientPid, 100),
            RemotePeerDownResult = lobby_utils:wait_process_down_reason(RemoteClientRef, 100),
            [
                ?_assertMatch({ok, #game_stop{}}, LocalPeerGameStopResult),
                ?_assertMatch({ok, protocol_violation}, RemotePeerDownResult)
            ]
        end
    ).



after_test() ->
    error_logger:tty(true).


