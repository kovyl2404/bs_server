
-module(game_lobby_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

-include_lib("game_lobby/include/common.hrl").


fixture(Inst) ->
    {setup,
        fun() ->
            ok = application:start(folsom),
            game_lobby:start()
        end,
        fun(_) ->
            game_lobby:stop(),
            ok = application:stop(folsom)
        end,
        Inst
    }.


before_test() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [
        {lager_file_backend, [{file, "../../../test_log/game_lobby_tests.log"}]}
    ]),
    ok = application:set_env(lager, error_logger_hwm, 1000),
    ok = application:start(lager).

application_start_stop_test_() ->
    {setup,
        fun() ->
            ok = game_lobby:start_deps(),
            ok = application:start(folsom)
        end,
        fun(_) ->
            ok = application:stop(folsom),
            ok = game_lobby:stop_deps()
        end,
        fun(_) ->
            ok = application:load(game_lobby),
            Started = application:start(game_lobby),
            Children = supervisor:which_children(game_lobby_sup),
            IsLobbyAlive = is_process_alive(whereis(lobby_server)),
            Stopped = application:stop(game_lobby),
            ok = application:unload(game_lobby),
            [
                ?_assertEqual(ok, Started),
                ?_assertEqual(ok, Stopped),
                ?_assertEqual(2, length(Children)),
                ?_assert(IsLobbyAlive)
            ]
        end
    }.

checkin_while_lobby_down_test_() ->
    LobbyReply = game_lobby:checkin(self(), "red"),
    [
        ?_assertEqual({error, server_fault}, LobbyReply)
    ].

down_after_checkin_test_() ->
    fixture(
        fun(_) ->
            ClientPid = spawn( fun() -> receive stop -> ok end end ),
            {ok, Token} = game_lobby:checkin(ClientPid, "red"),
            ClientPid ! stop,
            timer:sleep(100),
            {ok, NewToken} = game_lobby:checkin(self(), "red"),

            [
                ?_assertNotEqual(Token, NewToken)
            ]
        end
    ).

checkin_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    LobbyReply = game_lobby:checkin(self(), "red"),
                    TestHost ! {self(), LobbyReply}
                end,

            ClientPid1 = spawn( ClientFun ),
            ClientPid2 = spawn( ClientFun ),

            ClientPid3 = spawn( ClientFun ),
            ClientPid4 = spawn( ClientFun ),

            Reply1 = lobby_utils:wait_from_pid(ClientPid1, 100),
            Reply2 = lobby_utils:wait_from_pid(ClientPid2, 100),

            Reply3 = lobby_utils:wait_from_pid(ClientPid3, 100),
            Reply4 = lobby_utils:wait_from_pid(ClientPid4, 100),

            [
                ?_assertMatch({ok, _}, Reply1),
                ?_assertMatch(Reply1, Reply2),
                ?_assertMatch({ok, _}, Reply3),
                ?_assertMatch(Reply3, Reply4),
                ?_assertNotEqual(Reply1, Reply3)
            ]
        end
    ).

cancel_before_game_started_test_() ->
    fixture(
        fun(_) ->
            {ok, Token} = game_lobby:checkin(self(), "red"),
            {ok, Token} = game_lobby:cancel(Token),
            GameStop = lobby_utils:wait_game_stop(Token, 100),

            CheckinResult1 = game_lobby:checkin(self(), "red"),
            CheckinResult2 = game_lobby:checkin(self(), "blue"),

            GameStart1 = lobby_utils:wait_game_start(),
            GameStart2 = lobby_utils:wait_game_start(),
            [
                ?_assertEqual(
                    {ok, #game_stop{session_pid = undefined, token = Token, tag = undefined}},
                    GameStop
                ),
                ?_assertMatch({ok, _}, CheckinResult1),
                ?_assertEqual(CheckinResult1, CheckinResult2),
                ?_assertMatch({ok, _}, GameStart1),
                ?_assertMatch({ok, _}, GameStart2)

            ]
        end
    ).

cancel_after_game_started_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{token = Token, session_pid = SessionPid, tag = Tag}}
                        = lobby_utils:wait_game_start(),
                    game_lobby:cancel(Token),
                    GameStopResult = lobby_utils:wait_game_stop(Token, 100),
                    TestHost ! {self(), GameStopResult}
                end,

            {ok, Token} = game_lobby:checkin(self(), "blue"),
            PeerPid = spawn( ClientFun ),

            {ok, #game_start{tag = Tag, session_pid = SessionPid, token = Token}}
                = lobby_utils:wait_game_start(),
            SessionMonitor = monitor(process, SessionPid),

            GameStop = lobby_utils:wait_game_stop(Token, 100),
            {ok, PeerGameStop} = lobby_utils:wait_from_pid(PeerPid, 100),
            SessionDown = lobby_utils:wait_process_down(SessionMonitor, 100),
            [
                ?_assertEqual(
                    {ok, #game_stop{token = Token, tag = Tag, session_pid = SessionPid}},
                    GameStop
                ),
                ?_assertMatch(
                    {ok, #game_stop{token = Token, session_pid = SessionPid}},
                    PeerGameStop
                ),
                ?_assertEqual(ok, SessionDown)
            ]
        end
    ).


game_start_after_checkin_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), self()),
                    GameStart =
                        case lobby_utils:wait_game_start(Token, 100) of
                            {ok, #game_start{token = Token, session_pid = SessionPid}} ->
                                {Token, SessionPid};
                            Other ->
                                Other
                        end,
                    TestHost ! {self(), GameStart}
                end,

            ClientPid1 = spawn( ClientFun ),
            ClientPid2 = spawn( ClientFun ),

            Reply1 = lobby_utils:wait_from_pid(ClientPid1, 100),
            Reply2 = lobby_utils:wait_from_pid(ClientPid2, 100),

            [
                ?_assertMatch({ok, {_, _}}, Reply1),
                ?_assertMatch({ok, {_, _}}, Reply2),
                ?_assertEqual(Reply1, Reply2)
            ]
        end
    ).

reconnect_to_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{
                        token = Token,
                        tag = Tag,
                        session_pid = SessionPid
                    }} = lobby_utils:wait_game_start(Token, 100),
                    TestHost !  {self(), {Token, Tag, SessionPid}}
                end,
            ClientPid = spawn(ClientFun),

            {ok, Token} = game_lobby:checkin(self(), "blue"),
            {ok, #game_start{
                session_pid = ActualSessionPid
            }} = lobby_utils:wait_game_start(Token, 100),
            {ok, #peer_lost{}} = lobby_utils:wait_peer_lost(ActualSessionPid, 100),
            {ok, {Token, TheirTag, ActualSessionPid}} = lobby_utils:wait_from_pid(ClientPid, 100),

            ReconnectResult = game_lobby:checkin(self(), "red", Token, TheirTag),
            GameStartResult = lobby_utils:wait_game_start(Token, 100),
            PeerResetResult = lobby_utils:wait_peer_reset(ActualSessionPid, 100),

            [
                ?_assertEqual({ok, Token}, ReconnectResult),
                ?_assertMatch(
                    {ok, #game_start{ session_pid = ActualSessionPid, tag = TheirTag} },
                    GameStartResult
                ),
                ?_assertMatch({ok, #peer_reset{session_pid = ActualSessionPid}}, PeerResetResult)
            ]

        end
    ).

reconnect_to_expired_game_test_() ->
    fixture(
        fun(_) ->
            TestHost = self(),
            ClientFun =
                fun() ->
                    {ok, Token} = game_lobby:checkin(self(), "red"),
                    {ok, #game_start{
                        token = Token,
                        tag = Tag,
                        session_pid = SessionPid
                    }} = lobby_utils:wait_game_start(Token, 100),
                    TestHost !  {self(), {Token, Tag, SessionPid}}
                end,
            ClientPid = spawn(ClientFun),

            {ok, Token} = game_lobby:checkin(self(), "blue"),
            {ok, #game_start{
                session_pid = ActualSessionPid
            }} = lobby_utils:wait_game_start(Token, 100),
            {ok, #peer_lost{}} = lobby_utils:wait_peer_lost(ActualSessionPid, 100),
            {ok, {Token, TheirTag, ActualSessionPid}} = lobby_utils:wait_from_pid(ClientPid, 100),

            MonitorRef = monitor(process, ActualSessionPid),
            ok = lobby_utils:wait_process_down(MonitorRef, 2500),

            ReconnectResult = game_lobby:checkin(self(), "red", Token, TheirTag),
            GameStartResult = lobby_utils:wait_game_start(Token, 100),
            PeerResetResult = lobby_utils:wait_peer_reset(ActualSessionPid, 100),

            [
                ?_assertEqual({error, session_expired}, ReconnectResult),
                ?_assertMatch({error, timeout}, GameStartResult ),
                ?_assertMatch({error, timeout}, PeerResetResult)
            ]

        end
    ).


after_test() ->
    ok = application:stop(lager),
    ok = application:unload(lager),
    ok = application:stop(goldrush),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler).


