
-module(game_session_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_lobby/include/common.hrl").

before_test() ->
    error_logger:tty(false).

start_game_session_test_() ->
    TestProc = self(),
    ClientFun =
        fun() ->
            {ok, Res} = lobby_utils:wait_game_start(),
            TestProc ! {self(), Res}
        end,
    Client1 = spawn( ClientFun ),
    Client2 = spawn( ClientFun ),

    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = Client1, tag = <<"red">> },
            #peer_id{ client_pid = Client2, tag = <<"blue">>}
        ),

    GameStartClient1 = lobby_utils:wait_from_pid(Client1, 100),
    GameStartClient2 = lobby_utils:wait_from_pid(Client2, 100),

    exit(Client1, kill),
    exit(Client2, kill),

    [
        ?_assertMatch(
            {ok, #game_start{session_pid = SessionPid, tag = <<"red">>, token = some_token, turn = true}},
            GameStartClient1
        ),
        ?_assertMatch(
            {ok, #game_start{ session_pid = SessionPid, tag = <<"blue">>, token = some_token, turn = false}},
            GameStartClient2
        )
    ].

first_peer_lost_test_() ->
    PeerPid = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef = monitor(process, PeerPid),
    {ok, SessionPid} = game_session:start_link(
        some_token,
        #peer_id{ client_pid = self(), tag = <<"red">> },
        #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
    ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef, 100),
    PeerLost = lobby_utils:wait_peer_lost(SessionPid, 100),

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),
    [
        ?_assertMatch(
            {ok, #peer_lost{session_pid = SessionPid }},
            PeerLost
        ),
        ?_assertMatch(ok, SessionShutdown),
        ?_assert( SessionShutdownTime >= 1500000 andalso SessionShutdownTime =< 2500000)
    ].


first_peer_reconnect_after_start_test_() ->
    process_flag(trap_exit, true),
    PeerPid = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef = monitor(process, PeerPid),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = PeerPid, tag = <<"blue">> },
            #peer_id{ client_pid = self(), tag = <<"red">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, #game_start{turn = false}} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{client_pid = self(), tag = <<"blue">>}),
    GameStart = lobby_utils:wait_game_start(100),
    PeerReturn = lobby_utils:wait_peer_reset(SessionPid, 100),

    SessionDown = lobby_utils:wait_process_down(SessionMonitor, 5000),

    [
        ?_assertMatch(
            {ok, #game_start{session_pid = SessionPid, tag = <<"blue">>, token = some_token, turn = true}},
            GameStart
        ),
        ?_assertMatch(
            {ok, #peer_reset{ session_pid = SessionPid }}, PeerReturn
        ),
        ?_assertMatch({error, timeout}, SessionDown)
    ].

second_peer_reconnect_after_start_test_() ->
    process_flag(trap_exit, true),
    PeerPid = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef = monitor(process, PeerPid),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, #game_start{turn = true}} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{client_pid = self(), tag = <<"blue">>}),
    GameStart = lobby_utils:wait_game_start(100),
    PeerReturn = lobby_utils:wait_peer_reset(SessionPid, 100),

    SessionDown = lobby_utils:wait_process_down(SessionMonitor, 5000),

    [
        ?_assertMatch(
            {ok, #game_start{session_pid = SessionPid, tag = <<"blue">>, token = some_token, turn = false}},
            GameStart
        ),
        ?_assertMatch(
            {ok, #peer_reset{ session_pid = SessionPid }}, PeerReturn
        ),
        ?_assertMatch({error, timeout}, SessionDown)
    ].

peer_multiple_reconnects_test_() ->
    PeerPid = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef1 = monitor(process, PeerPid),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">>  },
            #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, #game_start{turn = true}} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef1, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    timer:sleep(1000),

    PeerPid2 = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef2 = monitor(process, PeerPid2),
    ok = game_session:set_peer(SessionPid, #peer_id{client_pid = PeerPid2, tag = <<"blue">>}),
    {ok, _} = lobby_utils:wait_peer_reset(SessionPid, 100),
    ok = lobby_utils:wait_process_down(MonitorRef2, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    {SessionShutdownTime, ok} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 5000) end ),

    [
        ?_assert( SessionShutdownTime >= 1500000 andalso SessionShutdownTime =< 2500000)
    ].

make_turn_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"some_turn">>),
    Result = lobby_utils:wait_peer_turn(SessionPid, 100),
    [
        ?_assertMatch(
            {ok, #peer_turn{ session_pid = SessionPid, data = <<"some_turn">>}},
            Result
        )
    ].

illegal_turn_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    TurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),
    TurnError = lobby_utils:wait_peer_turn_fail(SessionPid, 100),
    PeerLost = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"red">>, <<"some_turn">>),
    ValidTurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),
    [
        ?_assertMatch( {error, timeout}, TurnResult ),
        ?_assertMatch( {ok, #illegal_turn{session_pid = SessionPid}}, TurnError ),
        ?_assertMatch( {ok, #peer_lost{session_pid = SessionPid}}, PeerLost ),
        ?_assertEqual({error, timeout}, ValidTurnResult),
        ?_assertEqual( ok, SessionShutdown ),
        ?_assert( SessionShutdownTime > 1500000 andalso SessionShutdownTime < 2500000 )
    ].

reconnect_after_illegal_turn_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    {ok, _} = lobby_utils:wait_peer_turn_fail(SessionPid, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    timer:sleep(1000),
    ok = game_session:set_peer(SessionPid, #peer_id{ tag = <<"blue">>, client_pid = self() }),

    GameStart = lobby_utils:wait_game_start(100),
    PeerReset = lobby_utils:wait_peer_reset(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    TurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),
    TurnError = lobby_utils:wait_peer_turn_fail(SessionPid, 100),
    PeerLost = lobby_utils:wait_peer_lost(SessionPid, 100),
    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),

    [
        ?_assertMatch({ok, #game_start{session_pid = SessionPid, tag = <<"blue">>, token = some_token, turn = false}}, GameStart),
        ?_assertEqual({ok, #peer_reset{session_pid = SessionPid}}, PeerReset),
        ?_assertMatch( {error, timeout}, TurnResult ),
        ?_assertMatch( {ok, #illegal_turn{session_pid = SessionPid}}, TurnError ),
        ?_assertMatch( {ok, #peer_lost{session_pid = SessionPid}}, PeerLost ),
        ?_assertEqual( ok, SessionShutdown ),
        ?_assert( SessionShutdownTime > 1500000 andalso SessionShutdownTime < 2500000 )
    ].

peer_reconnect_and_lost_previous_turn_test_() ->
    ClientFun = fun() -> receive #peer_turn{} -> ok end end,

    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>}
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"turn_data">>),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    {ok, #game_start{tag = <<"blue">>, token = some_token, session_pid = SessionPid, turn = false}} =
        lobby_utils:wait_game_start(100),

    TurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),
    [
        ?_assertMatch(
            {ok, #peer_turn{session_pid = SessionPid, data = <<"turn_data">>}},
            TurnResult
        )
    ].

peer_make_turn_and_reconnects_test_() ->
    ClientFun =
        fun() ->
            {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} =
                lobby_utils:wait_game_start(),
            ok = game_session:make_turn(SessionPid, Tag, <<"turn_data">>)
        end,

    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>},
            #peer_id{ client_pid = self(), tag = <<"red">> }
        ),
    {ok, #game_start{session_pid = SessionPid}} = lobby_utils:wait_game_start(100),
    {ok, #peer_turn{data = <<"turn_data">>}} = lobby_utils:wait_peer_turn( SessionPid, 100),
    {ok, #peer_lost{session_pid = SessionPid}} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    GameStart = lobby_utils:wait_game_start(100),
    NoTurnRepeatForBlue = lobby_utils:wait_peer_turn( SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"red">>, <<"another_turn">>),
    RedTurn = lobby_utils:wait_peer_turn( SessionPid, 100),
    [
        ?_assertMatch(
            {ok, #game_start{tag = <<"blue">>, token = some_token, session_pid = SessionPid, turn = false}},
            GameStart
        ),
        ?_assertMatch({error, timeout}, NoTurnRepeatForBlue),
        ?_assertMatch( {ok, #peer_turn{data = <<"another_turn">>}}, RedTurn )
    ].



reconnect_expired_session_test_() ->
    ClientFun = fun() -> lobby_utils:wait_game_start() end,
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>}
        ),
    MonitorRef = monitor(process, SessionPid),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef, 2500),
    SetPeerResult = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    [
        ?_assertMatch({error, session_expired}, SetPeerResult)
    ].

stop_game_in_turn_test_() ->
    TestHost = self(),
    ClientFun =
        fun() ->
            {ok, #game_start{
                session_pid = SessionPid,
                token = Token, turn = false
            }} = lobby_utils:wait_game_start(),
            {ok, #peer_turn{}} = lobby_utils:wait_peer_turn(SessionPid, 100),
            ok = game_session:stop_game(SessionPid),
            StopResult = lobby_utils:wait_game_stop(Token, 100),
            TestHost ! {self(), StopResult}
        end,

    ClientPid = spawn( ClientFun ),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = ClientPid, tag = <<"blue">>}
        ),
    MonitorRef = monitor(process, SessionPid),
    {ok, #game_start{
        session_pid = SessionPid
    }} = lobby_utils:wait_game_start(some_token),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"some_turn">>),

    GameStopResult = lobby_utils:wait_game_stop(some_token, 100),
    SessionDownResult = lobby_utils:wait_process_down(MonitorRef, 100),
    {ok, PeerStopResult} = lobby_utils:wait_from_pid(ClientPid, 100),

    [
        ?_assertMatch(
            {ok, #game_stop{ token = some_token, tag = <<"red">>, session_pid = SessionPid} },
            GameStopResult
        ),
        ?_assertMatch(
            {ok, #game_stop{tag = <<"blue">>, token = some_token, session_pid = SessionPid}},
            PeerStopResult
        ),
        ?_assertEqual(ok, SessionDownResult)
    ].


stop_game_out_of_turn_test_() ->
    TestHost = self(),
    ClientFun =
        fun() ->
            {ok, #game_start{
                session_pid = SessionPid,
                token = Token, tag = _Tag
            }} = lobby_utils:wait_game_start(),
            ok = game_session:stop_game(SessionPid),
            StopResult = lobby_utils:wait_game_stop(Token, 100),
            TestHost ! {self(), StopResult}
        end,

    ClientPid = spawn( ClientFun ),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = ClientPid, tag = <<"blue">>}
        ),
    MonitorRef = monitor(process, SessionPid),
    {ok, #game_start{
        session_pid = SessionPid
    }} = lobby_utils:wait_game_start(some_token),

    GameStopResult = lobby_utils:wait_game_stop(some_token, 100),
    SessionDownResult = lobby_utils:wait_process_down(MonitorRef, 100),
    {ok, PeerStopResult} = lobby_utils:wait_from_pid(ClientPid, 100),

    [
        ?_assertMatch(
            {ok, #game_stop{ token = some_token, tag = <<"red">>, session_pid = SessionPid} },
            GameStopResult
        ),
        ?_assertMatch(
            {ok, #game_stop{tag = <<"blue">>, token = some_token, session_pid = SessionPid}},
            PeerStopResult
        ),
        ?_assertEqual(ok, SessionDownResult)
    ].


peer_surrender_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:surrender(SessionPid, <<"red">>, <<"surrender_data">>),
    SurrenderResult = lobby_utils:wait_peer_surrender(SessionPid, 100),
    [
        ?_assertMatch({ok, #peer_surrender{}}, SurrenderResult)
    ].

peer_surrender_out_of_order_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:surrender(SessionPid, <<"blue">>, <<"surrender_data">>),
    TurnResult = lobby_utils:wait_peer_surrender(SessionPid, 100),
    TurnError = lobby_utils:wait_peer_turn_fail(SessionPid, 100),
    PeerLost = lobby_utils:wait_peer_lost(SessionPid, 100),
    SessionMonitor = monitor(process, SessionPid),

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),
    [
        ?_assertMatch( {error, timeout}, TurnResult ),
        ?_assertMatch( {ok, #illegal_turn{session_pid = SessionPid}}, TurnError ),
        ?_assertMatch( {ok, #peer_lost{session_pid = SessionPid}}, PeerLost ),
        ?_assertEqual( ok, SessionShutdown ),
        ?_assert( SessionShutdownTime > 1500000 andalso SessionShutdownTime < 2500000 )
    ].


peer_reconnect_and_lost_previous_surrender_test_() ->
    ClientFun = fun() -> receive #peer_surrender{} -> ok end end,

    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>}
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = game_session:surrender(SessionPid, <<"red">>, <<"surrender_data">>),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    {ok, #game_start{tag = <<"blue">>, token = some_token, session_pid = SessionPid, turn = false}} =
        lobby_utils:wait_game_start(100),

    TurnResult = lobby_utils:wait_peer_surrender(SessionPid, 100),
    [
        ?_assertMatch(
            {ok, #peer_surrender{session_pid = SessionPid}},
            TurnResult
        )
    ].

peer_surrender_and_reconnects_test_() ->
    ClientFun =
        fun() ->
            {ok, #game_start{turn = true, session_pid = SessionPid, tag = Tag}} =
                lobby_utils:wait_game_start(),
            ok = game_session:surrender(SessionPid, Tag, <<"surrender_data">>)
        end,

    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>},
            #peer_id{ client_pid = self(), tag = <<"red">> }
        ),
    {ok, #game_start{session_pid = SessionPid, token = Token}} = lobby_utils:wait_game_start(100),
    {ok, #peer_surrender{}} = lobby_utils:wait_peer_surrender( SessionPid, 100),
    {ok, #peer_lost{session_pid = SessionPid}} = lobby_utils:wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    GameStart = lobby_utils:wait_game_start(100),
    NoTurnRepeatForBlue = lobby_utils:wait_peer_surrender( SessionPid, 100),

    ok = game_session:surrender(SessionPid, <<"red">>),
    GameStop1 = lobby_utils:wait_game_stop(Token, 1000),
    GameStop2 = lobby_utils:wait_game_stop(Token, 1000),
    [
        ?_assertMatch(
            {ok, #game_start{tag = <<"blue">>, token = some_token, session_pid = SessionPid, turn = false}},
            GameStart
        ),
        ?_assertMatch({error, timeout}, NoTurnRepeatForBlue),
        ?_assertMatch({ok, #game_stop{}}, GameStop1),
        ?_assertMatch({ok, #game_stop{}}, GameStop2)
    ].

after_test() ->
    error_logger:tty(true).