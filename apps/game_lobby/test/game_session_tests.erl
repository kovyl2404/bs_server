
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


peer_reconnect_test_() ->
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
    {ok, _} = lobby_utils:wait_game_start(100),
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
    {ok, _} = lobby_utils:wait_game_start(100),
    ok = lobby_utils:wait_process_down(MonitorRef1, 100),
    {ok, _} = lobby_utils:wait_peer_lost(SessionPid, 100),

    timer:sleep(1000),

    PeerPid2 = spawn( fun lobby_utils:wait_game_start/0 ),
    MonitorRef2 = monitor(process, PeerPid2),
    ok = game_session:set_peer(
        SessionPid, #peer_id{client_pid = PeerPid2, tag = <<"blue">>}
    ),
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

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),
    [
        ?_assertMatch( {error, timeout}, TurnResult ),
        ?_assertMatch( {ok, #illegal_turn{session_pid = SessionPid}}, TurnError ),
        ?_assertMatch( {ok, #peer_lost{session_pid = SessionPid}}, PeerLost ),
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
    PeerChange = lobby_utils:wait_peer_change(SessionPid, 100),
    PeerReset = lobby_utils:wait_peer_reset(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    TurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),
    TurnError = lobby_utils:wait_peer_turn_fail(SessionPid, 100),
    PeerLost = lobby_utils:wait_peer_lost(SessionPid, 100),
    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> lobby_utils:wait_process_down(SessionMonitor, 2500) end ),

    [
        ?_assertMatch({ok, #game_start{session_pid = SessionPid, tag = <<"blue">>, token = some_token, turn = false}}, GameStart),
        ?_assertEqual({ok, #peer_change{session_pid = SessionPid}}, PeerChange),
        ?_assertEqual({ok, #peer_reset{session_pid = SessionPid}}, PeerReset),
        ?_assertMatch( {error, timeout}, TurnResult ),
        ?_assertMatch( {ok, #illegal_turn{session_pid = SessionPid}}, TurnError ),
        ?_assertMatch( {ok, #peer_lost{session_pid = SessionPid}}, PeerLost ),
        ?_assertEqual( ok, SessionShutdown ),
        ?_assert( SessionShutdownTime > 1500000 andalso SessionShutdownTime < 2500000 )
    ].

peer_lost_turn_test_() ->
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


peer_ack_test_() ->
    ClientFun =
        fun() ->
            #peer_turn{data = Data, session_pid = ActualSessionPid} =
                receive
                    #peer_turn{ } = Turn -> Turn
                end,
            ok = game_session:ack_turn(ActualSessionPid, <<"blue">>, Data)
        end,
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
    {ok, #game_start{tag = <<"blue">>, token = some_token, session_pid = SessionPid}} =
        lobby_utils:wait_game_start(100),
    TurnRepeatResult = lobby_utils:wait_peer_turn(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"next_turn_data">>),

    TurnResult = lobby_utils:wait_peer_turn(SessionPid, 100),
    [
        ?_assertMatch({error, timeout}, TurnRepeatResult),
        ?_assertMatch({ok, #peer_turn{session_pid = SessionPid, data = <<"next_turn_data">>}}, TurnResult)
    ].

peer_reconnect_after_noack_test_() ->
    TestHost = self(),
    ClientFun =
        fun() ->
            {ok, #game_start{token = _Token, session_pid = Session, turn = true, tag = Tag}} =
                lobby_utils:wait_game_start(),
            ok = game_session:make_turn(Session, Tag, <<"turn_data">>),
            receive
                stop -> ok
            end
        end,
    ClientPid = spawn(ClientFun),
    {ok, SessionPid} =
        game_session:start_link(
            some_token,
            #peer_id{ client_pid = ClientPid, tag = <<"blue">>},
            #peer_id{ client_pid = self(), tag = <<"red">> }
        ),
    {ok, _} = lobby_utils:wait_game_start(100),
    {ok, _} = lobby_utils:wait_peer_turn(SessionPid, 100),
    NewClientFun =
        fun() ->
            {ok, #game_start{ turn = IsOursTurn, session_pid = NewSessionPid }} = lobby_utils:wait_game_start(),
            PreviousTurn = lobby_utils:wait_peer_turn(NewSessionPid, 100),
            TestHost ! {self(), {IsOursTurn, PreviousTurn}}
        end,
    NewClientPid = spawn( NewClientFun ),
    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"red">>, client_pid = NewClientPid}),

    Result = lobby_utils:wait_from_pid(NewClientPid, 1000),
    [
        ?_assertMatch({ok, {false, {ok, #peer_turn{}}}}, Result)
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
                tag = Tag, session_pid = SessionPid,
                token = Token
            }} = lobby_utils:wait_game_start(),
            {ok, #peer_turn{data = Data}} = lobby_utils:wait_peer_turn(SessionPid, 100),
            ok = game_session:ack_turn(SessionPid, Tag, Data),
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

stop_game_no_turn_ack_test_() ->
    TestHost = self(),
    ClientFun =
        fun() ->
            {ok, #game_start{
                tag = Tag, session_pid = SessionPid,
                token = Token
            }} = lobby_utils:wait_game_start(),
            {ok, _} = lobby_utils:wait_peer_turn(SessionPid, 100),
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


after_test() ->
    error_logger:tty(true).