
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
            {ok, Res} = wait_game_start(),
            TestProc ! {self(), Res}
        end,
    Client1 = spawn( ClientFun ),
    Client2 = spawn( ClientFun ),

    {ok, SessionPid} =
        game_session:start_link(
            #peer_id{ client_pid = Client1, tag = <<"red">> },
            #peer_id{ client_pid = Client2, tag = <<"blue">>}
        ),

    GameStartClient1 = wait_from_pid(Client1, 100),
    GameStartClient2 = wait_from_pid(Client2, 100),

    exit(Client1, kill),
    exit(Client2, kill),

    [
        ?_assertMatch(
            {ok, #game_start{session_pid = SessionPid, tag = <<"red">>}}, GameStartClient1
        ),
        ?_assertMatch(
            {ok, #game_start{ session_pid = SessionPid, tag = <<"blue">>}}, GameStartClient2
        )
    ].

first_peer_lost_test_() ->
    PeerPid = spawn( fun wait_game_start/0 ),
    MonitorRef = monitor(process, PeerPid),
    {ok, SessionPid} = game_session:start_link(
        #peer_id{ client_pid = self(), tag = <<"red">> },
        #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
    ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = wait_game_start(100),
    ok = wait_process_down(MonitorRef, 100),
    PeerLost = wait_peer_lost(SessionPid, 100),

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> wait_process_down(SessionMonitor, 2500) end ),
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
    PeerPid = spawn( fun wait_game_start/0 ),
    MonitorRef = monitor(process, PeerPid),
    {ok, SessionPid} =
        game_session:start_link(
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = wait_game_start(100),
    ok = wait_process_down(MonitorRef, 100),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{client_pid = self(), tag = <<"blue">>}),
    GameStart = wait_game_start(100),
    PeerReturn = wait_peer_reset(SessionPid, 100),

    SessionDown = wait_process_down(SessionMonitor, 5000),

    [
        ?_assertMatch(
            {ok, #game_start{session_pid = SessionPid, tag = <<"blue">>}},
            GameStart
        ),
        ?_assertMatch(
            {ok, #peer_reset{ session_pid = SessionPid }}, PeerReturn
        ),
        ?_assertMatch({error, timeout}, SessionDown)
    ].

peer_multiple_reconnects_test_() ->
    PeerPid = spawn( fun wait_game_start/0 ),
    MonitorRef1 = monitor(process, PeerPid),
    {ok, SessionPid} =
        game_session:start_link(
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = PeerPid, tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = wait_game_start(100),
    ok = wait_process_down(MonitorRef1, 100),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    timer:sleep(1000),

    PeerPid2 = spawn( fun wait_game_start/0 ),
    MonitorRef2 = monitor(process, PeerPid2),
    ok = game_session:set_peer(
        SessionPid, #peer_id{client_pid = PeerPid2, tag = <<"blue">>}
    ),
    {ok, _} = wait_peer_reset(SessionPid, 100),
    ok = wait_process_down(MonitorRef2, 100),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    {SessionShutdownTime, ok} = timer:tc( fun() -> wait_process_down(SessionMonitor, 5000) end ),

    [
        ?_assert( SessionShutdownTime >= 1500000 andalso SessionShutdownTime =< 2500000)
    ].

make_turn_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    {ok, _} = wait_game_start(100),
    {ok, _} = wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"some_turn">>),
    Result = wait_peer_turn(SessionPid, 100),
    [
        ?_assertMatch(
            {ok, #peer_turn{ session_pid = SessionPid, data = <<"some_turn">>}},
            Result
        )
    ].

illegal_turn_test_() ->
    {ok, SessionPid} =
        game_session:start_link(
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = wait_game_start(100),
    {ok, _} = wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    TurnResult = wait_peer_turn(SessionPid, 100),
    TurnError = wait_peer_turn_fail(SessionPid, 100),
    PeerLost = wait_peer_lost(SessionPid, 100),

    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> wait_process_down(SessionMonitor, 2500) end ),
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
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = self(), tag = <<"blue">> }
        ),
    SessionMonitor = monitor(process, SessionPid),
    {ok, _} = wait_game_start(100),
    {ok, _} = wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    {ok, _} = wait_peer_turn_fail(SessionPid, 100),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    timer:sleep(1000),
    ok = game_session:set_peer(SessionPid, #peer_id{ tag = <<"blue">>, client_pid = self() }),

    GameStart = wait_game_start(100),
    PeerChange = wait_peer_change(SessionPid, 100),
    PeerReset = wait_peer_reset(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"some_turn">>),
    TurnResult = wait_peer_turn(SessionPid, 100),
    TurnError = wait_peer_turn_fail(SessionPid, 100),
    PeerLost = wait_peer_lost(SessionPid, 100),
    {SessionShutdownTime, SessionShutdown} = timer:tc( fun() -> wait_process_down(SessionMonitor, 2500) end ),

    [
        ?_assertEqual({ok, #game_start{session_pid = SessionPid, tag = <<"blue">>}}, GameStart),
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
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>}
        ),
    {ok, _} = wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"turn_data">>),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    TurnResult = wait_peer_turn(SessionPid, 100),
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
            #peer_id{ client_pid = self(), tag = <<"red">> },
            #peer_id{ client_pid = spawn( ClientFun ), tag = <<"blue">>}
        ),

    {ok, _} = wait_game_start(100),
    ok = game_session:make_turn(SessionPid, <<"red">>, <<"turn_data">>),
    {ok, _} = wait_peer_lost(SessionPid, 100),

    ok = game_session:set_peer(SessionPid, #peer_id{tag = <<"blue">>, client_pid = self()}),
    TurnRepeatResult = wait_peer_turn(SessionPid, 100),

    ok = game_session:make_turn(SessionPid, <<"blue">>, <<"next_turn_data">>),

    TurnResult = wait_peer_turn(SessionPid, 100),
    [
        ?_assertMatch({error, timeout}, TurnRepeatResult),
        ?_assertMatch({ok, #peer_turn{session_pid = SessionPid, data = <<"next_turn_data">>}}, TurnResult)
    ].


wait_from_pid(Pid, Timeout) ->
    receive
        {Pid, Message} ->
            {ok, Message}
    after Timeout ->
        {error, timeout}
    end.

wait_game_start(Timeout) ->
    receive
        #game_start{} = Val ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.

wait_game_start() ->
    receive
        #game_start{} = Val ->
            {ok, Val}
    end.

wait_process_down(MonitorRef, Timeout) ->
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            ok
    after Timeout ->
        {error, timeout}
    end.

wait_peer_lost(SessionPid, Timeout) ->
    receive
        #peer_lost{session_pid = SessionPid} = PeerLost->
            {ok, PeerLost}
    after Timeout ->
        {error, timeout}
    end.

wait_peer_reset(SessionPid, Timeout) ->
    receive
        #peer_reset{session_pid = SessionPid} = Val ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.

wait_peer_turn(SessionPid, Timeout) ->
    receive
        #peer_turn{session_pid = SessionPid} = Val ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.


wait_peer_turn_fail(SessionPid, Timeout) ->
    receive
        #illegal_turn{session_pid = SessionPid} = Val ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.

wait_peer_change(SessionPid, Timeout) ->
    receive
        #peer_change{session_pid = SessionPid} = Val ->
            {ok, Val}
    after Timeout ->
        {error, Timeout}
    end.

after_test() ->
    error_logger:tty(true).