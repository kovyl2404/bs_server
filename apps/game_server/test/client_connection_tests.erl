
-module(client_connection_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").


fixture(Inst) ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        Inst
    }.

setup() ->
    ok = application:start(ranch),
    ok = application:load(game_server),
    ok = application:set_env(game_server, port, 7891),
    ok = application:set_env(game_server, max_pings_allowed, 5),
    ok = application:set_env(game_server, ping_interval_sec, 1),
    ok = application:start(game_server).

cleanup(_) ->
    ok = application:stop(game_server),
    ok = application:unload(game_server),
    ok = application:stop(ranch).

before_test() ->
    error_logger:tty(false).


accept_connection_test_() ->
    fixture(
        fun(_) ->
            ConnectResult = gen_tcp:connect("localhost", 7891, []),
            case ConnectResult of
                {ok, Socket} ->
                    gen_tcp:close(Socket);
                _ ->
                    ok
            end,
            [
                ?_assertMatch({ok, _}, ConnectResult)
            ]
        end
    ).

server_ping_test_() ->
    fixture(
        fun(_) ->
            {ok, Socket} = gen_tcp:connect("localhost", 7891, [{active, false}, binary]),
            ReadResult = gen_tcp:recv(Socket, 4, 1000),
            [
                ?_assertMatch({ok, ?PING_PACKET}, ReadResult)
            ]
        end
    ).

server_lost_ping_test_() ->
    fixture(
        fun(_) ->
            {ok, Socket} = gen_tcp:connect("localhost", 7891, [{active, false}, binary]),
            PingsCount = collect_pings(Socket, 1500),
            SendResult = gen_tcp:send(Socket, ?PING_PACKET(0)),
            ok = gen_tcp:close(Socket),
            [
                ?_assertEqual(5, PingsCount),
                ?_assertMatch({error, closed}, SendResult)
            ]
        end
    ).

late_reply_ping_test_() ->
    fixture(
        fun(_) ->
            {ok, Socket} = gen_tcp:connect("localhost", 7891, [binary]),

            lists:foreach(
                fun(_) ->
                    {ok, _} = wait_ping(Socket, 1500)
                end, lists:seq(1,3)
            ),

            {_, PingsReceived} =
                lists:foldl(
                    fun
                        (SeqId, {continue, Acc}) ->
                            case gen_tcp:send(Socket, ?PING_PACKET(SeqId)) of
                                ok ->
                                    case wait_ping(Socket, 1500) of
                                        {ok, _} ->
                                            {continue, Acc+1};
                                        _ ->
                                            {stop, Acc}
                                    end;
                                _ ->
                                    {stop, Acc}
                            end;
                        (_, {stop, Acc}) ->
                            {stop, Acc}
                    end, {continue, 0}, lists:seq(1, 10)
                ),

            ok = gen_tcp:close(Socket),

            [
                ?_assertEqual(10, PingsReceived)
            ]

        end
    ).

errorneous_ping_test_() ->
    fixture(
        fun(_) ->
            {ok, Socket} = gen_tcp:connect("localhost", 7891, [binary]),
            {ok, ?PING_PACKET(0)} = wait_ping(Socket, 1500),
            ok = gen_tcp:send(Socket, ?PING_PACKET(3)),
            CloseResult = wait_close(Socket, 1500),
            ok = gen_tcp:close(Socket),

            [
                ?_assertEqual(ok, CloseResult)
            ]
        end
    ).

wait_close(Socket, Timeout) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp_closed, Socket} ->
            ok
    after Timeout ->
        {error, timeout}
    end.


collect_pings(Socket, Timeout) ->
    collect_pings(Socket, Timeout, 0).

collect_pings(Socket, Timeout, Count) ->
    case wait_ping(Socket, Timeout) of
        {ok, _} ->
            collect_pings(Socket, Timeout, Count+1);
        {error, timeout} ->
            Count
    end.


wait_ping(Socket, Timeout) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, ?PING_PACKET = PingPacket} ->
            {ok, PingPacket}
    after Timeout ->
        {error, timeout}
    end.

after_test() ->
    error_logger:tty(false).