
-module(lobby_utils).
-author("Viacheslav V. Kovalev").

-include_lib("game_lobby/include/common.hrl").

%% Helpers
-export([
    random_token/0
]).

%% Test utils
-export([
    wait_from_pid/2,
    wait_game_start/0,
    wait_game_start/1,
    wait_game_start/2,
    wait_process_down/2,
    wait_peer_lost/2,
    wait_peer_reset/2,
    wait_peer_turn/2,
    wait_peer_turn_fail/2,
    wait_peer_change/2
]).

random_token() ->
    make_ref().


wait_from_pid(Pid, Timeout) ->
    receive
        {Pid, Message} ->
            {ok, Message}
    after Timeout ->
        {error, timeout}
    end.

wait_game_start(Token, Timeout) ->
    receive
        #game_start{token = Token} = Val ->
            {ok, Val}
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
