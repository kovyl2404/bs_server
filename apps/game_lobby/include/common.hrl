-ifndef(GAME_LOBBY_COMMON_HRL).
-define(GAME_LOBBY_COMMON_HRL, ok).
-author("Viacheslav V. Kovalev").


-record(
    peer_id, {
        client_pid,
        tag,
        client_label
    }
).


-record(
    game_start, {
        token,
        session_pid,
        tag,
        turn
    }
).

-record(
    game_stop, {
        token,
        session_pid,
        tag
    }
).

-record(
    peer_lost, {
        session_pid
    }
).

-record(
    peer_reset, {
        session_pid
    }
).

-record(
    peer_change, {
        session_pid
    }
).

-record(
    peer_turn, {
        session_pid,
        data
    }
).

-record(
    peer_surrender, {
        session_pid,
        data
    }
).

-record(
    illegal_turn, {
        session_pid
    }
).

-endif.


