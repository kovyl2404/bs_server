-ifndef(GAME_LOBBY_COMMON_HRL).
-define(GAME_LOBBY_COMMON_HRL, ok).
-author("Viacheslav V. Kovalev").

-record(
    client_wait, {
        key             = erlang:error(required, key)
    }
).

-record(
    client_connected, {
        key             = erlang:error(required, key),
        client_info     = erlang:error(required, client_info)
    }
).

-record(
    client_down, {
        key             = erlang:error(required, key),
        client_info     = erlang:error(required, client_info),
        reason          = erlang:error(required, reason)        :: atom()
    }
).

-endif.


