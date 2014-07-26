-ifndef(GAME_LOBBY_METRICS_HRL).
-define(GAME_LOBBY_METRICS_HRL, ok).
-author("Viacheslav V. Kovalev").

-define(GAME_LOBBY_METRICS, game_lobby).

-define(RUNNING_GAMES_METRIC, "game_lobby.running_games").
-define(START_GAME_REQUESTS_METRIC, "game_lobby.start_game_requests").
-define(CANCELLED_GAMES_METRIC, "game_lobby.cancelled_games.running").
-define(CANCELLED_WAITING_GAMES_METRIC, "game_lobby.cancelled_games.waiting").
-define(SUCCEEDED_RECONNECTIONS_METRIC, "game_lobby.reconnections.succeeded").
-define(FAILED_RECONNECTIONS_METRIC, "game_lobby.reconnections.failed").

-define(TIMEDOUT_GAMES_METRIC, "game_lobby.timed_out_games").

-endif.
