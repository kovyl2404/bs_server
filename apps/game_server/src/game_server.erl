-module(game_server).

-author("Viacheslav V. Kovalev").

-behaviour(application).

-include_lib("game_server/include/logging.hrl").
-include_lib("game_server/include/metrics.hrl").

-define(
    CLIENT_SESSION_DEPS,
    [
        ranch, game_lobby
    ]
).
-define(DEFAULT_ACCEPTORS_COUNT, 100).

%% Application callbacks
-export([
    start/2, stop/1
]).

%% Helpers
-export([
   start/0, stop/0
]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = start_deps(),
    ok = application:start(game_server).

stop() ->
    ok = application:stop(game_server),
    ok = stop_deps().

start(_StartType, _StartArgs) ->
    ok = init_metrics(),
    TransportOptions = transport_options(),
    StartResult =
        ranch:start_listener(
            ?MODULE, ?DEFAULT_ACCEPTORS_COUNT, ranch_tcp, transport_options(),
            client_connection, connection_options()
        ),
    case StartResult of
        {ok, _} ->
            ?WARNING("New game server started with transport ~p",[TransportOptions]),
            client_session_sup:start_link();
        Error ->
            ?CRITICAL("Could not start game server with transport ~p because of ~p",[TransportOptions, Error]),
            Error
    end.


stop(_State) ->
    ok.




start_deps() ->
    lists:foreach(
        fun(App) ->
            ok = application:start(App)
        end, ?CLIENT_SESSION_DEPS
    ).

stop_deps() ->
    lists:foreach(
        fun(App) ->
            ok = application:stop(App),
            ok = application:unload(App)
        end, lists:reverse(?CLIENT_SESSION_DEPS)
    ).


transport_options() ->
    {ok, Port} = application:get_env(game_server, port),
    [{port, Port}, {nodelay, false}].

connection_options() ->
    {ok, PingIntervalSec} = application:get_env(game_server, ping_interval_sec),
    {ok, MaxPingsAllowed} = application:get_env(game_server, max_pings_allowed),
    [
        {ping_interval_sec, PingIntervalSec},
        {max_pings_allowed, MaxPingsAllowed}
    ].


init_metrics() ->
    ok = folsom_metrics:new_counter(?GAME_SERVER_CONNECTIONS_METRIC),
    ok = folsom_metrics:new_counter(?GAME_SERVER_GUEST_CONNECTIONS_METRIC),
    ok = folsom_metrics:new_counter(?GAME_SERVER_AUTHENTICATED_CONNECTIONS_METRIC),
    ok = folsom_metrics:new_meter(?GAME_SERVER_PROTOCOL_VIOLATIONS),

    ok = folsom_metrics:tag_metric(?GAME_SERVER_CONNECTIONS_METRIC, ?GAME_SERVER_METRICS),
    ok = folsom_metrics:tag_metric(?GAME_SERVER_GUEST_CONNECTIONS_METRIC, ?GAME_SERVER_METRICS),
    ok = folsom_metrics:tag_metric(?GAME_SERVER_AUTHENTICATED_CONNECTIONS_METRIC, ?GAME_SERVER_METRICS),
    ok = folsom_metrics:tag_metric(?GAME_SERVER_PROTOCOL_VIOLATIONS, ?GAME_SERVER_METRICS).