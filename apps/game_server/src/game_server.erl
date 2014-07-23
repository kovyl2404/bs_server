-module(game_server).

-author("Viacheslav V. Kovalev").

-behaviour(application).

-include_lib("game_server/include/logging.hrl").

-define(
    CLIENT_SESSION_DEPS,
    [
        game_lobby
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