-module(game_lobby).

-behaviour(application).

-author("Viacheslav V. Kovalev").

-include_lib("game_lobby/include/metrics.hrl").

%% Interface functions
-export([
    checkin/2,
    checkin/4,
    cancel/1
]).

%% Application callbacks
-export([
    start/2, stop/1
]).

%% Helpers
-export([
    start/0, stop/0,
    start_deps/0, stop_deps/0
]).


%% ===================================================================
%% Interface functions
%% ===================================================================

checkin(ClientPid, ClientLabel) ->
    lobby_server:checkin(ClientPid, ClientLabel).

checkin(ClientPid, ClientLabel, SessionToken, SessionTag) ->
    lobby_server:checkin(ClientPid, ClientLabel, SessionToken, SessionTag).


cancel(Token) ->
    lobby_server:cancel(Token).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = init_metrics(),
    game_lobby_sup:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Helpers
%% ===================================================================

start_deps() ->
    ok.

stop_deps() ->
    ok.

start() ->
    ok = start_deps(),
    ok = application:load(game_lobby),
    ok = application:start(game_lobby).

stop() ->
    ok = application:stop(game_lobby),
    ok = application:unload(game_lobby),
    ok = stop_deps().

init_metrics() ->
    ok = folsom_metrics:new_counter(?RUNNING_GAMES_METRIC),
    ok = folsom_metrics:new_counter(?WAITING_GAMES_METRIC),
    ok = folsom_metrics:new_meter(?START_GAME_REQUESTS_METRIC),
    ok = folsom_metrics:new_meter(?CANCELLED_GAMES_METRIC),
    ok = folsom_metrics:new_meter(?CANCELLED_WAITING_GAMES_METRIC),
    ok = folsom_metrics:new_meter(?SUCCEEDED_RECONNECTIONS_METRIC),
    ok = folsom_metrics:new_meter(?FAILED_RECONNECTIONS_METRIC),
    ok = folsom_metrics:new_meter(?TIMEDOUT_GAMES_METRIC),

    ok = folsom_metrics:tag_metric(?RUNNING_GAMES_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?WAITING_GAMES_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?START_GAME_REQUESTS_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?CANCELLED_GAMES_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?CANCELLED_WAITING_GAMES_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?SUCCEEDED_RECONNECTIONS_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?FAILED_RECONNECTIONS_METRIC, ?GAME_LOBBY_METRICS),
    ok = folsom_metrics:tag_metric(?TIMEDOUT_GAMES_METRIC, ?GAME_LOBBY_METRICS).

