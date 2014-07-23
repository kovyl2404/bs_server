-module(game_lobby).

-behaviour(application).

-author("Viacheslav V. Kovalev").

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
    ok = application:start(game_lobby).

stop() ->
    ok = application:stop(game_lobby),
    ok = stop_deps().

