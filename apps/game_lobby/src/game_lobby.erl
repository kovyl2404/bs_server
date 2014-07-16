-module(game_lobby).

-behaviour(application).

-author("Viacheslav V. Kovalev").

%% Application callbacks
-export([start/2, stop/1]).


-export([checkin/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    game_lobby_sup:start_link().

stop(_State) ->
    ok.



checkin(UserInfo, ClientPid) ->
    lobby_server:checkin(UserInfo, ClientPid).