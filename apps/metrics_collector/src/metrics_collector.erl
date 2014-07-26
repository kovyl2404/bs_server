-module(metrics_collector).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(metrics_collector).

start(_StartType, _StartArgs) ->
    metrics_collector_sup:start_link().

stop(_State) ->
    ok.
