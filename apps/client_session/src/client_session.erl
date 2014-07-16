-module(client_session).

-author("Viacheslav V. Kovalev").

-behaviour(application).

-define(CLIENT_SESSION_DEPS, [gproc, ranch]).
-define(DEFAULT_ACCEPTORS_COUNT, 100).

%% Application callbacks
-export([
    start/0,
    start/2, stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = start_deps(),
    ok = application:start(client_session).

start(_StartType, _StartArgs) ->
    {ok, _ListenerPid} =
        ranch:start_listener(
            ?MODULE, ?DEFAULT_ACCEPTORS_COUNT, ranch_tcp, transport_options(),
            connection_protocol, []
        ),
    client_session_sup:start_link().

stop(_State) ->
    ok.




start_deps() ->
    lists:foreach(
        fun(App) ->
            ok = application:start(App)
        end, ?CLIENT_SESSION_DEPS
    ).


transport_options() ->
    {ok, Port} = application:get_env(client_session, port),
    [{port, Port}, {nodelay, false}].