
-module(game_server_sup).
-author("Viacheslav V. Kovalev").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    PasswordManagerParams = get_password_manager_params(),
    Children = [
        {password_manager, {password_manager, start_link, [PasswordManagerParams]},
            Restart, Shutdown, Type, [password_manager]},

        {client_session_sup, {client_session_sup, start_link, []},
            Restart, Shutdown, supervisor, [client_session_sup]}

    ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_password_manager_params() ->
    {ok, TimeoutSec} = application:get_env(game_server, password_reset_timeout),
    [
        {password_reset_timeout, TimeoutSec}
    ].