
-module(lobby_server).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").
-include_lib("game_lobby/include/logging.hrl").
-include_lib("game_lobby/include/metrics.hrl").

%% API
-export([
    start_link/0,
    checkin/2,
    checkin/4,
    cancel/1
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(
    state, {
        waiting_client = undefined,
        monitors_table
    }
).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


checkin(ClientPid, ClientLabel) ->
    safe_call(?SERVER, {checkin, {ClientPid, ClientLabel}}).

checkin(ClientPid, ClientLabel, GameToken, PeerTag) ->
    safe_call(?SERVER, {checkin, {ClientPid, ClientLabel}, GameToken, PeerTag}).

cancel(GameToken) ->
    safe_call(?SERVER, {cancel, GameToken }).


safe_call(Server, Request) ->
    case (catch gen_server:call(Server, Request)) of
        {'EXIT', _} -> {error, server_fault};
        Other -> Other
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    ets:new(?MODULE, [protected, set, named_table]),
    {ok, #state{
        monitors_table = ets:new(session_monitors, [protected, set])
    }}.


handle_call(
    {checkin, {ClientPid, ClientLabel}},
    _From,
    #state{
        waiting_client = undefined
    } = State
) ->
    folsom_metrics:notify({?START_GAME_REQUESTS_METRIC, 1}),
    folsom_metrics:notify({?WAITING_GAMES_METRIC, {inc, 1}}),
    WaitingMonitor = monitor(process, ClientPid),
    Token = lobby_utils:random_token(),
    ?NOTICE("User ~s waiting for game ~p",[ClientLabel, Token]),
    {reply, {ok, Token}, State#state{
        waiting_client = {ClientPid, Token, WaitingMonitor, ClientLabel}
    }};


handle_call(
    {checkin, {NewPid, ClientLabel}},
    _From,
    #state{
        waiting_client = {WaitingPid, WaitingToken, WaitingMonitor, WaitingClientLabel},
        monitors_table = MonitorsTable
    } = State
) ->
    ?NOTICE("User ~s will play with ~p in game ~p",[ClientLabel, WaitingClientLabel, WaitingToken]),
    FirstTag = lobby_utils:random_token(),
    SecondTag = lobby_utils:random_token(),
    demonitor(WaitingMonitor, [flush]),
    {ok, SessionPid} =
        supervisor:start_child(game_session_sup, [
            WaitingToken,
            #peer_id{client_pid = WaitingPid, tag = FirstTag, client_label = WaitingClientLabel},
            #peer_id{client_pid = NewPid, tag = SecondTag, client_label = ClientLabel}
        ]),
    folsom_metrics:notify({?RUNNING_GAMES_METRIC, {inc, 1}}),
    folsom_metrics:notify({?WAITING_GAMES_METRIC, {dec, 1}}),
    ?INFO("Starting game session ~p for game ~p",[SessionPid, WaitingToken]),
    MonitorRef = monitor(process, SessionPid),
    true = ets:insert(?SERVER, {WaitingToken, SessionPid}),
    true = ets:insert(MonitorsTable, {MonitorRef, WaitingToken}),
    {reply, {ok, WaitingToken}, State#state{
        waiting_client = undefined
    }};


%% TODO: Consider to avoid server call. Use direct ets:lookup from client instead
handle_call(
    {checkin, {ClientPid, ClientLabel}, GameToken, PeerTag},
    _From,
    #state{

    } = State
) ->
    ?DEBUG("Client session ~p (~s) asked to reconnect game ~p",[ClientPid, ClientLabel, GameToken]),
    case ets:lookup(?SERVER, GameToken) of
        [{_, SessionPid}] ->
            ?DEBUG("Game ~p still exists in game session ~p",[GameToken, SessionPid]),
            case game_session:set_peer(SessionPid, #peer_id{ client_pid = ClientPid, tag = PeerTag, client_label = ClientLabel}) of
                ok ->
                    folsom_metrics:notify({?SUCCEEDED_RECONNECTIONS_METRIC, 1}),
                    {reply, {ok, GameToken}, State};
                {error, _Reason} = Error ->
                    folsom_metrics:notify({?FAILED_RECONNECTIONS_METRIC, 1}),
                    {reply, Error, State}
            end;
        _ ->
            folsom_metrics:notify({?FAILED_RECONNECTIONS_METRIC, 1}),
            ?DEBUG("Game ~p not found. Session expired.",[GameToken]),
            {reply, {error, session_expired }, State}
    end;

handle_call(
    {cancel, Token},
    _From,
    #state{
        waiting_client = {WaitingClientPid, WaitingClientToken, WaitingMonitor, _ClientLabel}
    } = State
) when Token =:= WaitingClientToken ->
    folsom_metrics:notify({?CANCELLED_WAITING_GAMES_METRIC, 1}),
    folsom_metrics:notify({?WAITING_GAMES_METRIC, {dec, 1}}),
    ?DEBUG("Waiting client ~p (~s) asked to cancel game ~p",[WaitingClientPid, _ClientLabel, WaitingClientToken]),
    demonitor(WaitingMonitor, [flush]),
    WaitingClientPid ! #game_stop{ token = Token },
    {reply, {ok, Token}, State#state{waiting_client = undefined }};

handle_call(
    {cancel, Token},
    _From,
    #state{

    } = State
) ->
    ?DEBUG("Client asked to cancel game ~p",[Token]),
    folsom_metrics:notify({?CANCELLED_GAMES_METRIC, 1}),
    Reply =
        case ets:lookup(?SERVER, Token) of
            [{Token, SessionPid}] ->
                ?DEBUG("Game ~p found in game session ~p",[Token, SessionPid]),
                game_session:stop_game(SessionPid),
                {ok, Token};
            _ ->
                ?DEBUG("Game ~p not found. Session expired.",[Token]),
                {error, session_expired}
        end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(
    {'DOWN', MonitorRef, process, WaitingPid, _Reason},
    #state{
        waiting_client = {WaitingPid, _WaitingToken, MonitorRef}
    } = State
) ->
    folsom_metrics:notify({?WAITING_GAMES_METRIC, {dec, 1}}),
    ?DEBUG("Waiting client session ~p (~p) down with reason ~p.",[WaitingPid, _WaitingToken, _Reason]),
    {noreply, State#state{ waiting_client = undefined }};

handle_info(
    {'DOWN', MonitorRef, process, _, _},
    #state{
        monitors_table = MonitorsTable
    } = State
) ->
    folsom_metrics:notify({?RUNNING_GAMES_METRIC, {dec, 1}}),
    [ {_, Tag} ] = ets:lookup(MonitorsTable, MonitorRef),
    true = ets:delete(?SERVER, Tag),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
