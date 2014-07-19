
-module(lobby_server).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    start_link/0,
    checkin/1,
    checkin/3,
    cancel/2
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


checkin(ClientPid) ->
    safe_call(?SERVER, {checkin, ClientPid}).

checkin(ClientPid, GameToken, PeerTag) ->
    safe_call(?SERVER, {checkin, ClientPid, GameToken, PeerTag}).

cancel(ClientPid, GameToken) ->
    safe_call(?SERVER, {cancel, ClientPid, GameToken }).


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
    {checkin, ClientPid},
    _From,
    #state{
        waiting_client = undefined
    } = State
) ->
    Token = lobby_utils:random_token(),
    {reply, {ok, Token}, State#state{
        waiting_client = {ClientPid, Token}
    }};


handle_call(
    {checkin, NewPid},
    _From,
    #state{
        waiting_client = {WaitingPid, WaitingToken},
        monitors_table = MonitorsTable
    } = State
) ->
    FirstTag = lobby_utils:random_token(),
    SecondTag = lobby_utils:random_token(),

    {ok, SessionPid} =
        supervisor:start_child(game_session_sup, [
            WaitingToken,
            #peer_id{client_pid = WaitingPid, tag = FirstTag},
            #peer_id{client_pid = NewPid, tag = SecondTag}
        ]),
    MonitorRef = monitor(process, SessionPid),
    true = ets:insert(?SERVER, {WaitingToken, SessionPid}),
    true = ets:insert(MonitorsTable, {MonitorRef, WaitingToken}),
    {reply, {ok, WaitingToken}, State#state{
        waiting_client = undefined
    }};


%% TODO: Consider to avoid server call. Use direct ets:lookup from client instead
handle_call(
    {checkin, ClientPid, GameToken, PeerTag},
    _From,
    #state{

    } = State
) ->
    case ets:lookup(?SERVER, GameToken) of
        [{_, SessionPid}] ->
            case game_session:set_peer(SessionPid, #peer_id{ client_pid = ClientPid, tag = PeerTag}) of
                ok ->
                    {reply, {ok, GameToken}, State};
                {error, internal_fault} = Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {error, session_expired }, State}
    end;

handle_call(
    {cancel, ClientPid, Token},
    _From,
    #state{
        waiting_client = {WaitingClientPid, WaitingClientToken}
    } = State
) when ClientPid =:= WaitingClientPid andalso Token =:= WaitingClientToken ->
    ClientPid ! #game_stop{ token = Token },
    {reply, {ok, Token}, State#state{waiting_client = undefined }};

handle_call(
    {cancel, ClientPid, Token},
    _From,
    #state{

    } = State
) ->
    Reply =
        case ets:lookup(?SERVER, Token) of
            [{Token, SessionPid}] ->
                game_session:stop_game(SessionPid, ClientPid),
                {ok, Token};
            _ ->
                {error, session_expired}
        end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(
    {'DOWN', MonitorRef, process, _, _},
    #state{
        monitors_table = MonitorsTable
    } = State
) ->
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
