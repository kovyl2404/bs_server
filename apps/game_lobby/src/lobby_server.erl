
-module(lobby_server).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").

-define(
    UNEXPECTED_CALL(_, State),
    {stop, unexpected_call, unexpected_call, State}
).

-define(
    UNEXPECTED_CAST(_, State),
    {stop, unexpected_cast, State}
).

-define(
    UNEXPECTED_INFO(_, State),
    {stop, unexpected_info, State}
).

%% API
-export([
    start_link/0,
    checkin/3,
    checkin/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(
    peer_info, {
        client_info,
        client_pid
    }
).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

checkin(ClientInfo, ClientPid) ->
    checkin(ClientInfo, ClientPid, []).

checkin(ClientInfo, ClientPid, Params) ->
    case (catch gen_server:call(?SERVER, {checkin, ClientInfo, ClientPid, Params})) of
        {ok, Ref} ->
            {ok, Ref};
        {error, Reason} ->
            {error, Reason};
        {'EXIT', VerboseReason} ->
            {error, translate_verbose_reason(VerboseReason)}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(_) ->
    ets:new(?MODULE, [protected, named_table, ordered_set]),
    {ok, #state{}}.

handle_call(
    {checkin, ClientInfo, ClientPid, Params}, _From,
    State
) ->
    ThisPeerKey = {erlang:now(), make_ref()},
    ThisPeerInfo = #peer_info{ client_info = ClientInfo, client_pid = ClientPid },
    case select_appropriate_peer(Params) of
        {ok, RemotePeerKey, RemotePeerInfo} ->
            ok = proceed_checkin(ThisPeerKey, ThisPeerInfo, RemotePeerKey, RemotePeerInfo);
        not_found ->
            suspend_checkin(ThisPeerKey, ThisPeerInfo, Params)
    end,
    {reply, {ok, ThisPeerKey}, State};


handle_call(Request, _From, State) ->
    ?UNEXPECTED_CALL(Request, State).


handle_cast(Request, State) ->
    ?UNEXPECTED_CAST(Request, State).


handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info, State).


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

select_appropriate_peer(_Params) ->
    case ets:first(?MODULE) of
        {PeerKey, PeerInfo, _Params} ->
            {ok, PeerKey, PeerInfo};
        '$end_of_table' ->
            not_found
    end.

proceed_checkin(
    ThisPeerKey,
    #peer_info{client_pid = ThisPid, client_info = ThisClientInfo},
    RemotePeerKey,
    #peer_info{client_pid = RemotePid, client_info = RemoteClientInfo}
) ->
    true = ets:delete(?MODULE, RemotePeerKey),
    erlang:send(ThisPid, #client_connected{client_info = RemoteClientInfo, key = ThisPeerKey}),
    erlang:send(RemotePid, #client_connected{client_info = ThisClientInfo, key = RemotePeerKey}),
    ok.

suspend_checkin(PeerKey, #peer_info{client_pid = ClientPid} = PeerInfo, Params) ->
    true = ets:insert(?MODULE, {PeerKey, PeerInfo, Params}),
    erlang:send(ClientPid, #client_wait{ key = PeerKey }).


translate_verbose_reason(_) ->
    lobby_down.