
-module(game_session).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    start_link/2,
    set_peer/2,
    make_turn/3
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
        reconnect_timers    = [],
        peer_tags           = [],
        peer_queue          = []
    }
).

%%%===================================================================
%%% API
%%%===================================================================


start_link(FirstPeer, SecondPeer) ->
    gen_server:start_link(
        ?MODULE, {FirstPeer, SecondPeer}, []
    ).

set_peer(SessionPid, PeerId) ->
    gen_server:cast(SessionPid, {set_peer, PeerId}).

make_turn(SessionPid, PeerTag, Data) ->
    gen_server:cast(SessionPid, {turn, PeerTag, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init({
    #peer_id{client_pid = FirstPid, tag = FirstTag},
    #peer_id{ client_pid = SecondPid, tag = SecondTag}
}) ->
    erlang:monitor(process, FirstPid),
    erlang:monitor(process, SecondPid),
    FirstPid ! #game_start{tag = FirstTag, session_pid = self()},
    SecondPid ! #game_start{ tag = SecondTag, session_pid = self()},
    {ok, #state{
        peer_tags = orddict:from_list([
            {FirstTag, FirstPid}, {SecondTag, SecondPid}
        ]),
        peer_queue = [ FirstTag, SecondTag ]
    }}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(
    {set_peer, #peer_id{tag = Tag, client_pid = NewPeerPid}},
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers
    } = State
) ->
    erlang:monitor(process, NewPeerPid),
    OldPid = orddict:fetch(Tag, PeerTags),
    OldPid ! #peer_change{session_pid = self()},
    NewPeerPid ! #game_start{session_pid = self(), tag = Tag},
    [ P ! #peer_reset{session_pid = self() } || {T, P} <- PeerTags, T =/= Tag],
    {noreply, State#state{
        peer_tags = orddict:store( Tag, NewPeerPid, PeerTags ),
        reconnect_timers = orddict:erase(Tag, ReconnectTimers)
    }};

handle_cast(
    {turn, CurrentPeerTag, Data},
    #state{
        peer_queue = [ CurrentPeerTag, NextPeerTag ],
        peer_tags = PeerTags
    } = State
) ->
    NextPeerPid = orddict:fetch(NextPeerTag, PeerTags),
    NextPeerPid ! #peer_turn{session_pid = self(), data = Data},
    {noreply, State#state{
        peer_queue = [NextPeerTag, CurrentPeerTag]
    }};

handle_cast(
    {turn, CurrentPeerTag, _Data},
    #state{
        peer_tags = PeerTags,
        peer_queue = [ ActualPeerTag, FailedPeerTag ],
        reconnect_timers = ReconnectTimers
    } = State
) when FailedPeerTag =:= CurrentPeerTag->
    ActualPeerPid = orddict:fetch(ActualPeerTag, PeerTags),
    ActualPeerPid ! #peer_lost{session_pid = self()},

    FailedPeerPid = orddict:fetch(FailedPeerTag, PeerTags),
    FailedPeerPid ! #illegal_turn{session_pid = self()},
    TimerId = make_ref(),
    erlang:send_after(2000, self(), {reconnect_timeout, FailedPeerTag, TimerId}),
    {noreply, State#state{
        reconnect_timers = orddict:store(FailedPeerTag, TimerId, ReconnectTimers)
    }};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(
    {'DOWN', _, process, Pid, _Reason},
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers
    } = State
) ->
    {Tag, Pid} = lists:keyfind(Pid, 2, PeerTags),
    [ P ! #peer_lost{session_pid = self()} || {T, P} <- PeerTags, T =/= Tag],
    TimerId = make_ref(),
    erlang:send_after(2000, self(), {reconnect_timeout, Tag, TimerId}),
    {noreply, State#state{
        reconnect_timers = orddict:store(Tag, TimerId, ReconnectTimers)
    }};

handle_info(
    {reconnect_timeout, Tag, TimerId},
    #state{
        reconnect_timers = ReconnectTimers
    } = State
) ->
    case orddict:find(Tag, ReconnectTimers) of
        {ok, TimerId} ->
            {stop, normal, State};
        _ ->
            {noreply, State}
    end;


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


