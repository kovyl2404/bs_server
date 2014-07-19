
-module(game_session).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    start_link/3,
    set_peer/2,
    make_turn/3,
    ack_turn/3,
    stop_game/2
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
        game_token          = erlang:error(required, token),
        reconnect_timers    = [],
        peer_tags           = [],
        peer_queue          = [],
        cur_turn            = undefined
    }
).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Token, FirstPeer, SecondPeer) ->
    gen_server:start_link(
        ?MODULE, {Token, FirstPeer, SecondPeer}, []
    ).

set_peer(SessionPid, PeerId) ->
    case (catch gen_server:call(SessionPid, {set_peer, PeerId})) of
        {'EXIT', _} ->
            {error, session_expired };
        ok ->
            ok
    end.

make_turn(SessionPid, PeerTag, Data) ->
    gen_server:cast(SessionPid, {turn, PeerTag, Data}).

ack_turn(SessionPid, PeerTag, Data) ->
    gen_server:cast(SessionPid, {ack_turn, PeerTag, Data}).

stop_game(SessionPid, PeerTag) ->
    gen_server:cast(SessionPid, {stop_game, PeerTag}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init({
    Token,
    #peer_id{client_pid = FirstPid, tag = FirstTag},
    #peer_id{ client_pid = SecondPid, tag = SecondTag}
}) ->
    erlang:monitor(process, FirstPid),
    erlang:monitor(process, SecondPid),
    FirstPid ! #game_start{tag = FirstTag, session_pid = self(), token = Token},
    SecondPid ! #game_start{ tag = SecondTag, session_pid = self(), token = Token},
    {ok, #state{
        game_token = Token,
        peer_tags = orddict:from_list([
            {FirstTag, FirstPid}, {SecondTag, SecondPid}
        ]),
        peer_queue = [ FirstTag, SecondTag ]
    }}.


handle_call(
    {set_peer, #peer_id{tag = Tag, client_pid = NewPeerPid}},
    _From,
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers,
        cur_turn = CurTurn,
        game_token = Token
    } = State
) ->
    erlang:monitor(process, NewPeerPid),
    OldPid = orddict:fetch(Tag, PeerTags),
    OldPid ! #peer_change{session_pid = self()},
    NewPeerPid ! #game_start{session_pid = self(), tag = Tag, token = Token},
    [ P ! #peer_reset{session_pid = self() } || {T, P} <- PeerTags, T =/= Tag],
    ok = consider_repeat_turn(CurTurn, Tag, NewPeerPid),
    {reply, ok, State#state{
        peer_tags = orddict:store( Tag, NewPeerPid, PeerTags ),
        reconnect_timers = orddict:erase(Tag, ReconnectTimers)
    }}.


handle_cast(
    {ack_turn, PeerTag, Data},
    #state{
        peer_queue = [ CurrentPeerTag, NextPeerTag ],
        cur_turn = CurTurn
    } = State
) when CurTurn =:= {PeerTag, Data} ->
    {noreply, State#state{
        peer_queue = [NextPeerTag, CurrentPeerTag],
        cur_turn = undefined
    }};

handle_cast( {ack, _, _}, State ) ->
    {noreply, State};

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
        cur_turn = {NextPeerTag, Data}
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

handle_cast(
    {stop_game, _Tag},
    #state{
        peer_tags = PeerTags,
        game_token = Token
    } = State
) ->
%%     case orddict:find(Tag, PeerTags) of
%%
%%     end,
    [ P ! #game_stop{
        session_pid = self(),
        token = Token,
        tag = T
    } || {T, P} <- PeerTags ],
    {stop, normal, State};

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


consider_repeat_turn({Tag, Data}, Tag, NewPeerPid) ->
    NewPeerPid ! #peer_turn{session_pid = self(), data = Data},
    ok;
consider_repeat_turn(_, _Tag, _NewPeerPid) ->
    ok.