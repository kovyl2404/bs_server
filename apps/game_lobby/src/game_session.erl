
-module(game_session).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_lobby/include/common.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("game_lobby/include/logging.hrl").
-include_lib("game_lobby/include/metrics.hrl").

-define(DEFAULT_RECONNECT_TIMEOUT, 2000).   

%% API
-export([
    start_link/3,
    set_peer/2,
    make_turn/3,
    surrender/3,
    stop_game/1
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
        game_token               = erlang:error(required, token),
        reconnect_timers         = [],
        peer_tags                = [],
        peer_queue               = [],
        last_turn                = undefined,
        is_surrender_negotiation = false,
        peer_labels              = []
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
        Other ->
            Other
    end.

make_turn(SessionPid, PeerTag, Data) ->
    gen_server:cast(SessionPid, {turn, PeerTag, Data}).

surrender(SessionPid, PeerTag, SurrenderData) ->
    gen_server:cast(SessionPid, {surrender, PeerTag, SurrenderData}).

stop_game(SessionPid) ->
    gen_server:cast(SessionPid, stop_game).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init({
    Token,
    #peer_id{client_pid = FirstPid, tag = FirstTag, client_label = FirstLabel},
    #peer_id{ client_pid = SecondPid, tag = SecondTag, client_label = SecondLabel}
}) ->
    erlang:monitor(process, FirstPid),
    erlang:monitor(process, SecondPid),
    FirstPid ! #game_start{tag = FirstTag, session_pid = self(), token = Token, turn = true },
    SecondPid ! #game_start{ tag = SecondTag, session_pid = self(), token = Token, turn = false },
    ?INFO("New game started for ~s and ~s",[FirstLabel, SecondLabel]),
    {ok, #state{
        game_token = Token,
        peer_tags = orddict:from_list([
            {FirstTag, FirstPid}, {SecondTag, SecondPid}
        ]),
        peer_queue = [ FirstTag, SecondTag ],
        peer_labels = orddict:from_list([
            {FirstTag, FirstLabel}, {SecondTag, SecondLabel}
        ]),
        last_turn = undefined
    }}.


handle_call(
    {set_peer, #peer_id{tag = Tag, client_pid = NewPeerPid}},
    _From,
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers,
        last_turn = LastTurn,
        game_token = Token,
        peer_queue = PeerQueue,
        peer_labels = PeerLabels
    } = State
) ->
    ThisPeerTurn =
        case PeerQueue of
            [Tag, _] -> true;
            _ -> false
        end,
    erlang:monitor(process, NewPeerPid),
    case orddict:find(Tag, PeerTags) of
        {ok, OldPid} ->
            Label = orddict:fetch(Tag, PeerLabels),
            ?INFO("User ~s was resconnected from game ~p",[Label, Token]),
            send_safe(OldPid, #peer_change{session_pid = self()}),
            [ send_safe(P, #peer_reset{session_pid = self() }) || {T, P} <- PeerTags, T =/= Tag],
            NewPeerPid ! #game_start{
                session_pid = self(),
                tag = Tag,
                token = Token,
                turn = (LastTurn == undefined andalso ThisPeerTurn)
            },
            ThisPeerTurn andalso consider_repeat_turn(LastTurn, NewPeerPid),
            {reply, ok, State#state{
                peer_tags = orddict:store( Tag, NewPeerPid, PeerTags ),
                reconnect_timers = orddict:erase(Tag, ReconnectTimers)
            }};
        _ ->
            {reply, {error, invalid_tag}, State}
    end.



handle_cast(
    {turn, CurrentPeerTag, Data},
    #state{
        peer_queue = [ CurrentPeerTag, NextPeerTag ],
        peer_tags = PeerTags,
        is_surrender_negotiation = false
    } = State
) ->
    NextPeerPid = orddict:fetch(NextPeerTag, PeerTags),
    TurnValue = #peer_turn{session_pid = self(), data = Data},
    send_safe(NextPeerPid, TurnValue),
    {noreply, State#state{
        last_turn = TurnValue,
        peer_queue = [ NextPeerTag, CurrentPeerTag ]
    }};

handle_cast(
    {turn, FailedPeerTag, _Data},
    #state{
        peer_tags = PeerTags,
        peer_queue = [ ActualPeerTag, FailedPeerTag ],
        reconnect_timers = ReconnectTimers,
        is_surrender_negotiation = false
    } = State
) when FailedPeerTag =:= FailedPeerTag->
    TimerId = handle_turn_violation(ActualPeerTag, FailedPeerTag, PeerTags),
    {noreply, State#state{
        reconnect_timers = orddict:store(FailedPeerTag, TimerId, ReconnectTimers),
        peer_tags = orddict:store(FailedPeerTag, undefined, PeerTags)
    }};

handle_cast(
    {turn, FailedPeerTag, _Data},
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers,
        is_surrender_negotiation = true,
        peer_queue = PeerQueue
    } = State
) ->
    ActualPeerTag =
        case PeerQueue of
            [FailedPeerTag, AnotherTag] -> AnotherTag;
            [AnotherTag, FailedPeerTag] -> AnotherTag
        end,
    TimerId = handle_turn_violation(ActualPeerTag, FailedPeerTag, PeerTags),
    {noreply, State#state{
        reconnect_timers = orddict:store(FailedPeerTag, TimerId, ReconnectTimers),
        peer_tags = orddict:store(FailedPeerTag, undefined, PeerTags)
    }};


%% Peer claims about its loss
handle_cast(
    {surrender, CurrentPeerTag, SurrenderData},
    #state{
        peer_queue = [ CurrentPeerTag, NextPeerTag ],
        peer_tags = PeerTags,
        is_surrender_negotiation = false,
        peer_labels = PeerLabels
    } = State
) ->
    CurrentLabel = orddict:fetch(CurrentPeerTag, PeerLabels),
    NextLabel = orddict:fetch(NextPeerTag, PeerLabels),
    ?INFO("~s surrendered to ~s",[CurrentLabel, NextLabel]),
    NextPeerPid = orddict:fetch(NextPeerTag, PeerTags),
    TurnValue = #peer_surrender{session_pid = self(), data = SurrenderData},
    send_safe(NextPeerPid, TurnValue),
    {noreply, State#state{
        peer_queue = [ NextPeerTag, CurrentPeerTag ],
        is_surrender_negotiation = true,
        last_turn = TurnValue
    }};

%% Peer acknowledges another peer surrender
handle_cast(
    {surrender, CurrentPeerTag, _SurrenderData},
    #state{
        game_token = Token,
        peer_queue = [ CurrentPeerTag, NextPeerTag ],
        peer_tags = PeerTags,
        is_surrender_negotiation = true,
        peer_labels = PeerLabels
    } = State
) ->
    CurrentLabel = orddict:fetch(CurrentPeerTag, PeerLabels),
    NextLabel = orddict:fetch(NextPeerTag, PeerLabels),
    ?INFO("~s acknowledged its win to ~s",[CurrentLabel, NextLabel]),
    [ send_safe(P, #game_stop{ session_pid = self(), token = Token, tag = T }) || {T, P} <- PeerTags ],
    {stop, normal, State};

handle_cast(
    {surrender, FailedPeerTag, _SurrenderData},
    #state{
        peer_tags = PeerTags,
        peer_queue = [ ActualPeerTag, FailedPeerTag ],
        reconnect_timers = ReconnectTimers
    } = State
) ->
    TimerId = handle_turn_violation(ActualPeerTag, FailedPeerTag, PeerTags),
    {noreply, State#state{
        reconnect_timers = orddict:store(FailedPeerTag, TimerId, ReconnectTimers),
        peer_tags = orddict:store(FailedPeerTag, undefined, PeerTags)
    }};

handle_cast(
    stop_game,
    #state{
        peer_tags = PeerTags,
        game_token = Token,
        peer_labels = [{_, FirstLabel}, {_, SecondLabel}]
    } = State
) ->
    ?INFO("Game ~p of ~s vs. ~s cancelled by peer",[Token, FirstLabel, SecondLabel]),
    [ send_safe(P, #game_stop{ session_pid = self(), token = Token, tag = T }) || {T, P} <- PeerTags ],
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(
    {'DOWN', _, process, Pid, _Reason},
    #state{
        peer_tags = PeerTags,
        reconnect_timers = ReconnectTimers,
        peer_labels = PeerLabels,
        game_token = Token
    } = State
) ->
    case lists:keyfind(Pid, 2, PeerTags) of
        {Tag, Pid} ->
            Label = orddict:fetch(Tag, PeerLabels),
            ?INFO("User ~s was disconnected from game ~p",[Label, Token]),
            [ send_safe(P, #peer_lost{session_pid = self()}) || {T, P} <- PeerTags, T =/= Tag ],
            TimerId = make_ref(),
            erlang:send_after(reconnect_timeout(), self(), {reconnect_timeout, Tag, TimerId}),
            {noreply, State#state{
                reconnect_timers = orddict:store(Tag, TimerId, ReconnectTimers),
                peer_tags = orddict:store(Tag, undefined, PeerTags)
            }};
        _ ->
            {noreply, State}
    end;

handle_info(
    {reconnect_timeout, Tag, TimerId},
    #state{
        reconnect_timers = ReconnectTimers,
        peer_labels = PeerLabels,
        game_token = Token
    } = State
) ->
    DisconnectedLabel = orddict:fetch(Tag, PeerLabels),
    [{_, FirstLabel}, {_, SecondLabel}] = PeerLabels,
    [ send_safe(P, #game_stop{session_pid = self(), token = Token, tag = T}) || {T, P} <- PeerTags, T =/= Tag ],
    case orddict:find(Tag, ReconnectTimers) of
        {ok, TimerId} ->
            folsom_metrics:notify({?TIMEDOUT_GAMES_METRIC, 1}),
            ?INFO("Game ~s vs ~s stopped because of ~s was not reconnected in time", [FirstLabel, SecondLabel, DisconnectedLabel]),
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


consider_repeat_turn(undefined, _NewPeerPid) ->
    ok;
consider_repeat_turn(Turn, NewPeerPid) ->
    NewPeerPid ! Turn,
    ok.


send_safe(undefined, _) ->
    ok;
send_safe(Pid, Message) when is_pid(Pid) ->
    erlang:send(Pid, Message),
    ok.

handle_turn_violation(ActualPeerTag, FailedPeerTag, PeerTags) ->
    ActualPeerPid = orddict:fetch(ActualPeerTag, PeerTags),
    send_safe(ActualPeerPid, #peer_lost{session_pid = self()}),
    FailedPeerPid = orddict:fetch(FailedPeerTag, PeerTags),
    send_safe(FailedPeerPid, #illegal_turn{session_pid = self()}),
    TimerId = make_ref(),
    erlang:send_after(2000, self(), {reconnect_timeout, FailedPeerTag, TimerId}),
    TimerId.

reconnect_timeout() ->
    case application:get_env(game_lobby, game_reconnect_timeout_sec) of
        {ok, Value} ->
            trunc(Value*1000);
        _ -> ?DEFAULT_RECONNECT_TIMEOUT
    end.
