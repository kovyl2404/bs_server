
-module(client_connection).

-author("Viacheslav V. Kovalev").
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").
-include_lib("game_server/include/logging.hrl").

-define(TIMEOUT, 5000).

%% API
-export([
    start_link/4,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(
    state, {
        socket,
        transport,
        parser,
        cur_seq_id,
        their_seq_id,
        max_allowed_pings,
        ping_interval,
        session_pid
    }
).


start_link(Ref, Socket, Transport, ProtocolOptions) ->
    proc_lib:start_link(?MODULE, init, [{Ref, Socket, Transport, ProtocolOptions}]).


init({Ref, Socket, Transport, ProtocolOptions}) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}]),
    MaxAllowedPings = proplists:get_value(max_pings_allowed, ProtocolOptions),
    {ok, PeerName} = Transport:peername(Socket),
    {ok, SessionPid} = supervisor:start_child( client_session_sup,  [Socket, Transport, PeerName] ),
    ?DEBUG("Starting new client session ~p, peer ~p",[SessionPid, PeerName]),
    monitor(process, SessionPid),
    erlang:send(self(), send_ping),
    gen_server:enter_loop(
        ?MODULE, [],
        #state{
            socket = Socket,
            transport = Transport,
            parser = protocol_parser:init(),
            cur_seq_id = 0,
            their_seq_id = 0,
            max_allowed_pings = MaxAllowedPings,
            ping_interval = trunc(1000*proplists:get_value(ping_interval_sec, ProtocolOptions)),
            session_pid = SessionPid
        },
        ?TIMEOUT
    ).

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, unexpected_call, State}.


handle_cast(_Request, State) ->
    {stop, unexpected_cast, State}.

handle_info(
    send_ping,
    #state{
        their_seq_id = TheirSeqId,
        cur_seq_id = CurSeqId
    } = State
) when TheirSeqId > CurSeqId ->
    {stop, protocol_violation, State};


handle_info(
    send_ping,
    #state{
        cur_seq_id = CurSeqId,
        ping_interval = PingInterval,
        their_seq_id = TheirSeqId,
        max_allowed_pings = MaxAllowedPings,
        session_pid = SessionPid
    } = State
) ->
    case CurSeqId - TheirSeqId of
        Diff when Diff >= MaxAllowedPings ->
            ?DEBUG("Pings lost for client session ~p",[SessionPid]),
            {stop, normal, State};
        _ ->
            client_session:send_ping(SessionPid, CurSeqId),
            erlang:send_after(PingInterval, self(), send_ping),
            {noreply, State#state{
                cur_seq_id = CurSeqId+1
            }}
    end;


handle_info(
    {tcp, _, Data},
    #state{
        parser = Parser,
        session_pid = _SessionPid
    } = State
) ->
    case protocol_parser:feed(Parser, Data) of
        {ok, NewParser, Messages} ->
            NewState = handle_messages(Messages, State),
            {noreply, NewState#state{ parser = NewParser}};
        {error, Error} ->
            ?DEBUG("An parser error ~p occured in client session ~p",[Error,_SessionPid]),
            {stop, Error, State}
    end;

handle_info(
    {tcp_error, _Socket, Reason},
    #state{session_pid = _SessionPid} = State
) ->
    ?DEBUG("An transport error ~p occured in client session ~p",[Reason,_SessionPid]),
    {stop, Reason, State};

handle_info(
    {tcp_closed, _Socket},
    #state{ session_pid = _SessionPid } = State
) ->
    ?DEBUG("Session handler ~p closed by peer",[_SessionPid]),
    {stop, normal, State};

handle_info(
    {'DOWN', _, process, SessionPid, _},
    #state{
        session_pid = SessionPid
    } = State
) ->
    {stop, normal, State#state{session_pid = undefined}};

handle_info(_Message, State) ->
    {stop, unexpected_info, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(
    _Reason,
    #state{
        socket = Socket,
        transport = Transport,
        session_pid = SessionPid
    }
) ->
    SessionPid =/= undefined andalso client_session:stop(SessionPid),
    ok = Transport:close(Socket),
    ok.


handle_messages([], State) ->
    State;
handle_messages([Message | RestMessages], State) ->
    NewState = handle_message(Message, State),
    handle_messages(RestMessages, NewState).


handle_message(
    {command, ?PING_PACKET = PingPacket},
    #state{
        session_pid = _SessionPid
    } = State
) ->
    ?DEBUG("Client ression ~p received ping reply ~p",[_SessionPid, PingPacket]),
    TheirSeqId = ?PING_SEQ_ID(PingPacket),
    State#state{ their_seq_id = TheirSeqId };
handle_message(
    Command,
    #state{
        session_pid = SessionPid
    } = State
) ->
    ?DEBUG("Client session ~p received message ~p from peer",[SessionPid, Command]),
    ok = client_session:send_command(SessionPid, Command),
    State.