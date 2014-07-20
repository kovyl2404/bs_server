
-module(client_connection).

-author("Viacheslav V. Kovalev").
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").

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
        ping_interval
    }
).


start_link(Ref, Socket, Transport, ProtocolOptions) ->
    proc_lib:start_link(?MODULE, init, [{Ref, Socket, Transport, ProtocolOptions}]).


init({Ref, Socket, Transport, ProtocolOptions}) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}]),
    MaxAllowedPings = proplists:get_value(max_pings_allowed, ProtocolOptions),
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
            ping_interval = trunc(1000*proplists:get_value(ping_interval_sec, ProtocolOptions))
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
        transport = Transport,
        socket = Socket,
        cur_seq_id = CurSeqId,
        ping_interval = PingInterval,
        their_seq_id = TheirSeqId,
        max_allowed_pings = MaxAllowedPings
    } = State
) ->
    case CurSeqId - TheirSeqId of
        Diff when Diff >= MaxAllowedPings ->
            {stop, pings_lost, State};
        _ ->
            Transport:send(Socket, ?PING_PACKET(CurSeqId)),
            erlang:send_after(PingInterval, self(), send_ping),
            {noreply, State#state{
                cur_seq_id = CurSeqId+1
            }}
    end;


handle_info(
    {tcp, _, Data},
    #state{
        parser = Parser
    } = State
) ->
    case protocol_parser:feed(Parser, Data) of
        {ok, NewParser, Messages} ->
            NewState = handle_messages(Messages, State),
            {noreply, NewState#state{ parser = NewParser}};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info(
    {tcp_error, _Socket, Reason},
    State
) ->
    {stop, Reason, State};

handle_info(
    {tcp_closed, _Socket},
    State
) ->
    {stop, normal, State};

handle_info(_Message, State) ->
    {stop, unexpected_info, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ socket = Socket, transport = Transport }) ->
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

    } = State
) ->
    TheirSeqId = ?PING_SEQ_ID(PingPacket),
    State#state{ their_seq_id = TheirSeqId }.