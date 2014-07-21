-module(client_session).

-behaviour(gen_fsm).

-include_lib("game_server/include/client_protocol.hrl").
-include_lib("game_lobby/include/common.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([
    start/2,
    start_link/2,
    stop/1
]).

-export([
    send_command/2,
    send_ping/2
]).

-export([
    init/1,
    idle/2,
    waiting_for_game/2,
    running_game/2,
    stopping_game/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).


start(Socket, Transport) ->
    gen_fsm:start(?MODULE, {Socket, Transport}, []).

start_link(Socket, Transport) ->
    gen_fsm:start_link(?MODULE, {Socket, Transport}, []).

stop(Session) ->
    gen_fsm:send_all_state_event(Session, stop).

send_command(Session, Command) ->
    gen_fsm:send_event(Session, Command).

send_ping(Session, SeqId) ->
    gen_fsm:send_all_state_event(Session, {ping, SeqId}).

-record(
    state, {
        socket               = eralng:error(required, socket),
        transport            = eralng:error(required, transport),
        game_token           = undefined,
        client_tag           = undefined,
        game_session         = undefined,
        is_ours_turn         = false,
        is_surrender_claimed = false
    }
).

init({Socket, Transport}) ->
    {ok, idle, #state{
        socket = Socket,
        transport = Transport
    }}.


handle_event(
    {ping, SeqId}, StateName,
    #state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    case Transport:send(Socket, [?PING_PACKET(SeqId)]) of
        ok ->
            {next_state, StateName, State};
        _ ->
            {stop, normal, State}
    end;

handle_event(
    stop, _StateName, State
) ->
    {stop, normal, State}.

handle_sync_event(_, _, _StateName, State) ->
    {stop, unexpected_sync_event, State}.

idle(
    {command, ?START_GAME_PACKET(?NEW_GAME_FLAG)},
    #state{

    } = State
) ->
    {ok, GameToken} = game_lobby:checkin(self()),
    {next_state, waiting_for_game, State#state{ game_token = GameToken }};

idle(
    {command, ?START_GAME_PACKET(?RECONNECT_GAME_FLAG)},
    State
) ->
    {next_state, waiting_for_game, State#state{ game_token = reconnecting }};

idle( {command, _}, State ) ->
    {stop, protocol_violation, State}.


waiting_for_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token
    } = State
) when Token =/= reconnecting ->
    {ok, _} = game_lobby:cancel(Token),
    {next_state, waiting_for_game, State};

waiting_for_game(
    {data, ReconnectionData},
    #state{
        game_token = reconnecting,
        socket = Socket,
        transport = Transport
    } = State
) ->
    case (catch binary_to_term(ReconnectionData)) of
        {ClientTag, ClientToken} ->
            case game_lobby:checkin(self(), ClientTag, ClientToken) of
                {ok, Token} ->
                    {next_state, waiting_for_game, State#state{ game_token = Token}};
                {error, session_expired} ->
                    SendFrame = [
                        ?START_GAME_PACKET(0),
                        session_utils:make_server_frame(ReconnectionData),
                        ?CANCEL_GAME_PACKET
                    ],
                    case Transport:send(Socket, SendFrame) of
                        ok ->
                            {next_state, idle, #state{socket = Socket, transport = Transport}};
                        _ ->
                            {stop, normal, State}
                    end;
                _ ->
                    {stop, reconnection_token_corrupted, State}
            end;
        _ ->
            {stop, reconnection_token_corrupted, State}
    end;


waiting_for_game(
    _, #state{} = State
) ->
    {stop, protocol_violation, State}.


running_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token
    } = State
) ->
    {ok, _} = game_lobby:cancel(Token),
    {next_state, stopping_game, State};


running_game(
    {command, ?SURRENDER_PACKET = Surrender},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = IsSurrenderClaimed
    } = State
) ->
    ok = game_session:surrender(GameSession, ClientTag, Surrender),
    IsSurrenderClaimed andalso update_profile_wins(),
    {next_state, stopping_game, State};

running_game(
    {command, _TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = true
    } = State
) ->
    ok = game_session:surrender(GameSession, ClientTag, ?SURRENDER_PACKET(0,0,0)),
    {stop, protocol_violation, State};

running_game(
    {command, TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = false
    } = State
) ->
    ok = game_session:make_turn(GameSession, ClientTag, TurnData),
    {next_state, running_game, State#state{ is_ours_turn = false }};

running_game(
    {command, _},
    #state{
        game_session = _,
        client_tag = _,
        is_ours_turn = false
    } = State
) ->
    {stop, protocol_violation, State}.

stopping_game(
    {command, _},
    #state{} = State
) ->
    {stop, protocol_violation, State}.



handle_info(
    #game_start{
        session_pid = GameSession,
        tag = ClientTag,
        token = Token,
        turn = Turn
    },
    waiting_for_game,
    #state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    TurnFlag = turn_flag(Turn),
    SendFrame = [
        ?START_GAME_PACKET(TurnFlag),
        session_utils:make_server_frame(term_to_binary({Token, ClientTag}))
    ],
    case Transport:send(Socket, SendFrame) of
        ok ->
            {next_state, running_game, State#state{
                game_session = GameSession,
                client_tag = ClientTag,
                is_ours_turn = Turn
            }};
        _ ->
            {ok, _} = game_lobby:cancel(Token),
            {stop, normal, State}
    end;

handle_info(
    #game_stop{
        token = _Token
    },
    StateName,
    #state{
        socket = Socket,
        transport = Transport
    } = State
) when StateName =:= waiting_for_game; StateName =:= stopping_game; StateName =:= running_game ->
    case Transport:send(Socket, ?CANCEL_GAME_PACKET) of
        ok ->
            {next_state, idle, #state{socket = Socket, transport = Transport}};
        _ ->
            {stop, normal, State}
    end;

handle_info(
    #peer_turn{
        data = TurnData
    },
    running_game,
    #state{
        is_ours_turn = false,
        transport = Transport,
        socket = Socket
    } = State
) ->
    case Transport:send(Socket, TurnData) of
        ok ->
            {next_state, running_game, State#state{is_ours_turn = true}};
        _ ->
            {stop, normal, State}
    end;


handle_info( #peer_turn{}, running_game, State ) ->
    {stop, internal_violation, State};

handle_info(
    #peer_surrender{ data = SurrenderData }, running_game,
    #state{
        is_ours_turn = false,
        socket = Socket,
        transport = Transport
    } = State
) ->
    case Transport:send(Socket, SurrenderData) of
        ok ->
            {next_state, running_game, State#state{is_surrender_claimed = true, is_ours_turn = true}};
        _ ->
            {stop, normal, State}
    end;

handle_info( #peer_surrender{}, running_game, State ) ->
    {next_state, internal_violation, State};


handle_info(
    #peer_lost{ },
    running_game,
    #state{} = State
) ->
    {next_state, running_game, State};

handle_info(
    #peer_reset{ },
    running_game,
    #state{} = State
) ->
    {next_state, running_game, State}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_, _StateName, _State) ->
    ok.


turn_flag(true) -> 1;
turn_flag(false) -> 0.

update_profile_wins() ->
    ok.