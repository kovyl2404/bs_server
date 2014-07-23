-module(client_session).

-behaviour(gen_fsm).
%-compile([{parse_transform, lager_transform}]).
-include_lib("game_server/include/client_protocol.hrl").
-include_lib("game_lobby/include/common.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([
    start/2,
    start/3,
    start_link/3,
    stop/1
]).

-export([
    send_command/2,
    send_ping/2
]).

-export([
    init/1,
    guest/2,
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
    start(Socket, Transport, ?MODULE).

start(Socket, Transport, PeerName) ->
    gen_fsm:start(?MODULE, {Socket, Transport, PeerName}, []).

start_link(Socket, Transport, PeerName) ->
    gen_fsm:start_link(?MODULE, {Socket, Transport, PeerName}, []).

stop(Session) ->
    gen_fsm:send_all_state_event(Session, stop).

send_command(Session, Command) ->
    gen_fsm:send_event(Session, Command).

send_ping(Session, SeqId) ->
    gen_fsm:send_all_state_event(Session, {ping, SeqId}).

-record(
    state, {
        profile_backend      = undefined,
        socket               = eralng:error(required, socket),
        transport            = eralng:error(required, transport),
        game_token           = undefined,
        client_tag           = undefined,
        game_session         = undefined,
        is_ours_turn         = false,
        is_surrender_claimed = false,
        peer_name            = erlang:error(required)
    }
).

init({Socket, Transport, PeerName}) ->
    lager:info("New client connected from ~p", [PeerName]),
    ProfileBackend = profile_backend(),
    InitState =
        case ProfileBackend of
            undefined -> idle;
            _ -> guest
        end,
    {ok, InitState, #state{
        profile_backend = ProfileBackend,
        socket = Socket,
        transport = Transport,
        peer_name = PeerName
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

guest(
    {data, <<?LOGIN_TAG, AuthRequest/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket
    } = State
) ->
    case session_utils:decode_auth_request(AuthRequest) of
        {ok, {Login, Password}} ->
            case ProfileBackend:login(Login, Password) of
                {ok, _} ->
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?LOGIN_TAG, session_utils:encode_auth_response(true)])
                    ),
                    {next_state, idle, State};
                {error, not_found} ->
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?LOGIN_TAG, session_utils:encode_auth_response(false)])
                    ),
                    {next_state, guest, State}
            end;
        _Error ->
            {stop, protocol_violation, State}
    end;

guest(
    {data, <<?REGISTER_TAG, RegisterRequest/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket
    } = State
) ->
    case session_utils:decode_auth_request(RegisterRequest) of
        {ok, {Login, Password}} ->
            case ProfileBackend:register(Login, Password) of
                {ok, _} ->
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?REGISTER_TAG, session_utils:encode_auth_response(true)])
                    ),
                    {next_state, idle, State};
                {error, already_registered} ->
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?REGISTER_TAG, session_utils:encode_auth_response(false)])
                    ),
                    {next_state, guest, State}
            end;
        _Error ->
            {stop, protocol_violation, State}
    end;

guest(_, State) ->
    {stop, not_auth, State}.

idle(
    {command, ?START_GAME_PACKET(?NEW_GAME_FLAG)},
    #state{
        peer_name = PeerName
    } = State
) ->
    lager:debug("Received START_GAME(NEW_GAME) packet from ~p in idle state", [PeerName]),
    {ok, GameToken} = game_lobby:checkin(self()),
    {next_state, waiting_for_game, State#state{ game_token = GameToken }};

idle(
    {command, ?START_GAME_PACKET(?RECONNECT_GAME_FLAG)},
    #state{peer_name = PeerName} = State
) ->
    lager:debug("Received START_GAME(RECONNECT) packet from ~p in idle state", [PeerName]),
    {next_state, waiting_for_game, State#state{ game_token = reconnecting }};

idle( {command, Command}, #state{peer_name = PeerName} = State ) ->
    lager:debug("Received unexpected command ~p from ~p in idle state", [Command, PeerName]),
    {stop, protocol_violation, State};

idle( _, State ) ->
    {stop, protocol_violation, State}.


waiting_for_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token,
        peer_name = PeerName
    } = State
) when Token =/= reconnecting ->
    lager:debug("Received CANCEL_GAME from ~p in waiting_for_game state", [PeerName]),
    {ok, _} = game_lobby:cancel(Token),
    {next_state, waiting_for_game, State};

waiting_for_game(
    {data, ReconnectionData},
    #state{
        game_token = reconnecting,
        socket = Socket,
        transport = Transport,
        peer_name = PeerName
    } = State
) ->
    lager:debug("Received RECONNECTION_DATA ~p from ~p in waiting_for_game state", [ReconnectionData, PeerName]),
    case (catch binary_to_term(ReconnectionData)) of
        {ClientToken, ClientTag} ->
            case game_lobby:checkin(self(), ClientToken, ClientTag) of
                {ok, ClientToken} ->
                    lager:debug("Reconnecting to session ~p for ~p", [ClientToken, PeerName]),
                    {next_state, waiting_for_game, State#state{ game_token = ClientToken}};
                {error, session_expired} ->
                    lager:debug("Could not reconnect to expired session ~p for ~p", [ClientToken, PeerName]),
                    SendFrame = [
                        ?START_GAME_PACKET(0),
                        session_utils:make_server_frame(ReconnectionData),
                        ?CANCEL_GAME_PACKET
                    ],
                    case Transport:send(Socket, SendFrame) of
                        ok ->
                            {next_state, idle, #state{socket = Socket, transport = Transport, peer_name = PeerName}};
                        _ ->
                            {stop, normal, State}
                    end;
                _ ->
                    {stop, reconnection_token_corrupted, State}
            end;
        _ ->
            lager:error("Could not parse RECONNECTION_DATA ~p received from ~p", [ReconnectionData, PeerName]),
            {stop, reconnection_token_corrupted, State}
    end;


waiting_for_game(
    UnexpectedMessage,
    #state{
        peer_name = PeerName
    } = State
) ->
    lager:error("Unexpected message ~p for ~p in waiting_for_game state", [UnexpectedMessage, PeerName]),
    {stop, protocol_violation, State}.


running_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token,
        peer_name = PeerName
    } = State
) ->
    lager:debug("Received CANCEL_GAME from ~p in running_game state", [PeerName]),
    {ok, _} = game_lobby:cancel(Token),
    {next_state, stopping_game, State};


running_game(
    {command, ?SURRENDER_PACKET = Surrender},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = IsSurrenderClaimed,
        peer_name = PeerName
    } = State
) ->
    ok = game_session:surrender(GameSession, ClientTag, Surrender),
    case IsSurrenderClaimed of
        true ->
            lager:debug(
                "Acknowledge for peer SURRENDER_PACKET ~p from ~p in running_game state (this peer wins)",
                [Surrender, PeerName]
            ),
            update_profile_wins();
        false ->
            lager:debug(
                "Received SURRENDER_PACKET ~p from ~p in running_game state (this peer was surrendered)",
                [Surrender, PeerName]
            )
    end,
    {next_state, stopping_game, State};

running_game(
    {command, TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = true,
        peer_name = PeerName
    } = State
) ->
    lager:error(
        "Received command ~p instead of SURRENDER_PACKET acknowledge from ~p in running_game state",
        [TurnData, PeerName]
    ),
    ok = game_session:surrender(GameSession, ClientTag, ?SURRENDER_PACKET(0,0,0)),
    {stop, protocol_violation, State};

running_game(
    {command, TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = false,
        peer_name = PeerName
    } = State
) ->
    lager:debug(
        "Received TURN ~p from ~p in running_game state",
        [TurnData, PeerName]
    ),
    ok = game_session:make_turn(GameSession, ClientTag, TurnData),
    {next_state, running_game, State#state{ is_ours_turn = false }};

running_game(
    {command, Turn},
    #state{
        game_session = _,
        client_tag = _,
        is_ours_turn = false,
        peer_name = PeerName
    } = State
) ->
    lager:error(
        "Received unexpected TURN ~p from ~p in running_game (out of order)",
        [Turn, PeerName]
    ),
    {stop, protocol_violation, State};

running_game(
    {data, UnexpectedData},
    #state{
        game_session = _,
        client_tag = _,
        is_ours_turn = false,
        peer_name = PeerName
    } = State
) ->
    lager:error(
        "Received unexpected DATA ~p from ~p in running_game",
        [UnexpectedData, PeerName]
    ),
    {stop, protocol_violation, State}.

stopping_game(
    UnexpectedMessage,
    #state{
        peer_name = PeerName
    } = State
) ->
    lager:error(
        "Received unexpected command ~p from ~p in stopping_game",
        [UnexpectedMessage, PeerName]
    ),
    {stop, protocol_violation, State};

stopping_game(
    {data, Data},
    #state{
        peer_name = PeerName
    } = State
) ->
    lager:error(
        "Received unexpected data ~p from ~p in stopping_game",
        [Data, PeerName]
    ),
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
        socket = Socket,
        peer_name = PeerName
    } = State
) ->
    TurnFlag = turn_flag(Turn),
    SendFrame = [
        ?START_GAME_PACKET(TurnFlag),
        session_utils:make_server_frame(term_to_binary({Token, ClientTag}))
    ],
    case Transport:send(Socket, SendFrame) of
        ok ->
            lager:debug("Succeeded sent START_GAME(~p) to peer ~p",[TurnFlag, PeerName]),
            {next_state, running_game, State#state{
                game_session = GameSession,
                client_tag = ClientTag,
                is_ours_turn = Turn
            }};
        _ ->
            lager:debug(
                "Failed to send START_GAME(~p) to peer ~p, forcing game cancellation and closing session",
                [TurnFlag, PeerName]
            ),
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
        transport = Transport,
        peer_name = PeerName
    } = State
) when StateName =:= waiting_for_game; StateName =:= stopping_game; StateName =:= running_game ->
    case Transport:send(Socket, ?CANCEL_GAME_PACKET) of
        ok ->
            lager:debug(
                "Succeeded sent CANCEL_GAME to peer ~p, in ~p state",
                [PeerName, StateName]
            ),
            {next_state, idle, #state{socket = Socket, transport = Transport, peer_name = PeerName}};
        _ ->
            lager:debug( "Failed to send CANCEL_GAME to peer ~p, closing session", [PeerName] ),
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
        socket = Socket,
        peer_name = PeerName
    } = State
) ->
    case Transport:send(Socket, TurnData) of
        ok ->
            lager:debug(
                "Succeeded sent TURN ~p to peer ~p, in running_game state",
                [TurnData, PeerName]
            ),
            {next_state, running_game, State#state{is_ours_turn = true}};
        _ ->
            lager:debug( "Failed to send TURN ~p to peer ~p, closing session", [TurnData, PeerName] ),
            {stop, normal, State}
    end;


handle_info( #peer_turn{}, running_game, State ) ->
    {stop, internal_violation, State};

handle_info(
    #peer_surrender{ data = SurrenderData }, running_game,
    #state{
        is_ours_turn = false,
        socket = Socket,
        transport = Transport,
        peer_name = PeerName
    } = State
) ->
    case Transport:send(Socket, SurrenderData) of
        ok ->
            lager:debug(
                "Succeeded sent SURRENDER_PACKET ~p to peer ~p, in running_game state (this peer claiming surrender)",
                [SurrenderData, PeerName]
            ),
            {next_state, running_game, State#state{is_surrender_claimed = true, is_ours_turn = true}};
        _ ->
            lager:debug(
                "Failed to send SURRENDER_PACKET ~p to peer ~p, in running_game state (this peer claiming surrender), closing session",
                [SurrenderData, PeerName]
            ),
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
    {next_state, running_game, State};

handle_info(
    _, _,
    #state{} = State
) ->
    {stop, internal_violation, State}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(
    Reason, StateName,
    #state{
        peer_name = PeerName
    } = State
) ->
    lager:debug(
        "Client session ~p stopped in state ~p with reason ~p. State data ~p",
        [PeerName, StateName, Reason, State]
    ),
    ok.


turn_flag(true) -> 1;
turn_flag(false) -> 0.

update_profile_wins() ->
    ok.

profile_backend() ->
    case application:get_env(game_server, profile) of
        {ok, Value} -> Value;
        _ -> undefined
    end.