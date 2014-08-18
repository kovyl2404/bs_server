-module(client_session).

-behaviour(gen_fsm).

-include_lib("game_server/include/client_protocol.hrl").
-include_lib("game_lobby/include/common.hrl").
-include_lib("game_server/include/logging.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/metrics.hrl").

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
    ProfileBackend = profile_backend(),
    random:seed(),
    InitState =
        case ProfileBackend of
            undefined -> idle;
            _ -> guest
        end,
    folsom_metrics:notify({?GAME_SERVER_CONNECTIONS_METRIC, {inc, 1}}),
    folsom_metrics:notify({?GAME_SERVER_GUEST_CONNECTIONS_METRIC, {inc, 1}}),
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
    stop, _StateName,
    State
) ->
    {stop, normal, State}.

handle_sync_event(_, _, _StateName, State) ->
    {stop, unexpected_sync_event, State}.

guest(
    {data, <<?RESET_PASSWORD_REQUEST_TAG, RequestData/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket
    } = State
) when ProfileBackend =/= undefined ->
    {ok, Login} = session_utils:decode_password_reset_request(RequestData),
    SendFrame =
        case ProfileBackend:get_by_id(Login) of
            {ok, Profile} ->
                ok = password_manager:request(Login, Profile),
                session_utils:encode_password_reset_response(ok);
            {error, not_found} ->
                session_utils:encode_password_reset_response(incorrect_login)
        end,
    Transport:send(Socket, session_utils:make_server_frame([ ?RESET_PASSWORD_REQUEST_TAG, SendFrame ])),
    {next_state, guest, State};

guest(
    {data, <<?COMMIT_PASSWORD_REQUEST_TAG, RequestData/binary>>},
    #state{
        profile_backend = ProfileBackend,
        socket = Socket,
        transport = Transport
    } = State
) when ProfileBackend =/= undefined ->
    {ok, Login, ConfirmationCode, NewPassword} = session_utils:decode_commit_password_request(RequestData),
    SendFrame =
        case ProfileBackend:get_by_id(Login) of
            {ok, Profile} ->
                CommitResult =
                    case password_manager:commit(Login, ConfirmationCode) of
                        ok ->
                            ok = ProfileBackend:change_password(Login, NewPassword),
                            ok;
                        {error, Reason} ->
                            Reason
                    end,
                session_utils:encode_commit_password_result(CommitResult);
            {error, not_found} ->
                session_utils:encode_commit_password_result(incorrect_login)
        end,
    Transport:send(Socket, session_utils:make_server_frame([ ?COMMIT_PASSWORD_REQUEST_TAG, SendFrame ])),
    {next_state, guest, State};

guest(
    {data, <<?LOGIN_TAG, AuthRequest/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket
    } = State
) ->
    ?DEBUG("Client session ~p (guest) received auth request ~p",[self(), AuthRequest]),
    case session_utils:decode_auth_request(AuthRequest) of
        {ok, {Login, Password}} ->
            LoginString = unicode:characters_to_list(Login, utf8),
                            
            ?DEBUG("Client session ~p trying to authenticate with login ~s and password ~p",[self(), LoginString, Password]),
            case ProfileBackend:login(Login, Password) of
                {ok, Profile} ->
                    case (catch gproc:reg({n, l, Login})) of
                        true -> 
                            ?DEBUG("Client session ~p successfully authenticated",[self()]),
                            EncodedProfile = session_utils:encode_profile_request(Profile),
                            Transport:send(
                                Socket,[
                                    session_utils:make_server_frame([?LOGIN_TAG, session_utils:encode_auth_response(ok)]),
                                    session_utils:make_server_frame([?PROFILE_TAG, EncodedProfile])
                                ]
                            ),
                            folsom_metrics:notify({?GAME_SERVER_GUEST_CONNECTIONS_METRIC, {dec, 1}}),
                            folsom_metrics:notify({?GAME_SERVER_AUTHENTICATED_CONNECTIONS_METRIC, {inc, 1}}),
                            {next_state, idle, State#state{ peer_name = LoginString }};
                        _ ->
                            Transport:send(Socket, session_utils:make_server_frame([
                                ?LOGIN_TAG,
                                session_utils:encode_auth_response(already_authenticated)
                            ])),
                            {next_state, guest, State}
                    end;
                {error, not_found} ->
                    ?ERROR("Client session ~p failed authentication as ~s (no such login found)",[self(), LoginString]),
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?LOGIN_TAG, session_utils:encode_auth_response(incorrect_login)])
                    ),
                    {next_state, guest, State};
                {error, incorrect_password} ->
                    ?ERROR("Client session ~p failed authentication as ~s (incorrect password)",[self(), LoginString]),
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?LOGIN_TAG, session_utils:encode_auth_response(incorrect_password)])
                    ),
                    {next_state, guest, State}
            end;
        _Error ->
            ?DEBUG("Client session ~p could not decode authenticate request",[self()]),
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
    ?DEBUG("Client session ~p (guest) received register request ~p",[self(), RegisterRequest]),
    case session_utils:decode_register_request(RegisterRequest) of
        {ok, {Login, Password, Email}} ->
            LoginString = unicode:characters_to_list(Login, utf8),
            ?DEBUG("Client session ~p trying to register with login ~s (~s) and password ~p",[self(), LoginString, Email, Password]),
            case ProfileBackend:register(Login, Password, Email) of
                {ok, Profile} ->
                    ?DEBUG("Client session ~p successfully registered", [self()]),
                    EncodedProfile = session_utils:encode_profile_request(Profile),
                    Transport:send(
                        Socket,[
                            session_utils:make_server_frame([?REGISTER_TAG, session_utils:encode_auth_response(ok)]),
                            session_utils:make_server_frame([?PROFILE_TAG, EncodedProfile])
                        ]
                    ),
                    {next_state, idle, State#state{ peer_name = LoginString}};
                {error, already_registered} ->
                    ?DEBUG("Client session ~p failed registration, login ~s exists", [self(), LoginString]),
                    Transport:send(
                        Socket,
                        session_utils:make_server_frame([?REGISTER_TAG, session_utils:encode_auth_response(incorrect_login)])
                    ),
                    {next_state, guest, State}
            end;
        _Error ->
            ?DEBUG("Client session ~p could not decode register request", [self()]),
            {stop, protocol_violation, State}
    end;

guest(
    {data, <<?TOP_TAG, TopRequest>>},
    #state{
        profile_backend = ProfileBackend,
        socket = Socket,
        transport = Transport
    } = State
) ->
    ?DEBUG("Client session ~p (guest) received top request (~p items)", [self(), TopRequest]),
    {ok, Top} = ProfileBackend:get_top(TopRequest),
    Transport:send(
        Socket,
        session_utils:make_server_frame([?TOP_TAG, session_utils:encode_top_response(Top)])
    ),
    {next_state, guest, State};

guest(
    {data, <<?SERVER_STATUS_TAG, StatusRequest/binary>>},
    #state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    {ok, ClientVsn} = session_utils:decode_server_status_request(StatusRequest),

    {ClientConnections, GamesRunning, GamesWaiting} = session_utils:get_basic_metrics(),
    VersionSupported = game_server:is_supported_vsn(ClientVsn),
    Response =
        session_utils:encode_server_status_response(
            VersionSupported, ClientConnections, GamesRunning, GamesWaiting
        ),
    Transport:send(Socket, session_utils:make_server_frame([?SERVER_STATUS_TAG, Response])),
    case VersionSupported of
        true ->
            {next_state, guest, State};
        false ->
            {stop, invalid_vsn, State}
    end;

guest(_, State) ->
    {stop, not_auth, State}.


idle(
    {data, <<?RESET_PASSWORD_REQUEST_TAG, RequestData/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket
    } = State
) when ProfileBackend =/= undefined ->
    {ok, Login} = session_utils:decode_password_reset_request(RequestData),
    SendFrame =
        case ProfileBackend:get_by_id(Login) of
            {ok, Profile} ->
                ok = password_manager:request(Login, Profile),
                session_utils:encode_password_reset_response(ok);
            {error, not_found} ->
                session_utils:encode_password_reset_response(incorrect_login)
        end,
    Transport:send(Socket, session_utils:make_server_frame([ ?RESET_PASSWORD_REQUEST_TAG, SendFrame ])),
    {next_state, idle, State};

idle(
    {data, <<?COMMIT_PASSWORD_REQUEST_TAG, RequestData/binary>>},
    #state{
        profile_backend = ProfileBackend,
        socket = Socket,
        transport = Transport
    } = State
) when ProfileBackend =/= undefined ->
    {ok, Login, ConfirmationCode, NewPassword} = session_utils:decode_commit_password_request(RequestData),
    SendFrame =
        case ProfileBackend:get_by_id(Login) of
            {ok, _} ->
                CommitResult =
                    case password_manager:commit(Login, ConfirmationCode) of
                        ok ->
                            ok = ProfileBackend:change_password(Login, NewPassword),
                            ok;
                        {error, Reason} ->
                            Reason
                    end,
                session_utils:encode_commit_password_result(CommitResult);
            {error, not_found} ->
                session_utils:encode_commit_password_result(incorrect_login)
        end,
    Transport:send(Socket, session_utils:make_server_frame([ ?COMMIT_PASSWORD_REQUEST_TAG, SendFrame ])),
    {next_state, idle, State};

idle(
    {command, ?START_GAME_PACKET(?NEW_GAME_FLAG)},
    #state{
        peer_name = PeerName
    } = State
) ->
    {ok, GameToken} = game_lobby:checkin(self(), PeerName),
    ?DEBUG("Client session ~p (~s) received request to start new game ~p", [self(), PeerName, GameToken]),
    {next_state, waiting_for_game, State#state{ game_token = GameToken }};

idle(
    {command, ?START_GAME_PACKET(?RECONNECT_GAME_FLAG)},
    #state{peer_name = _PeerName} = State
) ->
    ?DEBUG("Client session ~p (~s) received request to reconnect existed game", [self(), _PeerName]),
    {next_state, waiting_for_game, State#state{ game_token = reconnecting }};

idle( {command, _Command}, #state{peer_name = _PeerName} = State ) ->
    {stop, protocol_violation, State};

idle(
    {data, <<?PROFILE_TAG, ProfileRequest/binary>>},
    #state{
        profile_backend = ProfileBackend,
        transport = Transport,
        socket = Socket,
        peer_name = PeerName
    } = State
) when ProfileBackend =/= undefined  ->
    ?DEBUG("Client session ~p (~s) received request to update profile", [self(), PeerName]),
    case session_utils:decode_profile_request(ProfileRequest) of
        {ok, Profile} ->
            ?DEBUG("Client session ~p (~s) successfully decoded profile ~p", [self(), PeerName, Profile]),
            {ok, UpdatedProfile} = ProfileBackend:update_profile(Profile, unicode:characters_to_binary(PeerName, utf8)),
            EncodedProfile = session_utils:encode_profile_request(UpdatedProfile),
            Transport:send(
                Socket,[
                    session_utils:make_server_frame([?PROFILE_TAG, EncodedProfile])
                ]
            ),
            {next_state, idle, State};
        {error, _Reason} ->
            ?DEBUG("Client session ~p (~s) failed to decoded profile with reason ~p", [self(), PeerName, _Reason]),
            {stop, protocol_violation, State}
    end;


idle(
    {data, <<?TOP_TAG, TopRequest>>},
    #state{
        profile_backend = ProfileBackend,
        socket = Socket,
        transport = Transport,
        peer_name = _PeerName
    } = State
) when ProfileBackend =/= undefined ->
    ?DEBUG("Client session ~p (~s) received top request (~p items)", [self(), _PeerName, TopRequest]),
    {ok, Top} = ProfileBackend:get_top(TopRequest),
    Transport:send(
        Socket,
        session_utils:make_server_frame([?TOP_TAG, session_utils:encode_top_response(Top)])
    ),
    {next_state, idle, State};

idle(
    {data, <<?SERVER_STATUS_TAG, StatusRequest/binary>>},
    #state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    {ok, ClientVsn} = session_utils:decode_server_status_request(StatusRequest),

    {ClientConnections, GamesRunning, GamesWaiting} = session_utils:get_basic_metrics(),
    VersionSupported = game_server:is_supported_vsn(ClientVsn),
    Response =
        session_utils:encode_server_status_response(
            VersionSupported, ClientConnections, GamesRunning, GamesWaiting
        ),
    Transport:send(Socket, session_utils:make_server_frame([?SERVER_STATUS_TAG, Response])),
    case VersionSupported of
        true ->
            {next_state, idle, State};
        false ->
            {stop, invalid_vsn, State}
    end;


idle( _Msg, State ) ->
    {stop, protocol_violation, State}.


waiting_for_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token,
        peer_name = _PeerName
    } = State
) when Token =/= reconnecting ->
    ?DEBUG("Client session ~p (~s) received request to cancel game ~p", [self(), _PeerName, Token]),
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
    ?DEBUG("Client session ~p (~s) received reconnection token ~p", [self(), PeerName, ReconnectionData]),
    case (catch binary_to_term(ReconnectionData)) of
        {ClientToken, ClientTag} ->
            case game_lobby:checkin(self(), PeerName, ClientToken, ClientTag) of
                {ok, ClientToken} ->
                    ?DEBUG("Client session ~p (~s) reconnected to game ~p", [self(), PeerName, ClientToken]),
                    {next_state, waiting_for_game, State#state{ game_token = ClientToken}};
                {error, session_expired} ->
                    ?DEBUG("Client session ~p (~s) failed to reconnect to game ~p, because of session expired", [self(), PeerName, ClientToken]),
                    SendFrame = [
                        ?START_GAME_PACKET(0),
                        session_utils:make_server_frame(ReconnectionData),
                        ?CANCEL_GAME_PACKET
                    ],
                    Transport:send(Socket, SendFrame),
                    {next_state, idle, #state{socket = Socket, transport = Transport, peer_name = PeerName}};
                {error, _Reason} ->
                    ?DEBUG("Client session ~p (~s) failed to reconnect to game ~p, because of ~p", [self(), PeerName, ClientToken, _Reason]),
                    {stop, reconnection_token_corrupted, State}
            end;
        _ ->
            ?DEBUG("Client session ~p (~s) failed to decode reconnection token ~p", [self(), PeerName, ReconnectionData]),
            {stop, reconnection_token_corrupted, State}
    end;


waiting_for_game(
    _, State
) ->
    {stop, protocol_violation, State}.


running_game(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        game_token = Token,
        peer_name = _PeerName
    } = State
) ->
    ?DEBUG("Client session ~p (~s) received request to cancel game ~p", [self(), _PeerName, Token]),
    {ok, _} = game_lobby:cancel(Token),
    {next_state, stopping_game, State};


running_game(
    {command, ?SURRENDER_PACKET = Surrender},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = IsSurrenderClaimed,
        peer_name = PeerName,
        game_token = _Token
    } = State
) ->
    ok = game_session:surrender(GameSession, ClientTag, Surrender),
    case IsSurrenderClaimed of
        true ->
            ?DEBUG("Client session ~p (~s) acknowledged that remote peer surrendered in game ~p", [self(), PeerName, _Token]);
        false ->
            ?DEBUG("Client session ~p (~s) decided to surrendered in game ~p", [self(), PeerName, _Token])
    end,
    {next_state, stopping_game, State};

running_game(
    {command, _TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = true,
        game_token = _Token,
        peer_name = _PeerName
    } = State
) ->
    ?DEBUG("Client session ~p (~s) makes turn ~p instead of surrendered acknowledge in game ~p", [self(), _PeerName, _TurnData, _Token]),
    ok = game_session:surrender(GameSession, ClientTag, ?SURRENDER_PACKET_NIL()),
    {stop, protocol_violation, State};

running_game(
    {command, TurnData},
    #state{
        game_session = GameSession,
        client_tag = ClientTag,
        is_ours_turn = true,
        is_surrender_claimed = false,
        peer_name = _PeerName,
        game_token = _Token
    } = State
) ->
    ?DEBUG("Client session ~p (~s) makes turn ~p in game ~p", [self(), _PeerName, TurnData, _Token]),
    ok = game_session:make_turn(GameSession, ClientTag, TurnData),
    {next_state, running_game, State#state{ is_ours_turn = false }};

running_game(
    {command, _Turn},
    #state{
        game_session = _,
        client_tag = _,
        is_ours_turn = false,
        peer_name = _PeerName,
        game_token = _Token
    } = State
) ->
    ?DEBUG("Client session ~p (~s) makes UNEXPECTED turn ~in game ~p", [self(), _PeerName, _Turn, _Token]),
    {stop, protocol_violation, State};

running_game(
    {data, _UnexpectedData},
    #state{
        game_session = _,
        client_tag = _,
        is_ours_turn = false
    } = State
) ->
    {stop, protocol_violation, State}.

stopping_game( _UnexpectedMessage, State ) ->
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
        peer_name = _PeerName
    } = State
) ->
    TurnFlag = turn_flag(Turn),
    SendFrame = [
        ?START_GAME_PACKET(TurnFlag),
        session_utils:make_server_frame(term_to_binary({Token, ClientTag}))
    ],
    case Transport:send(Socket, SendFrame) of
        ok ->
            ?DEBUG("Client session ~p (~s) successfully started game ~p", [self(), _PeerName, Token]),
            {next_state, running_game, State#state{
                game_session = GameSession,
                client_tag = ClientTag,
                is_ours_turn = Turn
            }};
        _Error ->
            ?DEBUG("Client session ~p (~s) failed to start game, because of socket error ~p", [self(), _PeerName, Token, _Error]),
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
        peer_name = PeerName,
        game_token = _Token,
        profile_backend = ProfileBackend
    }
) when StateName =:= waiting_for_game; StateName =:= stopping_game; StateName =:= running_game ->
    ?DEBUG("Client session ~p (~s) finished game ~p in state ~p", [self(), PeerName, _Token, StateName]),
    Transport:send(Socket, ?CANCEL_GAME_PACKET),
    {next_state, idle, #state{
        socket = Socket, transport = Transport, peer_name = PeerName,
        profile_backend = ProfileBackend
    }};


handle_info(
    #peer_turn{
        data = TurnData
    },
    running_game,
    #state{
        is_ours_turn = false,
        transport = Transport,
        socket = Socket,
        peer_name = _PeerName,
        game_token = _Token
    } = State
) ->
    ?DEBUG("Client session ~p (~s) received peer turn ~p in game ~p", [self(), _PeerName, TurnData, _Token]),
    Transport:send(Socket, TurnData),
    {next_state, running_game, State#state{is_ours_turn = true}};


handle_info( #peer_turn{}, running_game, State ) ->
    {stop, internal_violation, State};

handle_info(
    #peer_surrender{ data = SurrenderData }, running_game,
    #state{
        is_ours_turn = false,
        socket = Socket,
        transport = Transport,
        peer_name = _PeerName,
        game_token = _Token
    } = State
) ->
    ?DEBUG("Client session ~p (~s) received peer surrender ~p in game ~p", [self(), _PeerName, SurrenderData, _Token]),
    Transport:send(Socket, SurrenderData),
    {next_state, running_game, State#state{is_surrender_claimed = true, is_ours_turn = true}};

handle_info( #peer_surrender{}, running_game, State ) ->
    {next_state, internal_violation, State};


handle_info(
    #peer_lost{ },
    running_game,
    #state{
        socket = Socket,
        transport = Transport
    } = State
) ->
    SendFrame =
        session_utils:make_server_frame(
            [?PEER_STATUS_TAG, session_utils:encode_peer_status(false)]
        ),
    Transport:send(Socket, SendFrame),
    {next_state, running_game, State};

handle_info(
    #peer_reset{ },
    running_game,
    #state{
        socket = Socket,
        transport = Transport
    } = State
) ->
    SendFrame =
        session_utils:make_server_frame(
            [?PEER_STATUS_TAG, session_utils:encode_peer_status(true)]
        ),
    Transport:send(Socket, SendFrame),
    {next_state, running_game, State};

handle_info(
    _, _,
    #state{} = State
) ->
    {stop, internal_violation, State}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate( Reason, guest, _ ) ->
    folsom_metrics:notify({?GAME_SERVER_GUEST_CONNECTIONS_METRIC, {dec, 1}}),
    folsom_metrics:notify({?GAME_SERVER_CONNECTIONS_METRIC, {dec, 1}}),
    handle_terminate_reason(Reason),
    ok;
terminate(
    Reason, _,
    #state{
        profile_backend = ProfileBackend
    }
) ->
    case ProfileBackend of
        undefined ->
            folsom_metrics:notify({?GAME_SERVER_GUEST_CONNECTIONS_METRIC, {dec, 1}});
        _ ->
            folsom_metrics:notify({?GAME_SERVER_AUTHENTICATED_CONNECTIONS_METRIC, {dec, 1}})
    end,
    folsom_metrics:notify({?GAME_SERVER_CONNECTIONS_METRIC, {dec, 1}}),
    handle_terminate_reason(Reason),
    ok.

handle_terminate_reason(already_authenticated) ->
    folsom_metrics:notify({?GAME_SERVER_PROTOCOL_VIOLATIONS, 1});
handle_terminate_reason(not_auth) ->
    folsom_metrics:notify({?GAME_SERVER_PROTOCOL_VIOLATIONS, 1});
handle_terminate_reason(protocol_violation) ->
    folsom_metrics:notify({?GAME_SERVER_PROTOCOL_VIOLATIONS, 1});
handle_terminate_reason(reconnection_token_corrupted) ->
    folsom_metrics:notify({?GAME_SERVER_PROTOCOL_VIOLATIONS, 1});
handle_terminate_reason(_) ->
    ok.

turn_flag(true) -> 1;
turn_flag(false) -> 0.


profile_backend() ->
    case application:get_env(game_server, profile) of
        {ok, Value} -> Value;
        _ -> undefined
    end.
