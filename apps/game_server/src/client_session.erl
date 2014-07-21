-module(client_session).

-behaviour(gen_server).

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
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(
    idle_state, {
        socket      = erlang:error(required, socket),
        transport   = erlang:error(required, transport)
    }
).


-record(
    in_game_state, {
        socket = erlang:error(required, socket),
        transport = erlang:error(required, transport),
        token = erlang:error(required, token),
        session_pid = undefined,
        tag  = undefined,
        is_ours_turn = false,
        game_stopping = false,
        is_reconnect = false,
        waiting_surrender_ack = false
    }
).

start_link(Socket, Transport) ->
    gen_server:start_link(
        ?MODULE, {Socket, Transport}, []
    ).

start(Socket, Transport) ->
    gen_server:start(
        ?MODULE, {Socket, Transport}, []
    ).


send_command(SessionPid, Command) ->
    gen_server:cast(SessionPid, Command).

send_ping(SessionPid, SeqId) ->
    gen_server:cast(SessionPid, {send_ping, SeqId}).

stop(SessionPid) ->
    gen_server:cast(SessionPid, stop).


init({Socket, Transport}) ->
    {ok, #idle_state{
        socket = Socket,
        transport = Transport
    }}.


handle_call(
    _, _, State
) ->
    {stop, unexpected_call, unexpected_call, State}.


handle_cast(
    stop,
    #in_game_state{
        session_pid = undefined,
        is_reconnect = IsReconnect,
        token = Token
    } = State
) ->
    not IsReconnect andalso game_lobby:cancel(Token),
    {stop, normal, State};

handle_cast(
    stop, State
) ->
    {stop, normal, State};

handle_cast(
    {send_ping, SeqId},
    #in_game_state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    case send_ping(Socket, Transport, SeqId) of
        ok ->
            {noreply, State};
        _ ->
            {stop, normal, State}
    end;

handle_cast(
    {send_ping, SeqId},
    #idle_state{
        transport = Transport,
        socket = Socket
    } = State
) ->
    case send_ping(Socket, Transport, SeqId) of
        ok ->
            {noreply, State};
        _ ->
            {stop, normal, State}
    end;

handle_cast(
    {command, ?START_GAME_PACKET(0)},
    #idle_state{
        socket = Socket,
        transport = Transport
    }
) ->
    {ok, Token} = game_lobby:checkin(self()),
    {noreply, #in_game_state{
        token = Token,
        socket = Socket,
        transport = Transport
    }};


%% ?START_GAME_PACKET(1) stand for reconnect to formerly started game.
%% After this message client must send data-packet with payload,
%% that indicates which game reconnect to.
handle_cast(
    {command, ?START_GAME_PACKET(1)},
    #idle_state{
        socket = Socket,
        transport = Transport
    }
) ->
    {noreply, #in_game_state{
        token = undefined,
        socket = Socket,
        transport = Transport
    }};


%% Packet, indicates which session client wants to reconnect.
%% In this case, previous message only was ?START_GAME_PACKET(1)
handle_cast(
    {data, Data},
    #in_game_state{
        token = undefined,
        session_pid = undefined,
        game_stopping = false,
        tag = undefined,
        socket = Socket,
        transport = Transport
    } = State
) ->
    {Token, Tag} = binary_to_term(Data),
    case game_lobby:checkin(self(), Token, Tag) of
        {error, invalid_tag} ->
            {stop, invalid_tag, State};
        {error, session_expired} ->
            StartFrame = session_utils:make_server_frame( term_to_binary({Token, Tag})),
            Response = [
                ?START_GAME_PACKET(0), StartFrame, ?CANCEL_GAME_PACKET
            ],
            case Transport:send(Socket, Response) of
                ok ->
                    {noreply, #idle_state{ socket = Socket, transport = Transport}};
                _ ->
                    {stop, normal, State}
            end;
        {ok, Token} ->
            {noreply, State#in_game_state{ token = Token, is_reconnect = true }}
    end;

handle_cast(
    {command, ?START_GAME_PACKET},
    #in_game_state{} = State
) ->
    {stop, already_in_game, State};

handle_cast(
    {command, ?CANCEL_GAME_PACKET},
    #in_game_state{
        token = Token
    } = State
) ->
    game_lobby:cancel(Token),
        %% Doesnt matter, either this call succeeded, or not.
        %% Process have to receive #game_stop{} anyway, 'cause
        %% checkin and cancel are serialized through single process 'lobby_server'

    {noreply, State#in_game_state{game_stopping = true}};



handle_cast(
    {command, ?CANCEL_GAME_PACKET},
    #idle_state{

    } = State
) ->
    {stop, not_in_game, State};


%% After client decided to surrender, it send ?SURRENDER_PACKET.
%% This packet treats as typical turn, but after it client only
%% expects that game will be stopped by beer.
handle_cast(
    {command, ?SURRENDER_PACKET = _TurnData},
    #in_game_state{
        is_ours_turn = true,
        session_pid = SessionPid,
        waiting_surrender_ack = true,
        token = Token
    } = State
) when SessionPid =/= undefined ->
    %% TODO: Update wins count in profile
    {ok, _} = game_lobby:cancel(Token),
    {noreply, State#in_game_state{
        game_stopping = true
    }};

handle_cast(
    {command, TurnData},
    #in_game_state{
        is_ours_turn = true,
        session_pid = SessionPid,
        tag = Tag,
        waiting_surrender_ack = false
    } = State
) when SessionPid =/= undefined ->
    ok = game_session:make_turn(SessionPid, Tag, TurnData),
    {noreply, State};

handle_cast(
    {command, _TurnData},
    #in_game_state{
        is_ours_turn = true,
        session_pid = SessionPid,
        token = Token,
        waiting_surrender_ack = true
    } = State
) when SessionPid =/= undefined ->
    {ok, _} = game_lobby:cancel(Token),
    {stop, protocol_violation, State};


handle_cast(
    {command, _},
    State
) ->
    {stop, protocol_violation, State};

handle_cast(
    _,
    State
) ->
    {stop, unexpected_cast, State}.

handle_info(
    #game_start{
        session_pid = SessionPid,
        tag = Tag,
        token = Token,
        turn = Turn
    },
    #in_game_state{
        token = ActualToken,
        socket = Socket,
        transport = Transport,
        is_reconnect = IsReconnect
    } = State
) when ActualToken =:= Token ->
    TurnFlag = case Turn of true -> 1; false -> 0 end,
    StartFrame = session_utils:make_server_frame( term_to_binary({Token, Tag})),
    case Transport:send(Socket, [ ?START_GAME_PACKET(TurnFlag), StartFrame ]) of
        ok ->
            {noreply, State#in_game_state{
                tag = Tag,
                session_pid = SessionPid,
                is_ours_turn = Turn,
                game_stopping = true
            }};
        {error, _Reason} ->
            not IsReconnect andalso
                game_lobby:cancel(Token),
            {stop, normal, State}
    end;

handle_info(
    #game_stop{
        token = Token,
        session_pid = SessionPid,
        tag = Tag
    },
    #in_game_state{
        socket = Socket,
        transport = Transport,
        token = Token,
        session_pid = SessionPid,
        tag = Tag,
        game_stopping = true
    } = State
) ->
    case Transport:send(Socket, [?CANCEL_GAME_PACKET]) of
        ok ->
            {noreply, #idle_state{socket = Socket, transport = Transport}};
        {error, _Reason} ->
            {stop, normal, State}
    end;

handle_info(
    #peer_lost{

    },
    #in_game_state{

    } = State
) ->
    {noreply, State};

handle_info(
    #peer_turn{
        session_pid = SessionPid,
        data = ?SURRENDER_PACKET = Data
    },
    #in_game_state{
        session_pid = SessionPid,
        socket = Socket,
        transport = Transport,
        tag = PeerTag
    } = State
) when PeerTag =/= undefined ->
    case Transport:send(Socket, Data) of
        ok ->
            ok = game_session:ack_turn(SessionPid, PeerTag, Data),
            {noreply, State#in_game_state{is_ours_turn = true, waiting_surrender_ack = true}};
        _Another ->
            {stop, normal, State}
    end;

handle_info(
    #peer_turn{
        session_pid = SessionPid,
        data = Data
    },
    #in_game_state{
        session_pid = SessionPid,
        socket = Socket,
        transport = Transport,
        tag = PeerTag
    } = State
) when PeerTag =/= undefined ->
    case Transport:send(Socket, Data) of
        ok ->
            ok = game_session:ack_turn(SessionPid, PeerTag, Data),
            {noreply, State#in_game_state{is_ours_turn = true}};
        _Another ->
            {stop, normal, State}
    end;




handle_info(
    _, State
) ->
    {stop, unexpected_message, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


send_ping(Socket, Transport, SeqId) ->
    Transport:send(Socket, ?PING_PACKET(SeqId)).