-module(client_session_proc).

-behaviour(gen_server).

-include_lib("client_session/include/client_protocol.hrl").
-include_lib("game_lobby/include/common.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([
    send_command/2
]).

-export([
    start/2,
    start_link/2,
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
        game_stopping = false
    }
).

send_command(SessionPid, Command) ->
    gen_server:cast(SessionPid, Command).

start_link(Socket, Transport) ->
    gen_server:start_link(
        ?MODULE, {Socket, Transport}, []
    ).

start(Socket, Transport) ->
    gen_server:start(
        ?MODULE, {Socket, Transport}, []
    ).


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
    {command, ?START_GAME_PACKET},
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

handle_cast(
    {command, ?START_GAME_PACKET},
    #in_game_state{} = State
) ->
    {stop, already_in_game, State};

handle_cast(
    {command, ?CANCEL_GAME_PACKET},
    #in_game_state{
        token = Token,
        session_pid = SessionPid,
        tag = Tag
    } = State
) ->
    case SessionPid of
        undefined ->
            {ok, Token} = game_lobby:cancel(self(), Token);
        Value when is_pid(Value) ->
            ok = game_session:stop_game(SessionPid, Tag)
    end,
    {noreply, State#in_game_state{game_stopping = true}};

handle_cast(
    {command, ?CANCEL_GAME_PACKET},
    #idle_state{

    } = State
) ->
    {stop, not_in_game, State};

handle_cast(
    _, State
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
        transport = Transport
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
            game_session:stop_game(SessionPid, Tag),
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
    _, State
) ->
    {stop, unexpected_message, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.