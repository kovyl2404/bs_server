
-module(client_emulator).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).
-include_lib("game_server/include/client_protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    start/2,
    stop/1,
    start_game/1,
    stop_game/1,
    surrender/1,
    make_turn/2,
    reconnect_game/2,
    set_owner/2
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
        socket,
        current_game,
        parser,
        owner
    }
).

%%%===================================================================
%%% API
%%%===================================================================

start(Host, Port) ->
    gen_server:start(?MODULE, {Host, Port, self()}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

start_game(ClientEmulator) ->
    ok = gen_server:cast(ClientEmulator, start_game).

stop_game(ClientEmulator) ->
    ok = gen_server:cast(ClientEmulator, stop_game).

surrender(ClientEmulator) ->
    ok = gen_server:cast(ClientEmulator, surrender).

make_turn(ClientEmulator, TurnData) ->
    ok = gen_server:cast(ClientEmulator, {make_turn, TurnData}).

set_owner(ClientEmulator, Pid) ->
    gen_server:call(ClientEmulator, {set_owner, Pid}).

reconnect_game(ClientEmulator, Data) ->
    gen_server:cast(ClientEmulator, {reconnect, Data}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init({Host, Port, Owner}) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, binary]),
    {ok, #state{
        socket = Socket,
        current_game = undefined,
        parser = protocol_parser:init(),
        owner = Owner
    }}.


handle_call({set_owner, Pid}, _From, State) ->
    {reply, ok, State#state{owner = Pid}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(
    start_game,
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, [?START_GAME_PACKET(0)]),
    {noreply, State};

handle_cast(
    {reconnect, Data},
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, [?START_GAME_PACKET(1), session_utils:make_server_frame(Data)]),
    {noreply, State};

handle_cast(
    stop_game,
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, [?CANCEL_GAME_PACKET]),
    {noreply, State};


handle_cast(
    surrender,
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, [?SURRENDER_PACKET(0,0,0)]),
    {noreply, State};

handle_cast(
    {make_turn, TurnData},
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, TurnData),
    {noreply, State};

handle_cast(
    stop, State
) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(
    {tcp, _Socket, Data},
    #state{
        parser = Parser
    } = State
) ->
    {ok, NewParser, Messages} = protocol_parser:feed(Parser, Data),
    lists:foreach(
        fun(Msg) ->
            erlang:send(self(), Msg)
        end, Messages
    ),
    {noreply, State#state{
        parser = NewParser
    }};

handle_info(
    {command, ?PING_PACKET = Ping},
    #state{
        socket = Socket
    } = State
) ->
    ok = gen_tcp:send(Socket, Ping),
    {noreply, State};

handle_info(
    {command, ?SURRENDER_PACKET},
    #state{
        owner = Owner
    } = State
) ->
    Owner ! {self(), surrender},
    {noreply, State};



handle_info(
    {command, ?START_GAME_PACKET(Data)},
    #state{
        socket = _Socket,
        owner = Owner
    } = State
) ->
    OursTurn =
        case Data of
            1 -> true;
            0 -> false
        end,
    ReconnectData =
        receive
            {data, Value} ->
                Value
        after 1000 ->
            erlang:error(game_start_fail, "Reconnect data was not received")
        end,
    Owner ! {self(), {game_start, OursTurn, ReconnectData}},
    {noreply, State};

handle_info(
    {command, ?CANCEL_GAME_PACKET},
    #state{
        owner = Owner
    } = State
) ->
    Owner ! {self(), game_stop},
    {noreply, State};

handle_info(
    {command, TurnData},
    #state{
        owner = Owner
    } = State
) ->
    Owner ! {self(), {turn,TurnData}},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, #state{socket = Socket} = _State) ->
    ok = gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
