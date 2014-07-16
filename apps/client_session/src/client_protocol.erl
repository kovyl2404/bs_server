
-module(client_protocol).

-author("Viacheslav V. Kovalev").
-behaviour(ranch_protocol).

%% API
-export([
    start_link/4
]).

-record(
    handler_state, {

    }
).


start_link(Ref, Socket, Transport, ProtocolOptions) ->
    Pid = spawn_link( fun() -> init_handler(Ref, Socket, Transport, ProtocolOptions) end ),
    {ok, Pid}.



init_handler(Ref, Socket, Transport, _ProtocolOptions) ->
    ok = ranch:accept_ack(Ref),
    loop_handler(Socket, Transport, #handler_state{}).


loop_handler(Socket, Transport, HandlerState) ->
    Transport:setopts(Socket, [{active, once}]),
    receive
        {tcp, _, _Data} ->
            loop_handler(Socket, Transport, HandlerState);
        {tcp_closed, _} ->
            ok = handle_close(Socket, Transport, HandlerState),
            ok = terminate(Socket, Transport, HandlerState);
        {tcp_error, _, Reason} ->
            ok = handle_error(Socket, Transport, HandlerState, Reason),
            ok = terminate(Socket, Transport, HandlerState)
    end.

handle_close(_, _, _) ->
    ok.

handle_error(_, _, _, _) ->
    ok.

terminate(_, _, _) ->
    ok.

