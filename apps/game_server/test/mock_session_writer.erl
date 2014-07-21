
-module(mock_session_writer).
-author("Viacheslav V. Kovalev").

%% API
-export([
    send/2,
    close/1
]).

send(_, _) ->
    erlang:error(not_mocked, send).
close(_) ->
    ok.