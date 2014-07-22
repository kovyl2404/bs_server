
-module(mock_session_writer).
-author("Viacheslav V. Kovalev").

%% API
-export([
    send/2
]).

send(_, _) ->
    erlang:error(not_mocked, send).

