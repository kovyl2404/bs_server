
-module(database_utils).
-author("Viacheslav V. Kovalev").

%% API
-export([
    now_timestamp/0,
    password_digest/2,
    format_date/1
]).


now_timestamp() ->
    {MegaSeconds, Seconds, _} = erlang:now(),
    MegaSeconds * Seconds.

password_digest(_, _) ->
    <<"test">>.

format_date({Mega, Seconds, _}) ->
    Mega*1000000 + Seconds.
