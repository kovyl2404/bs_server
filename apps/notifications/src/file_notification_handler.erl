
-module(file_notification_handler).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    init_handler/1,
    handle_notification/3,
    terminate_handler/1
]).


init_handler(InitParams) ->
    Path = proplists:get_value(path, InitParams),
    ok = filelib:ensure_dir(Path),
    {ok, Path}.


handle_notification(Notification, NotificationParams, Path) ->
    Receiver = proplists:get_value(receiver, NotificationParams),
    ReceiverFile = filename:join(Path, Receiver),
    file:write_file(ReceiverFile, Notification).

terminate_handler(_State) ->
    ok.