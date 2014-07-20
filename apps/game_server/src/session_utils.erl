-module(session_utils).
-author("Viacheslav V. Kovalev").

-include_lib("game_server/include/client_protocol.hrl").

%% API
-export([
    make_server_frame/1
]).

make_server_frame(Binary) ->
    PayloadLength = iolist_size(Binary),
    [
        ?SERVER_PACKET(PayloadLength), Binary
    ].