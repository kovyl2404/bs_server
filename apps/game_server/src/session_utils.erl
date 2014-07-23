-module(session_utils).
-author("Viacheslav V. Kovalev").

-include_lib("game_server/include/client_protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    make_server_frame/1,
    strip_trailing_zeros/1,
    allign_string/2,
    encode_auth_request/2,
    decode_auth_request/1,
    encode_auth_response/1,
    decode_auth_response/1
]).

make_server_frame(Iolist) ->
    PayloadLength = iolist_size(Iolist),
    [
        ?SERVER_PACKET(PayloadLength), Iolist
    ].

allign_string(Binary, Length) ->
    case byte_size(Binary) of
        Value when Value>Length ->
            erlang:error(string_too_long);
        Value when Value =:= Length ->
            Binary;
        Value ->
            AppendLength = Length - Value,
            Padding = list_to_binary(lists:duplicate(AppendLength, 0)),
            <<Binary/binary, Padding/binary>>
    end.

strip_trailing_zeros(Binary) ->
    list_to_binary( lists:reverse(do_strip_zeros( lists:reverse( binary:bin_to_list(Binary) ) )) ).

do_strip_zeros( [ 0 | Rest]) ->
    do_strip_zeros(Rest);
do_strip_zeros(Rest) ->
    Rest.

encode_auth_request(Login, Password) ->
    [
        allign_string(Login, 64),
        allign_string(Password, 64)
    ].

decode_auth_request(Packet) ->
    case Packet of
        <<Login:64/binary, Password:64/binary>> ->
            {ok, {strip_trailing_zeros(Login), strip_trailing_zeros(Password)}};
        _ ->
            {error, invalid_packet}
    end.

encode_auth_response(true) ->
    <<1>>;
encode_auth_response(false) ->
    <<0>>.

decode_auth_response(<<1>>) ->
    {ok, true};
decode_auth_response(<<0>>) ->
    {ok, false};
decode_auth_response(_X) ->
    {error, invalid_packet}.