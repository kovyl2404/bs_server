-module(session_utils).
-author("Viacheslav V. Kovalev").

-include_lib("game_server/include/client_protocol.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("game_server/include/metrics.hrl").
-include_lib("game_lobby/include/metrics.hrl").

%% API
-export([
    make_server_frame/1,
    strip_trailing_zeros/1,
    allign_string/2,
    encode_auth_request/2,
    decode_auth_request/1,
    encode_auth_response/1,
    decode_auth_response/1,
    encode_profile_request/1,
    decode_profile_request/1,
    encode_top_response/1,
    decode_top_request/1,
    decode_server_status_request/1,
    encode_server_status_request/1,
    encode_server_status_response/4,
    get_basic_metrics/0,
    encode_peer_status/1,
    encode_password_reset_request/1,
    decode_password_reset_request/1,
    encode_password_reset_response/1,
    decode_commit_password_request/1,
    encode_commit_password_result/1,
    encode_register_request/3,
    decode_register_request/1
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
        allign_string(Login, 32),
        allign_string(Password, 32)
    ].

encode_register_request(Login, Password, Email) ->
    [
        allign_string(Login, 32),
        allign_string(Password, 32),
        allign_string(Email, 64)
    ].

decode_register_request(Packet) ->
    case Packet of
        <<Login:32/binary, Password:32/binary, Email:64/binary>> ->
            {ok, {
                strip_trailing_zeros(Login),
                strip_trailing_zeros(Password),
                strip_trailing_zeros(Email)
            }};
        _ ->
            {error, invalid_packet}
    end.

decode_auth_request(Packet) ->
    case Packet of
        <<Login:32/binary, Password:32/binary>> ->
            {ok, {strip_trailing_zeros(Login), strip_trailing_zeros(Password)}};
        _ ->
            {error, invalid_packet}
    end.

encode_profile_request(RawProfile) ->
    Profile = orddict:from_list(RawProfile),
    Rank = orddict:fetch(<<"rank">>, Profile),
    Experience = orddict:fetch(<<"experience">>, Profile),
    Reserved1 = orddict:fetch(<<"reserved1">>, Profile),
    Reserved2 = orddict:fetch(<<"reserved2">>, Profile),
    Reserved3 = orddict:fetch(<<"reserved3">>, Profile),
    Reserved4 = orddict:fetch(<<"reserved4">>, Profile),
    Reserved5 = orddict:fetch(<<"reserved5">>, Profile),
    Reserved6 = orddict:fetch(<<"reserved6">>, Profile),
    Reserved7 = orddict:fetch(<<"reserved7">>, Profile),
    Score = orddict:fetch(<<"score">>, Profile),
    Achievements = list_to_binary(orddict:fetch(<<"achievements">>, Profile)),
    Timestamp = orddict:fetch(<<"timestamp">>, Profile),
    <<
        Rank:4/unsigned-big-integer-unit:8,
        Experience:4/unsigned-big-integer-unit:8,
        Reserved1:4/unsigned-big-integer-unit:8,
        Reserved2:4/unsigned-big-integer-unit:8,
        Reserved3:4/unsigned-big-integer-unit:8,
        Reserved4:4/unsigned-big-integer-unit:8,
        Reserved5:4/unsigned-big-integer-unit:8,
        Reserved6:4/unsigned-big-integer-unit:8,
        Reserved7:4/unsigned-big-integer-unit:8,
        Score:4/unsigned-big-integer-unit:8,
        Achievements:8/binary-unit:8,
        Timestamp:8/unsigned-big-integer-unit:8
    >>.

decode_profile_request(Packet) ->
    case Packet of
        <<Rank:4/unsigned-big-integer-unit:8,
            Experience:4/unsigned-big-integer-unit:8,
            Reserved1:4/unsigned-big-integer-unit:8,
            Reserved2:4/unsigned-big-integer-unit:8,
            Reserved3:4/unsigned-big-integer-unit:8,
            Reserved4:4/unsigned-big-integer-unit:8,
            Reserved5:4/unsigned-big-integer-unit:8,
            Reserved6:4/unsigned-big-integer-unit:8,
            Reserved7:4/unsigned-big-integer-unit:8,
            Score:4/unsigned-big-integer-unit:8,
            Achievements:8/binary-unit:8,
            Timestamp:8/unsigned-big-integer-unit:8
        >> ->
            Profile = [
                {<<"rank">>, Rank},
                {<<"experience">>, Experience},
                {<<"reserved1">>, Reserved1},
                {<<"reserved2">>, Reserved2},
                {<<"reserved3">>, Reserved3},
                {<<"reserved4">>, Reserved4},
                {<<"reserved5">>, Reserved5},
                {<<"reserved6">>, Reserved6},
                {<<"reserved7">>, Reserved7},
                {<<"score">>, Score},
                {<<"achievements">>, binary_to_list(Achievements)},
                {<<"timestamp">>, Timestamp}
            ],
            {ok, Profile};
        _ ->
            {error, invalid_packet}
    end.

encode_top_response(Top) ->
    Count = length(Top),
    TopData =
        lists:foldl(
            fun({Login, Rank}, Acc) ->
                [ [allign_string(Login, 32), <<Rank:4/unsigned-big-integer-unit:8>>] | Acc ]
            end, [], lists:reverse(Top)
        ),
    [
        <<Count>>, TopData
    ].

decode_top_request(TopRequest) ->
    case TopRequest of
        <<TopCount>> ->
            {ok, TopCount};
        _ -> {error, invalid_packet}
    end.

encode_auth_response(ok) ->
    <<0>>;
encode_auth_response(incorrect_login) ->
    <<1>>;
encode_auth_response(incorrect_password) ->
    <<2>>;
encode_auth_response(already_authenticated) ->
    <<3>>.

decode_auth_response(<<2>>) ->
    {ok, incorrect_password};
decode_auth_response(<<1>>) ->
    {ok, incorrect_login};
decode_auth_response(<<0>>) ->
    {ok, ok};
decode_auth_response(_X) ->
    {error, invalid_packet}.

decode_server_status_request(<<ClientVersion:2/big-unsigned-integer-unit:8>>) ->
    {ok, ClientVersion};
decode_server_status_request(_) ->
    {error, invalid_packet}.

encode_server_status_request(ClientVersion) ->
    <<ClientVersion:2/big-unsigned-integer-unit:8>>.

encode_peer_status(true) ->
    <<1>>;
encode_peer_status(false) ->
   <<0>>.

encode_server_status_response(IsVersionSupported, TotalConnections, RunningGames, WaitingGames) ->
    SupportedFlag = case IsVersionSupported of true -> 1; false -> 0 end,
    <<SupportedFlag,
        TotalConnections:4/big-unsigned-integer-unit:8,
        RunningGames:4/big-unsigned-integer-unit:8,
        WaitingGames:4/big-unsigned-integer-unit:8
    >>.

get_basic_metrics() ->
    ClientConnections = folsom_metrics:get_metric_value(?GAME_SERVER_CONNECTIONS_METRIC),
    GamesWaiting = folsom_metrics:get_metric_value(?WAITING_GAMES_METRIC),
    GamesRunning = folsom_metrics:get_metric_value(?RUNNING_GAMES_METRIC),
    {ClientConnections, GamesRunning, GamesWaiting}.


decode_password_reset_request(Data) ->
    case Data of
        <<_:32/binary-unit:8>> ->
            {ok, strip_trailing_zeros(Data)};
        _ ->
            {error, invalid_packet}
    end.

encode_password_reset_request(Login) ->
    allign_string(Login, 32).

encode_password_reset_response(ok) ->
    <<0>>;
encode_password_reset_response(incorrect_login) ->
    <<1>>.

decode_commit_password_request(Data) ->
    case Data of
        <<
            Login:32/binary-unit:8,
            ConfirmationCode:32/binary-unit:8,
            NewPassword:32/binary-unit:8
        >> ->
            {ok,
                strip_trailing_zeros(Login),
                strip_trailing_zeros(ConfirmationCode),
                strip_trailing_zeros(NewPassword)
            };
        _ ->
            {error, invalid_packet}
    end.

encode_commit_password_result(ok) ->
    <<0>>;
encode_commit_password_result(incorrect_login) ->
    <<1>>;
encode_commit_password_result(request_expired) ->
    <<2>>;
encode_commit_password_result(invalid_code) ->
    <<3>>.
