
-module(session_utils_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

strip_zeros_string_test() ->
    Example = <<$a, $b, $c, 0, 0, 0, 0>>,
    Res = session_utils:strip_trailing_zeros(Example),
    ?assertEqual(<<$a, $b, $c>>, Res).


allign_string_test() ->
    Example = <<$a, $b, $c>>,
    Res = session_utils:allign_string(Example, 5),
    ?assertEqual(<<$a, $b, $c, 0, 0>>, Res).

encode_decode_register_request_test() ->
    Packet = session_utils:encode_auth_request(<<"login">>, <<"password">>),
    {ok, {Login, Password}} = session_utils:decode_auth_request(iolist_to_binary(Packet)),
    [
        ?_assertEqual(64, iolist_size(Packet)),
        ?_assertEqual(<<"login">>, Login),
        ?_assertEqual(<<"password">>, Password)
    ].

encode_decode_register_response_test() ->
    Packet1 = session_utils:encode_auth_response(true),
    Packet2 = session_utils:encode_auth_response(false),
    Result1 = session_utils:decode_auth_response(iolist_to_binary(Packet1)),
    Result2 = session_utils:decode_auth_response(iolist_to_binary(Packet2)),
    [
        ?_assertEqual(1, iolist_size(Packet1)),
        ?_assertEqual(1, iolist_size(Packet2)),
        ?_assertEqual({ok, true}, Result1),
        ?_assertEqual({ok, false}, Result2)
    ].

encode_decode_profile_test() ->
    Profile = [
        {<<"rank">>, 1},
        {<<"experience">>, 2},
        {<<"reserved1">>, 1},
        {<<"reserved2">>, 2},
        {<<"reserved3">>, 3},
        {<<"reserved4">>, 5},
        {<<"reserved5">>, 5},
        {<<"reserved6">>, 6},
        {<<"reserved7">>, 7},
        {<<"score">>, 100},
        {<<"achievements">>, [1,2,3,4,5,6,7,8]}
    ],
    Packet = session_utils:encode_profile_request(Profile),
    {ok, DecodedProfile} = session_utils:decode_profile_request(Packet),

    [
        ?_assertEqual(lists:sort(Profile), lists:sort(DecodedProfile))
    ].