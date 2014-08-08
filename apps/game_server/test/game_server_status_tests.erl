
-module(game_server_status_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").

fixture(Inst) ->
    {setup,
        fun() ->
            ok = application:start(folsom),
            ok = meck:new(mock_session_writer, [passthrough]),
            ok = meck:new(game_server, [passthrough]),
            ok = meck:new(session_utils, [passthrough])
        end,
        fun(_) ->
            ok = meck:unload(session_utils),
            ok = meck:unload(game_server),
            ok = meck:unload(mock_session_writer),
            ok = application:stop(folsom)
        end,
        Inst
    }.

valid_vsn_test_() ->
    fixture(
        fun(_) ->

            ok = meck:expect(mock_session_writer, send, 2, meck:val(ok)),
            ok = meck:expect(game_server, is_supported_vsn, 1, meck:val(true)),
            ok = meck:expect(session_utils, get_basic_metrics, 0, meck:val({0,0,0}) ),
            {ok, idle, InitState} = client_session:init({mock, mock_session_writer, ?MODULE}),
            ServerStatusRequest = session_utils:encode_server_status_request(1),
            ProcessResult =
                client_session:idle(
                    {data, iolist_to_binary([?SERVER_STATUS_TAG, ServerStatusRequest])},
                    InitState
                ),

            ExectedStatusResponse = session_utils:encode_server_status_response(true, 0, 0, 0),
            ExpectedFrame = session_utils:make_server_frame([?SERVER_STATUS_TAG, ExectedStatusResponse]),
            ValidResponseSent = meck:called(mock_session_writer, send, [mock, ExpectedFrame]),

            [
                ?_assertMatch({next_state, idle, _}, ProcessResult),
                ?_assert(ValidResponseSent)
            ]

        end
    ).

invalid_vsn_test_() ->
    fixture(
        fun(_) ->
            ok = meck:expect(mock_session_writer, send, 2, meck:val(ok)),
            ok = meck:expect(game_server, is_supported_vsn, 1, meck:val(false)),
            ok = meck:expect(session_utils, get_basic_metrics, 0, meck:val({0,0,0}) ),

            {ok, idle, InitState} = client_session:init({mock, mock_session_writer, ?MODULE}),
            ServerStatusRequest = session_utils:encode_server_status_request(3),
            ProcessResult =
                client_session:idle(
                    {data, iolist_to_binary([?SERVER_STATUS_TAG, ServerStatusRequest])},
                    InitState
                ),

            ExectedStatusResponse = session_utils:encode_server_status_response(false, 0, 0, 0),
            ExpectedFrame = session_utils:make_server_frame([?SERVER_STATUS_TAG, ExectedStatusResponse]),
            ValidResponseSent = meck:called(mock_session_writer, send, [mock, ExpectedFrame]),

            [
                ?_assertMatch({stop, invalid_vsn, _}, ProcessResult),
                ?_assert(ValidResponseSent)
            ]

        end
    ).

valid_vsn_guest_test_() ->
    fixture(
        fun(_) ->

            ok = meck:expect(mock_session_writer, send, 2, meck:val(ok)),
            ok = meck:expect(game_server, is_supported_vsn, 1, meck:val(true)),
            ok = meck:expect(session_utils, get_basic_metrics, 0, meck:val({0,0,0}) ),
            {ok, _, InitState} = client_session:init({mock, mock_session_writer, ?MODULE}),
            ServerStatusRequest = session_utils:encode_server_status_request(1),
            ProcessResult =
                client_session:guest(
                    {data, iolist_to_binary([?SERVER_STATUS_TAG, ServerStatusRequest])},
                    InitState
                ),

            ExectedStatusResponse = session_utils:encode_server_status_response(true, 0, 0, 0),
            ExpectedFrame = session_utils:make_server_frame([?SERVER_STATUS_TAG, ExectedStatusResponse]),
            ValidResponseSent = meck:called(mock_session_writer, send, [mock, ExpectedFrame]),

            [
                ?_assertMatch({next_state, guest, _}, ProcessResult),
                ?_assert(ValidResponseSent)
            ]

        end
    ).