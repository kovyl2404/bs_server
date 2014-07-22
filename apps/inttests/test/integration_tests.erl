
-module(integration_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("game_server/include/client_protocol.hrl").

before_test() ->
    ok = error_logger:tty(false),
    ok = application:start(game_lobby),
    ok = application:start(ranch),
    ok = application:start(game_server).


game_reconnect_test_() ->
    {setup,
        fun() -> ok end,
        fun(_) -> ok end,
        fun(_) ->
            {Time, Tests} =
                timer:tc(
                    fun() ->
                        lists:foldl(
                            fun(_I, Acc) ->
                                {ok, TmpFirstEmulator} = client_emulator:start("localhost", 7890),
                                {ok, TmpSecondEmulator} = client_emulator:start("localhost", 7890),

                                ok = client_emulator:start_game(TmpFirstEmulator),
                                timer:sleep(50),
                                ok = client_emulator:start_game(TmpSecondEmulator),
                                {ok, {game_start, FirstTurns, ReconnectData1}} = lobby_utils:wait_from_pid(TmpFirstEmulator, 100),
                                {ok, {game_start, _SecondTurns, ReconnectData2}} = lobby_utils:wait_from_pid(TmpSecondEmulator, 100),

                                {FirstEmulator, SecondEmulator, ReconnectData} = %{TmpFirstEmulator, TmpSecondEmulator, ReconnectData2},
                                    case FirstTurns of
                                        true ->
                                            {TmpFirstEmulator, TmpSecondEmulator, ReconnectData2};
                                        _ ->
                                            {TmpSecondEmulator, TmpFirstEmulator, ReconnectData1}
                                    end,
                                EmulatorMonitor = monitor(process, SecondEmulator),

                                ok = client_emulator:stop(SecondEmulator),
                                {ok, normal} = lobby_utils:wait_process_down_reason(EmulatorMonitor, 100),
                                ok = client_emulator:make_turn(FirstEmulator, <<1,2,3,4>>),

                                {ok, NewSecondEmulator} = client_emulator:start("localhost", 7890),
                                ok = client_emulator:reconnect_game(NewSecondEmulator, ReconnectData),
                                ReconnectResult = lobby_utils:wait_from_pid(NewSecondEmulator, 100),

                                TurnResult = lobby_utils:wait_from_pid(NewSecondEmulator, 100),
                                ok = client_emulator:surrender(NewSecondEmulator),

                                SurrenderResult = lobby_utils:wait_from_pid(FirstEmulator, 100),
                                ok = client_emulator:surrender(FirstEmulator),

                                GameStopResult1 = lobby_utils:wait_from_pid(FirstEmulator, 100),
                                GameStopResult2 = lobby_utils:wait_from_pid(NewSecondEmulator, 100),

                                ok = client_emulator:stop(FirstEmulator),
                                ok = client_emulator:stop(NewSecondEmulator),

                                Acc ++ [
                                    ?_assertMatch({ok, {game_start, false, _}}, ReconnectResult),
                                    ?_assertMatch({ok, {turn, <<1,2,3,4>>}}, TurnResult),
                                    ?_assertMatch({ok, surrender}, SurrenderResult),
                                    ?_assertMatch({ok, game_stop}, GameStopResult1),
                                    ?_assertMatch({ok, game_stop}, GameStopResult2)
                                ]
                            end, [], lists:seq(1, 1000)
                        )
                    end
                ),
            ?debugFmt("Reconnection test: Total execution time ~p uS, ~p games played",[Time, 1000]),
            Tests
        end
    }.



game_start_stop_test_() ->
    {setup,
        fun() ->
            {ok, FirstEmulator} = client_emulator:start("localhost", 7890),
            {ok, SecondEmulator} = client_emulator:start("localhost", 7890),
            {FirstEmulator, SecondEmulator}
        end,
        fun({FirstEmulator,SecondEmulator}) ->
            ok = client_emulator:stop(FirstEmulator),
            ok = client_emulator:stop(SecondEmulator)
        end,
        fun({FirstEmulator,SecondEmulator}) ->
            {Time, Tests} =
                timer:tc(
                    fun() ->
                        lists:foldl(
                            fun(_, Acc) ->
                                ok = client_emulator:start_game(FirstEmulator),
                                ok = client_emulator:start_game(SecondEmulator),
                                FirstClientGameStart = lobby_utils:wait_from_pid(FirstEmulator, 100),
                                SecondClientGameStart = lobby_utils:wait_from_pid(SecondEmulator, 100),

                                ok = client_emulator:stop_game(FirstEmulator),
                                FirstClientGameStop = lobby_utils:wait_from_pid(FirstEmulator, 100),
                                SecondClientGameStop = lobby_utils:wait_from_pid(SecondEmulator, 100),
                                Tests = [
                                    ?_assertMatch({ok, {game_start, _, _}}, FirstClientGameStart),
                                    ?_assertMatch({ok, {game_start, _, _}}, SecondClientGameStart),
                                    ?_assertMatch({ok, game_stop}, FirstClientGameStop),
                                    ?_assertMatch({ok, game_stop}, SecondClientGameStop)
                                ],
                                Acc ++ Tests
                            end, [], lists:seq(1, 500)
                        )
                    end
                ),
            ?debugFmt("Start/stop game test: Total execution time ~p uS, ~p games played",[Time, 500]),
            Tests

        end
    }.

game_play_test_() ->
    {setup,
        fun() ->
            {ok, FirstEmulator} = client_emulator:start("localhost", 7890),
            {ok, SecondEmulator} = client_emulator:start("localhost", 7890),
            {FirstEmulator, SecondEmulator}
        end,
        fun({FirstEmulator,SecondEmulator}) ->
            ok = client_emulator:stop(FirstEmulator),
            ok = client_emulator:stop(SecondEmulator)
        end,
        fun({FirstEmulator,SecondEmulator}) ->
            TestHost = self(),
            PlayFun =
                fun(Emulator) ->
                    ok = client_emulator:set_owner(Emulator, self()),
                    Result = play_game(Emulator, 10),
                    TestHost ! {self(), Result}
                end,
            {Time, Tests} =
                timer:tc(
                    fun() ->
                        lists:foldl(
                            fun(_, Acc) ->
                                Player1 = spawn( fun() -> PlayFun(FirstEmulator) end ),
                                Player2 = spawn( fun() -> PlayFun(SecondEmulator) end ),

                                Result1 = lobby_utils:wait_from_pid(Player1, 1000),
                                Result2 = lobby_utils:wait_from_pid(Player2, 1000),
                                Tests = [
                                    ?_assert(
                                        (Result1 =:= {ok, win} andalso Result2 =:= {ok, surrender})
                                            orelse (Result2 =:= {ok, win} andalso Result1 =:= {ok, surrender})
                                    )
                                ],
                                Acc ++ Tests
                            end, [], lists:seq(1, 500)
                        )
                    end
                ),
            ?debugFmt("Playing game test: Total execution time ~p uS ~p games player",[Time, 500]),
            Tests

        end
    }.


play_game(ClientEmulator, TurnsToMake) ->
    ok = client_emulator:start_game(ClientEmulator),
    {ok, {game_start, IsOursTurn, _}} = lobby_utils:wait_from_pid(ClientEmulator, 1000),
    case IsOursTurn of
        true ->
            make_turn(ClientEmulator, TurnsToMake);
        false ->
            {ok, {turn, _}} = lobby_utils:wait_from_pid(ClientEmulator, 1000),
            make_turn(ClientEmulator, TurnsToMake)
    end.


make_turn(ClientEmulator, 0) ->
    ok = client_emulator:surrender(ClientEmulator),
    case lobby_utils:wait_from_pid(ClientEmulator, 1000) of
        {ok, game_stop} ->
            surrender;
        _ ->
            error
    end;
make_turn(ClientEmulator, TurnsLeft) ->
    ok = client_emulator:make_turn(ClientEmulator, <<1,2,3,4>>),
    case lobby_utils:wait_from_pid(ClientEmulator, 1000) of
        {ok, surrender} ->
            ok = client_emulator:surrender(ClientEmulator),
            case lobby_utils:wait_from_pid(ClientEmulator, 1000) of
                {ok, game_stop} ->
                    win;
                _ ->
                    error
            end;
        {ok, {turn, _}} ->
            make_turn(ClientEmulator, TurnsLeft-1);
        _ ->
            error
    end.

after_test() ->
    ok = application:stop(game_server),
    ok = application:stop(ranch),
    ok = application:stop(game_lobby),
    ok = error_logger:tty(true).
