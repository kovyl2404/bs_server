
-module(notifications_tests).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").


before_test() ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(merl),
    ok = application:start(erlydtl).

temlate_test_() ->
    {setup,
        fun() -> notifications:init_templates() end,
        fun(_) -> notifications:terminate_templates() end,
        fun(_) ->
            SuccessResult = notifications:format_notification(hello, [{user_name, <<"kovyl">>}]),
            FailResult = notifications:format_notification(unknown_template, [{user_name, <<"kovyl">>}]),
            [
                ?_assertMatch({ok, [<<"Hello ">>, <<"kovyl">>, <<"!">>]}, SuccessResult),
                ?_assertMatch({error, template_not_found}, FailResult)
            ]
        end
    }.

handler_test_() ->
    TargetPath = "/tmp/battleship/notifications/",
    NotificationMessage = <<"Hello!">>,
    Handlers = [
        {file_notification_handler, [{path, TargetPath}]}
    ],
    {setup,
        fun() ->
            os:cmd(["rm -r " ++ TargetPath]),
            notifications:init_handlers(Handlers)
        end,
        fun(_) ->
            notifications:terminate_handlers()
        end,
        fun(_) ->
            HandlerInitialized = filelib:is_dir(TargetPath),
            HandleResult =
                notifications:handle(
                    file_notification_handler, NotificationMessage, [
                        {receiver, "test"}
                    ]
                ),
            ExpectedFile = filename:join([TargetPath, "test"]),
            Content = file:read_file(ExpectedFile),

            UnknownHandlerResult =
                notifications:handle(
                    some_unknown_handler, NotificationMessage, [{receiver, "test"}]
                ),
            [
                ?_assert(HandlerInitialized),
                ?_assertEqual(ok, HandleResult),
                ?_assertEqual({ok, <<"Hello!">>}, Content),
                ?_assertEqual({error, handler_not_found}, UnknownHandlerResult)
            ]
        end
    }.

smtp_send_test_() ->
    {setup,
        fun() ->
            notifications:start_smtp()
        end,
        fun(_) ->
            notifications:stop_smtp()
        end,
        fun(_) ->
            Res =
                notifications:send_notification(
                    smtp_notification_handler, password_recovery,
                    [
                        {username, "Some guy"},
                        {subject, <<"Password recovery">>},
                        {receiver, <<"someguy@example.net">>},
                        {confirmation_code, 12345}
                    ]
                ),
            [
                ?_assertMatch({ok, _}, Res)
            ]
        end
    }.

after_test() ->
    ok = application:stop(erlydtl),
    ok = application:stop(merl),
    ok = application:stop(syntax_tools),
    ok = application:stop(compiler).


