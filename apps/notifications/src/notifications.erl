-module(notifications).

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").

-export([
    default_templates_dir/0,
    init_handlers/1,
    terminate_handlers/0,
    handle/3,
    init_templates/0,
    init_templates/1,
    format_notification/2,
    terminate_templates/0,
    send_notification/3
]).

-export([start_smtp/0, stop_smtp/0, start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

default_templates_dir() ->
    code:priv_dir(notifications).

init_handlers(Handlers) ->
    ets:new(?MODULE, [protected, set, named_table]),
    lists:foreach(
        fun({HandlerModule, HandlerParams}) ->
            {ok, HandlerState} = HandlerModule:init_handler(HandlerParams),
            true = ets:insert(?MODULE, {HandlerModule, HandlerState})
        end, Handlers
    ),
    ok.

terminate_handlers() ->
    ets:foldl(
        fun({HandlerModule, HandlerState}, _) ->
            ok = HandlerModule:terminate_handler(HandlerState)
        end, ok, ?MODULE
    ),
    ets:delete(?MODULE),
    ok.


handle(HandlerModule, NotificationMessage, NotificationParams) ->
    case ets:lookup(?MODULE, HandlerModule) of
        [{HandlerModule, HandlerState}] ->
            HandlerModule:handle_notification(NotificationMessage, NotificationParams, HandlerState);
        _ ->
            {error, handler_not_found}
    end.

init_templates() ->
    init_templates(filename:join([code:priv_dir(notifications), "templates"])).

init_templates(TemplatesDir) ->
    Wildcard = filename:join([TemplatesDir, "*.dtl"]),
    TemplateFiles = filelib:wildcard(Wildcard),
    lists:foreach(
        fun(File) ->
            TemplateName = list_to_atom(filename:basename(File, ".dtl")),
            {ok, TemplateName} = erlydtl:compile_file(File, TemplateName)
        end, TemplateFiles
    ).

format_notification(Template, Params) when is_atom(Template) ->
    case module_loaded(Template) of
        true ->
            Template:render(Params);
        false ->
            {error, template_not_found}
    end.

terminate_templates() ->
    ok.

send_notification(Handler, Template, Args) ->
    case format_notification(Template, Args) of
        {ok, Message} ->
            handle(Handler, Message, Args);
        Error ->
            Error
    end.


start_smtp() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(gen_smtp),
    application:start(notifications).

stop_smtp() ->
    application:stop(notifications),
    application:stop(gen_smtp),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(asn1),
    application:stop(crypto).

start() ->
    application:start(merl),
    application:start(erlydtl),
    start_smtp(),
    application:start(notifications).




start(_StartType, _StartArgs) ->
    {ok, Handlers} = application:get_env(notifications, handlers),
    ok = init_templates(),
    ok = init_handlers(Handlers),
    notifications_sup:start_link().

stop(_State) ->
    ok = terminate_handlers(),
    ok = terminate_templates(),
    ok.
