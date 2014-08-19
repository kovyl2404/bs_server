
-module(smtp_notification_handler).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    init_handler/1,
    terminate_handler/1,
    handle_notification/3
]).

-record(
    smtp_handler, {
        smtp_params,
        from_field,
        email_type
    }
).

init_handler(Params) ->
    SmtpParams = proplists:get_value(smtp, Params),
    FromField = proplists:get_value(from_field, Params),
    EmailType = translate_type(proplists:get_value(email_type, Params)),
    {ok, #smtp_handler{
        smtp_params = SmtpParams,
        from_field = FromField,
        email_type = EmailType
    }}.

translate_type(EmailType) when is_binary(EmailType) ->
    EmailType;
translate_type(EmailType) when is_list(EmailType) ->
    list_to_binary(EmailType).


terminate_handler(_) ->
    ok.

handle_notification(
    Message, Params,
    #smtp_handler{
        smtp_params = SmtpParams,
        from_field = FromField,
        email_type = EmailType
    }
) ->
    Sender = proplists:get_value(username, SmtpParams),
    Receiver = proplists:get_value(receiver, Params),
    Subject = proplists:get_value(subject, Params),
    Email= {
        <<"text">>, EmailType, [
            {<<"From">>, [
                FromField, <<" <">>, Sender, <<">">>
            ]},
            {<<"To">>, Receiver},
            {<<"Subject">>, Subject}
        ], [],
        iolist_to_binary(Message)
    },
    MimeData = mimemail:encode(Email),
    SendResult =
        gen_smtp_client:send_blocking(
            {Sender, [Receiver], MimeData},
            SmtpParams
        ),
    {ok, SendResult}.


