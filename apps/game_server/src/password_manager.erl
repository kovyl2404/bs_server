
-module(password_manager).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

-include_lib("game_server/include/logging.hrl").

-define(CONFIRMATION_CODE_LENGTH, 5).


%% API
-export([start_link/1]).

-export([
    request/1,
    commit/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(
    state, {
        request_timeout
    }
).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

request(Profile) ->
    Login = proplists:get_value(<<"login">>, Profile),
    Email = proplists:get_value(<<"email">>, Profile),
    case Email of
        undefined ->
            ?ERROR("User ~s have no email, can't send password recovery notification",[Login]),
            {error, email_not_found};
        _ ->
            ?INFO("User ~s requested to reset password, sending confirmation code to ~s",[Login, Email]),
            ConfirmationCode = get_random_string(?CONFIRMATION_CODE_LENGTH, "1234567890"),
            ok = gen_server:call(?SERVER, {request, Login, ConfirmationCode}),
            NotificationParams = [
                {subject, <<"Password recovery">>},
                {username, Login},
                {receiver, Email},
                {confirmation_code, ConfirmationCode}
            ],
            {ok, SendNotificationResult} =
                notifications:send_notification(
                    smtp_notification_handler, password_recovery,
                    NotificationParams
                ),
            ?INFO("Password reset notification for ~s (~s): result is ~p", [Email, Login, SendNotificationResult])
    end.

commit(Profile, ConfirmationCode) ->
    Login = proplists:get_value(login, Profile),
    gen_server:call(?SERVER, {commit, Login, ConfirmationCode}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Params) ->
    RequestTimeoutSec = proplists:get_value(password_reset_timeout, Params),
    ets:new(?MODULE, [protected, set, named_table]),
    {ok, #state{
        request_timeout = RequestTimeoutSec*1000
    }}.


handle_call(
    {request, Login, ConfirmationCode},
    _From,
    #state{
        request_timeout = RequestTimeout
    }
) ->
    true = ets:insert(?MODULE, {Login, ConfirmationCode}),
    erlang:send_after(
        RequestTimeout, self(),
        {request_expired, Login, ConfirmationCode}
    ),
    ok;
handle_call(
    {commit, Login, ConfirmationCode},
    _From,
    State
) ->
    case ets:lookup(?MODULE, Login) of
        [{Login, ConfirmationCode}] ->
            {reply, ok, State};
        [{Login, _}] ->
            {reply, {error, invalid_code}, State};
        _ ->
            {reply, {error, request_expired}, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(
    {request_expired, Login, ConfirmationCode},
    #state{

    } = State
) ->
    case ets:lookup(?MODULE, Login) of
        [{Login, ConfirmationCode}] ->
            ets:delete_object(?MODULE, {Login, ConfirmationCode});
        _ ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)),
            AllowedChars) | Acc]
    end, [], lists:seq(1, Length)).