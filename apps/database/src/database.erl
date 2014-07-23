-module(database).

-behaviour(application).

-export([
    register/2,
    login/2,
    get_by_id/1,
    set_field/3,
    get_top/1,
    increase_field/2,
    update_profile/2,
    reinitialize/0
]).

-export([
    stop_deps/0,
    start_deps/0,
    start/0,
    stop/0
]).


%% Application callbacks
-export([start/2, stop/1]).


-include_lib("eunit/include/eunit.hrl").
-author("Viacheslav V. Kovalev").



start_deps() ->
    ok = hackney:start(),
    ok = couchbeam:start().

stop_deps() ->
    ok = couchbeam:stop(),
    ok = hackney:stop().

start() ->
    ok = start_deps(),
    ok = application:start(database).

stop() ->
    ok = application:stop(database),
    ok = stop_deps().

reinitialize() ->
    {ok, BackendModule} = application:get_env(database, backend),
    {ok, BackendConfig} = application:get_env(database, backend_config),
    BackendModule:reinitialize(BackendConfig).




%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, BackendModule} = application:get_env(database, backend),
    {ok, BackendConfig} = application:get_env(database, backend_config),
    {ok, BackendInst} = BackendModule:init(BackendConfig),
    ets:new(?MODULE, [public, named_table]),
    ets:insert(?MODULE, {backend, {BackendModule, BackendInst}}),
    database_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API functions
%% ===================================================================


get_top(Count) ->
    {BackendModule, BackendState} = get_backend(),
    BackendModule:get_top(Count, BackendState).

get_by_id(Login) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {ok, {Profile}} ->
            {ok, Profile};
        Error ->
            Error
    end.

register(Login, Password) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            create_profile(Login, Password);
        {ok, _} ->
            {error, already_registered}
    end.



create_profile(Login, Password) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:create_profile( init_profile(Login, Password), BackendState) of
        {ok, {Doc}} ->
            {ok, Doc};
        {error, _} = Error ->
            Error
    end.

login(Login, Password) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            {error, not_found};
        {ok, {Profile}} ->
            case password_correct(Password, Profile) of
                true ->
                    {ok, Profile};
                false ->
                    {error, not_found}
            end
    end.

update_profile(NewProfileVersion, Login) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            {error, not_found};
        {ok, {Profile}} ->
            UpdatedProfile = update_fields(Profile, NewProfileVersion),
            case BackendModule:create_profile( {UpdatedProfile}, BackendState ) of
                {ok, {Doc}} ->
                    {ok, Doc};
                {error, _} = Error ->
                    Error
            end
    end.


update_fields(Profile, NewProfileVersion) ->
    SortedProfile = orddict:from_list(Profile),
    UpdateFields = [
        <<"rank">>, <<"experience">>, <<"achievements">>,
        <<"reserved1">>, <<"reserved2">>, <<"reserved3">>,
        <<"reserved4">>, <<"reserved5">>, <<"reserved6">>,
        <<"reserved7">>
    ],
    lists:foldl(
        fun(Field, Acc) ->
            case lists:keyfind(Field, 1, NewProfileVersion) of
                {_, Value} ->
                    orddict:store(Field, Value, Acc);
                _ ->
                    Acc
            end
        end, SortedProfile, UpdateFields
    ).



set_field(Field, Value, Login) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            {error, not_found};
        {ok, {Profile}} ->
            NewProfile = [ {Field, Value} | proplists:delete(Field, Profile) ],
            case BackendModule:create_profile( {NewProfile}, BackendState ) of
                {ok, {Doc}} ->
                    {ok, Doc};
                {error, _} = Error ->
                    Error
            end
    end.

increase_field(Field, Login) ->
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            {error, not_found};
        {ok, {Profile}} ->
            OldFieldValue = proplists:get_value(Field, Profile),
            NewProfile = [ {Field, OldFieldValue+1} | proplists:delete(Field, Profile) ],
            case BackendModule:create_profile( {NewProfile}, BackendState ) of
                {ok, {Doc}} ->
                    {ok, Doc};
                {error, _} = Error ->
                    Error
            end
    end.

get_backend() ->
    [{backend, Result}] = ets:lookup(?MODULE, backend),
    Result.


password_correct(Password, Profile) ->
    Password == proplists:get_value(<<"password">>, Profile).

init_profile(Login, Password) ->
    {[
        {<<"_id">>, Login},
        {<<"password">>, Password},
        {<<"rank">>, 0},
        {<<"experience">>, 0},
        {<<"achievements">>, [0, 0, 0, 0, 0, 0, 0, 0]},
        {<<"reserved1">>, 0},
        {<<"reserved2">>, 0},
        {<<"reserved3">>, 0},
        {<<"reserved4">>, 0},
        {<<"reserved5">>, 0},
        {<<"reserved6">>, 0},
        {<<"reserved7">>, 0},
        {<<"score">>, 0}
    ]}.
