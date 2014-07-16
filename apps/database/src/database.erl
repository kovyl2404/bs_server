-module(database).

-behaviour(application).

-export([
    lookup_profile/1,
    create_profile/3
]).


%% Application callbacks
-export([start/2, stop/1]).

-author("Viacheslav V. Kovalev").

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

create_profile(UserId, Password, ProfileFields) when is_binary(UserId), is_binary(Password) ->
    case validate_fields(ProfileFields) of
        {ok, ValidFields} ->
            do_create_profile(UserId, Password, ValidFields);
        Error -> Error
    end.

do_create_profile(UserId, Password, ValidatedFields) ->
    {BackendModule, BackendState} = get_backend(),
    DateCreated = database_utils:now_timestamp(),
    Digest = database_utils:password_digest(Password, DateCreated),
    BackendModule:store(
        UserId, [
            {password_digest, Digest},
            {date_created, DateCreated}
            | ValidatedFields
        ], BackendState
    ).


lookup_profile(UserId) when is_binary(UserId) ->
    {BackendModule, BackendState} = get_backend(),
    BackendModule:lookup_by_id(UserId, BackendState).



get_backend() ->
    [{backend, Result}] = ets:lookup(?MODULE, backend),
    Result.

validate_fields(Fields) ->
    do_validate(Fields, []).

do_validate([], Acc) ->
    {ok, Acc};
do_validate([ {Key, _} = Field | Rest ], Acc) when is_binary(Key) ->
    do_validate(Rest, [ Field | Acc]);
do_validate([BadField | _], _) ->
    {error, {bad_field, BadField}}.