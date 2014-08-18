-module(database).

-behaviour(application).

-export([
    register/3,
    login/2,
    get_by_id/1,
    set_field/3,
    get_top/1,
    update_profile/2,
    reinitialize/0,
    change_password/2
]).

-compile([
	{parse_transform, lager_transform}
]).

-define(DATABASE_METRICS, ?MODULE).

-define(GET_TOP_METRIC, "database.get_top").

-define(REGISTER_METRIC, "database.register.total").
-define(FAIL_REGISTER_METRIC, "database.register.failed").
-define(BACKEND_ERROR_REGISTER_METRIC, "database.register.backed_error").

-define(LOGIN_METRIC, "database.login.total").
-define(FAIL_LOGIN_METRIC, "database.login.failed").
-define(BACKEND_ERROR_LOGIN_METRIC, "database.login.backed_error").

-define(UPDATE_METRIC, "database.update_profile.total").
-define(FAIL_UPDATE_METRIC, "database.update_profile.failed").
-define(BACKEND_ERROR_UPDATE_METRIC, "database.update_profile.backend_error").

-export([bin_to_hex/1]).

-export([
    stop_deps/0,
    start_deps/0,
    start/0,
    stop/0
]).


-compile({inline, [hex/1]}).

%% Application callbacks
-export([start/2, stop/1]).


-include_lib("eunit/include/eunit.hrl").
-include_lib("database/include/logging.hrl").
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
    ok = init_metrics(),
    case BackendModule:init(BackendConfig) of
        {ok, BackendInst} ->
            ets:new(?MODULE, [public, named_table]),
            ets:insert(?MODULE, {backend, {BackendModule, BackendInst}}),
            database_sup:start_link();
        Error ->
            Error
    end.


stop(_State) ->
    ok.

%% ===================================================================
%% API functions
%% ===================================================================


get_top(Count) ->
    ?DEBUG("Requesting top ~p from database",[Count]),
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

register(Login, Password, Email) ->
    ?DEBUG("Registering ~p (~p) in database",[Login, Email]),
    {BackendModule, BackendState} = get_backend(),
    folsom_metrics:notify({?REGISTER_METRIC, 1}),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            create_profile(Login, Password, Email);
        {ok, _} ->
            folsom_metrics:notify({?FAIL_REGISTER_METRIC, 1}),
            {error, already_registered};
        Error ->
            folsom_metrics:notify({?BACKEND_ERROR_REGISTER_METRIC, 1}),
            Error
    end.



create_profile(Login, Password, Email) ->
    ?NOTICE("Registering ~p in database",[Login]),
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:create_profile( init_profile(Login, Password, Email), BackendState) of
        {ok, {Doc}} ->
            {ok, Doc};
        {error, _} = Error ->
            Error
    end.

login(Login, Password) ->
    ?DEBUG("Trying to authenticate ~p in database",[Login]),
    {BackendModule, BackendState} = get_backend(),
    folsom_metrics:notify({?LOGIN_METRIC, 1}),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            folsom_metrics:notify({?FAIL_LOGIN_METRIC, 1}),
            {error, not_found};
        {ok, {Profile}} ->
            case password_correct(Password, Profile) of
                true ->
                    {ok, Profile};
                false ->
                    folsom_metrics:notify({?FAIL_LOGIN_METRIC, 1}),
                    {error, incorrect_password}
            end;
        Error ->
            folsom_metrics:notify({?BACKEND_ERROR_LOGIN_METRIC, 1}),
            Error
    end.

update_profile(NewProfileVersion, Login) ->
    {BackendModule, BackendState} = get_backend(),
    ?DEBUG("Updating ~p profile in database with data ~p",[Login, NewProfileVersion]),
    folsom_metrics:notify({?UPDATE_METRIC, 1}),
    case BackendModule:get_by_id(Login, BackendState) of
        {error, not_found} ->
            folsom_metrics:notify({?FAIL_UPDATE_METRIC,  1}),
            {error, not_found};
        {ok, {Profile}} ->
            UpdatedProfile = update_fields(Profile, NewProfileVersion),
            case BackendModule:create_profile( {UpdatedProfile}, BackendState ) of
                {ok, {Doc}} ->
                    {ok, Doc};
                {error, _} = Error ->
                    folsom_metrics:notify({?BACKEND_ERROR_UPDATE_METRIC, 1}),
                    Error
            end;
        Error ->
            folsom_metrics:notify({?BACKEND_ERROR_UPDATE_METRIC, 1}),
            Error
    end.


change_password(Profile, NewPassword) ->
    NewProfile = [
        {<<"password">>, bin_to_hex(crypto:hash(md5, NewPassword))}
        | proplists:delete(<<"password">>, Profile)
    ],
    {BackendModule, BackendState} = get_backend(),
    case BackendModule:create_profile( {NewProfile}, BackendState ) of
        {ok, {Doc}} ->
            {ok, Doc};
        {error, _} = Error ->
            Error
    end.

update_fields(Profile, NewProfileVersion) ->
    SortedProfile = orddict:from_list(Profile),
    UpdateFields = [
        <<"rank">>, <<"experience">>, <<"achievements">>,
        <<"reserved1">>, <<"reserved2">>, <<"reserved3">>,
        <<"reserved4">>, <<"reserved5">>, <<"reserved6">>,
        <<"reserved7">>, <<"score">>, <<"email">>
    ],
    Tmp =
        lists:foldl(
            fun(Field, Acc) ->
                case lists:keyfind(Field, 1, NewProfileVersion) of
                    {_, Value} ->
                        orddict:store(Field, Value, Acc);
                    _ ->
                        Acc
                end
            end, SortedProfile, UpdateFields
        ),
    orddict:store(<<"timestamp">>, unix_now_utc(), Tmp).


get_backend() ->
    [{backend, Result}] = ets:lookup(?MODULE, backend),
    Result.


password_correct(Password, Profile) ->
    bin_to_hex(crypto:hash(md5, Password)) == proplists:get_value(<<"password">>, Profile).

init_profile(Login, Password, Email) ->
    {[
        {<<"_id">>, Login},
        {<<"email">>, Email},
        {<<"password">>, bin_to_hex(crypto:hash(md5, Password))},
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
        {<<"score">>, 0},
        {<<"timestamp">>, unix_now_utc()}
    ]}.


init_metrics() ->
    ok = folsom_metrics:new_meter(?GET_TOP_METRIC),
    ok = folsom_metrics:new_meter(?REGISTER_METRIC),
    ok = folsom_metrics:new_meter(?FAIL_REGISTER_METRIC),
    ok = folsom_metrics:new_meter(?BACKEND_ERROR_REGISTER_METRIC),

    ok = folsom_metrics:new_meter(?LOGIN_METRIC),
    ok = folsom_metrics:new_meter(?FAIL_LOGIN_METRIC),
    ok = folsom_metrics:new_meter(?BACKEND_ERROR_LOGIN_METRIC),

    ok = folsom_metrics:new_meter(?UPDATE_METRIC),
    ok = folsom_metrics:new_meter(?FAIL_UPDATE_METRIC),
    ok = folsom_metrics:new_meter(?BACKEND_ERROR_UPDATE_METRIC),

    ok = folsom_metrics:tag_metric(?GET_TOP_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?REGISTER_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?FAIL_REGISTER_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?BACKEND_ERROR_REGISTER_METRIC, ?DATABASE_METRICS),

    ok = folsom_metrics:tag_metric(?LOGIN_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?FAIL_LOGIN_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?BACKEND_ERROR_LOGIN_METRIC, ?DATABASE_METRICS),

    ok = folsom_metrics:tag_metric(?UPDATE_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?FAIL_UPDATE_METRIC, ?DATABASE_METRICS),
    ok = folsom_metrics:tag_metric(?BACKEND_ERROR_UPDATE_METRIC, ?DATABASE_METRICS).



bin_to_hex(B) when is_binary(B) ->
    bin_to_hex(B, <<>>).

-define(H(X), (hex(X)):16).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(Bin, Acc) when byte_size(Bin) band 7 =:= 0 ->
    bin_to_hex_(Bin, Acc);
bin_to_hex(<<X:8, Rest/binary>>, Acc) ->
    bin_to_hex(Rest, <<Acc/binary, ?H(X)>>).

bin_to_hex_(<<>>, Acc) -> Acc;
bin_to_hex_(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acc) ->
    bin_to_hex_(
        Rest,
        <<Acc/binary,
        ?H(A), ?H(B), ?H(C), ?H(D), ?H(E), ?H(F), ?H(G), ?H(H)>>).

hex(X) ->
    element(
        X+1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
            16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044,
            16#3045, 16#3046, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
            16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3141, 16#3142,
            16#3143, 16#3144, 16#3145, 16#3146, 16#3230, 16#3231, 16#3232,
            16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
            16#3241, 16#3242, 16#3243, 16#3244, 16#3245, 16#3246, 16#3330,
            16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
            16#3338, 16#3339, 16#3341, 16#3342, 16#3343, 16#3344, 16#3345,
            16#3346, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
            16#3436, 16#3437, 16#3438, 16#3439, 16#3441, 16#3442, 16#3443,
            16#3444, 16#3445, 16#3446, 16#3530, 16#3531, 16#3532, 16#3533,
            16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3541,
            16#3542, 16#3543, 16#3544, 16#3545, 16#3546, 16#3630, 16#3631,
            16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
            16#3639, 16#3641, 16#3642, 16#3643, 16#3644, 16#3645, 16#3646,
            16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
            16#3737, 16#3738, 16#3739, 16#3741, 16#3742, 16#3743, 16#3744,
            16#3745, 16#3746, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
            16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3841, 16#3842,
            16#3843, 16#3844, 16#3845, 16#3846, 16#3930, 16#3931, 16#3932,
            16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
            16#3941, 16#3942, 16#3943, 16#3944, 16#3945, 16#3946, 16#4130,
            16#4131, 16#4132, 16#4133, 16#4134, 16#4135, 16#4136, 16#4137,
            16#4138, 16#4139, 16#4141, 16#4142, 16#4143, 16#4144, 16#4145,
            16#4146, 16#4230, 16#4231, 16#4232, 16#4233, 16#4234, 16#4235,
            16#4236, 16#4237, 16#4238, 16#4239, 16#4241, 16#4242, 16#4243,
            16#4244, 16#4245, 16#4246, 16#4330, 16#4331, 16#4332, 16#4333,
            16#4334, 16#4335, 16#4336, 16#4337, 16#4338, 16#4339, 16#4341,
            16#4342, 16#4343, 16#4344, 16#4345, 16#4346, 16#4430, 16#4431,
            16#4432, 16#4433, 16#4434, 16#4435, 16#4436, 16#4437, 16#4438,
            16#4439, 16#4441, 16#4442, 16#4443, 16#4444, 16#4445, 16#4446,
            16#4530, 16#4531, 16#4532, 16#4533, 16#4534, 16#4535, 16#4536,
            16#4537, 16#4538, 16#4539, 16#4541, 16#4542, 16#4543, 16#4544,
            16#4545, 16#4546, 16#4630, 16#4631, 16#4632, 16#4633, 16#4634,
            16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642,
            16#4643, 16#4644, 16#4645, 16#4646}).



-define(EPOCH, 62167219200).

unix_now_utc() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?EPOCH.


%% Only for TEST purposes
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
            end;
        Error ->
            Error
    end.