
-module(couchdb_backend).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").
-include_lib("database/include/logging.hrl").


%% API
-export([
    reinitialize/1,
    init/1,
    create/1,
    get_by_id/2,
    create_profile/2,
    get_top/2
]).

%% Helper function

create(Params) ->
    ok = hackney:start(),
    ok = couchbeam:start(),
    Host = get_host(Params),
    DbName = get_db_name(Params),
    Server = couchbeam:server_connection(Host),
    couchbeam:open_or_create_db(Server, DbName).

reinitialize(Params) ->
    Host = get_host(Params),
    DbName = get_db_name(Params),
    Server = couchbeam:server_connection(Host),
    {ok, _} = couchbeam:delete_db(Server, DbName),
    {ok, _} = init(Params),
    ok.

%% API functions

init(Params) ->
    Host = get_host(Params),
    DbName = get_db_name(Params),
    Server = couchbeam:server_connection(Host),
    case couchbeam:open_or_create_db(Server, DbName) of
        {ok, Db} ->
            ensure_views(Db),
            {ok, Db};
        Error ->
            ?CRITICAL("Could not operate with couchdb-server ~p, database ~p because of ~p",[Server, DbName, Error]),
            couchbeam:open_db(Server, DbName)
    end.


get_top(Count, Database) ->
    TopOptions = [
        descending, {limit, Count}
    ],
    {ok, JsonObjects} = couchbeam_view:fetch(Database, {<<"users">>, <<"top">>}, TopOptions),
    Result =
        lists:map(
            fun({Json}) ->
                Score = proplists:get_value(<<"key">>, Json),
                Login = proplists:get_value(<<"id">>, Json),
                {Login, Score}
            end, JsonObjects
        ),
    {ok, Result}.


get_by_id(Login, Database) ->
    Result = couchbeam:open_doc(Database, Login),
    Result.

create_profile(Profile, Database) ->
    couchbeam:save_doc(Database, Profile).

get_host(Params) ->
    proplists:get_value(host, Params, "http://localhost:5984").

get_db_name(Params) ->
    proplists:get_value(dbname, Params, "users").

ensure_views(Db) ->
    PrivDir = code:priv_dir(database),
    {ok, MapFun} = file:read_file(filename:join([PrivDir, "top.js"])),
    DesignDoc = {[
        {<<"_id">>, <<"_design/users">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, [
            {<<"top">>, [
                {<<"map">>, MapFun}
            ]}
        ]}
    ]},
    case couchbeam:doc_exists(Db, <<"_design/users">>) of
        true ->
            ok;
        false ->
            ?WARNING("Design document 'uses' not found in database, creating new one from ~p",[code:priv_dir(database)]),
            case couchbeam:save_doc(Db, DesignDoc) of
                {ok, _} ->
                    ?WARNING("Design document 'uses' successfully created",[]),
                    ok;
                Error ->
                    ?CRITICAL("Failed to create design document 'uses' because of ~p",[Error])
            end
    end.