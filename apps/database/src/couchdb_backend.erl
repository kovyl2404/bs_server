
-module(couchdb_backend).
-author("Viacheslav V. Kovalev").


%% API
-export([
    init/1,
    store/3,
    lookup_by_id/2
]).

init(Params) ->
    Host = get_host(Params),
    DbName = get_db_name(Params),
    Server = couchbeam:server_connection(Host),
    couchbeam:open_or_create_db(Server, DbName).



store(Id, Fields, Database) ->
    {ok, _} =
        couchbeam:save_doc(
            Database, {[
                {<<"_id">>, Id}
                | Fields
            ]}
        ),
    ok.

lookup_by_id(Id, Database) ->
    case couchbeam:open_doc(Database, Id) of
        {ok, {Doc}} ->
            {ok, fixup_id(Doc)};
        {error, _} ->
            {error, not_found}
    end.


get_host(Params) ->
    case proplists:get_value(host, Params) of
        undefined ->
            erlang:error(required, {?MODULE, host});
        Value ->
            Value
    end.

get_db_name(Params) ->
    case proplists:get_value(dbname, Params) of
        undefined ->
            erlang:error(required, {?MODULE, dbname});
        Value ->
            Value
    end.

fixup_id(Fields) ->
    fixup_id(Fields, []).

fixup_id([], Acc) ->
    Acc;
fixup_id([ {<<"_id">>, Val} | Rest ], Acc) ->
    Rest ++ [ {<<"id">>, Val} | Acc ].