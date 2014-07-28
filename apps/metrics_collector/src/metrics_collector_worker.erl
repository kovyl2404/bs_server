
-module(metrics_collector_worker).
-author("Viacheslav V. Kovalev").

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-compile([
    {parse_transform, lager_transform}
]).

%% API
-export([start_link/0]).

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
        metrics_path,
        update_interval,
        metrics_tags
    }
).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    {ok, MetricsPath} = application:get_env(metrics_collector, metrics_path),
    {ok, MetricsInterval} = application:get_env(metrics_collector, metrics_update_interval),
    {ok, MetricsTags} = application:get_env(metrics_collector, metrics_tags),
    gen_server:start_link(
        {local, ?SERVER}, ?MODULE,
        {MetricsPath, MetricsInterval, MetricsTags},
        []
    ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MetricsPath, UpdateIntervalSec, MetricsTags}) ->
    Interval = trunc(UpdateIntervalSec*1000),
    erlang:send_after(Interval, self(), flush_metrics),
    case filelib:ensure_dir(MetricsPath) of
        ok -> ok;
        {error, _} = Error ->
            FullDirName = filename:absname(MetricsPath),
            lager:error("Failed to create metrics dir ~p with reason ~p",[FullDirName, Error])
    end,
    {ok, #state{
        metrics_path = MetricsPath,
        update_interval = Interval,
        metrics_tags = MetricsTags
    }}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(
    flush_metrics,
    #state{
        metrics_path = MetricsPath,
        update_interval = Interval,
        metrics_tags = MetricsTags
    } = State
) ->
    lists:foreach(
        fun({Tag, FileName}) ->
            Dumpfile = filename:join(MetricsPath, FileName),
            MetricsValues = folsom_metrics:get_metrics_value(Tag),
            case dump_to_file(MetricsValues, Dumpfile) of
                ok ->
                    ok;
                Reason ->
                    lager:error("Failed to dump metrics to file ~p with reason ~p",[Dumpfile, Reason])
            end
        end, MetricsTags
    ),
    erlang:send_after(Interval, self(), flush_metrics),
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


dump_to_file(Values, Dumpfile) ->
    SortedValues = lists:sort(Values),
    file:write_file(Dumpfile, format_values(SortedValues, [])).

format_values([], Acc) ->
    lists:reverse(Acc);
format_values([{MetricName, Values} | Rest], Acc) when is_list(Values) ->
    Count = proplists:get_value(count, Values),
    format_values(
        Rest,
        [ io_lib:format("~s.count = ~p~n", [MetricName, Count]) | Acc]
    );
format_values([{MetricName, Value} | Rest], Acc) when is_number(Value) ->
    format_values(
        Rest,
        [ io_lib:format("~s.count = ~p~n", [MetricName, Value]) | Acc]
    ).
