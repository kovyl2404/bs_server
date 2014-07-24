
-module(metrics_collector_worker).
-author("Viacheslav V. Kovalev").

-behaviour(gen_server).

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
        metrics_file,
        update_interval
    }
).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({MetricsFile, UpdateIntervalSec}) ->
    Interval = trunc(UpdateIntervalSec*1000),
    erlang:send_after(Interval, self(), flush_metrics),
    {ok, #state{
        metrics_file = MetricsFile,
        update_interval = Interval
    }}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(
    flush_metrics,
    #state{
        metrics_file = MetricsFile,
        update_interval = Interval
    } = State
) ->
    erlang:send_after(Interval, self(), flush_metrics),
    dump_metrics(MetricsFile),
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


dump_metrics(_MetricsFile) ->
    ok.