-module(sample_mnesia_worker).

%%-behaviour(gen_server).

-export([start_link/3]).
-export([init/1
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2]).

-define(S, state).
-record(?S, {job, interval, count, count2}).



start_link(Job, Interval, Count) when is_function(Job, 0) andalso
                                      is_integer(Interval) andalso
                                      (is_integer(Count) orelse Count =:= infinity) ->
    gen_server:start_link(?MODULE, {Job, Interval, Count}, []).



init({Job, Interval, Count}) ->
    start(),
    {ok, #?S{job = Job, interval = Interval, count = Count, count2 = 0}}.


handle_cast(start, #?S{count2 = X, count = X}=St) ->
    {stop, done, St};
handle_cast(start, #?S{job = Job, interval = Interval, count2 = Count2}=St) ->
    Job(),
    {noreply, St#?S{count2 = Count2+1}, Interval}.


handle_info(timeout, St) ->
    start(),
    {noreply, St}.


terminate(_Rsn, _St) ->
    ok.


start() ->
    gen_server:cast(self(), start).