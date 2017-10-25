-module(sample_ets).
-behaviour(application).

%% API
-export([start/0
        ,stop/0
        ,start_worker/2
        ,stop_worker/1
        ,get_pid/0
        ,get_pid/1
        ,get_pids/0]).

%% Application callbacks
-export([start/2
        ,stop/1]).

-include("sample_ets_table.hrl").

%% -------------------------------------------------------------------------------------------------
%% API

start() ->
    application:start(?MODULE).


stop() ->
    application:stop(?MODULE).


start_worker(Id, WorkerCallbackMod) ->
    sample_ets_sup:start_worker(Id, WorkerCallbackMod).


stop_worker(Id) ->
    sample_ets_sup:stop_worker(Id).


get_pid() ->
    {ok, Pids} = sample_ets_sup:get_pids(),
    case length(Pids) of
        0 ->
            not_found;
        Len ->
            element(2, lists:nth(rand:uniform(Len), Pids))
    end.


get_pid(Id) ->
    case sample_ets_sup:get_pid(Id) of
        {ok, Pid} ->
            Pid;
        {error, _} ->
            not_found
    end.


get_pids() ->
    {ok, Pids} = sample_ets_sup:get_pids(),
    Pids.

%% -------------------------------------------------------------------------------------------------
%% Application callbacks

start(_StartType, _StartArgs) ->
    ets:new(?TAB, [named_table, {keypos, 2}, public]),
    sample_ets_sup:start_link().


stop(_State) ->
    ok.