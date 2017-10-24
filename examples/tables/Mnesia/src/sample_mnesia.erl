-module(sample_mnesia).
-behaviour(application).

%% API
-export([start/0
        ,stop/0
        ,start_service/4
        ,stop_service/1
        ,services/0
        ,service_pid/1]).

%% Application callbacks
-export([start/2
        ,stop/1]).

-include("sample_mnesia_table.hrl").

%% -------------------------------------------------------------------------------------------------
%% API

start() ->
    application:start(?MODULE).


stop() ->
    application:stop(?MODULE).


start_service(Name, Job, Interval, Count) ->
    sample_mnesia_sup:start_service(Name, Job, Interval, Count).


services() ->
    sample_mnesia_sup:get_pids().


service_pid(Name) ->
    case sample_mnesia_sup:get_pid(Name) of
        {ok, Pid} ->
            Pid;
        {error, Rsn} ->
            Rsn
    end.


stop_service(Name) ->
    sample_mnesia_sup:stop_service(Name).

%% -------------------------------------------------------------------------------------------------
%% application callbacks

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(mnesia),
    _ = mnesia:create_table(?TAB, director_table_mnesia:options()),
    sample_mnesia_sup:start_link().


stop(_State) ->
    ok.