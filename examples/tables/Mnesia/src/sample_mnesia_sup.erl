-module(sample_mnesia_sup).
-behaviour(director).

%% API
-export([start_link/0
        ,start_service/4
        ,stop_service/1
        ,get_pid/1
        ,get_pids/0]).

%% director callbacks
-export([init/1
        ,terminate/2]).
-export([plan/4]).

-include("sample_mnesia_table.hrl").
-define(PROC, ?MODULE).


start_link() ->
    director:start_link({local, ?PROC}, ?MODULE, undefined).

start_service(Name, Job, Interval, Count) ->
    director:start_child(?PROC
                        ,#{id => Name
                          ,start => {sample_mnesia_worker, start_link, [Job, Interval, Count]}
                          ,plan => fun ?MODULE:plan/4
                          ,delete_before_terminate => false}).


stop_service(Name) ->
    director:terminate_and_delete_child(?PROC, Name).


get_pid(Name) ->
    director:get_pid(?PROC, Name).


get_pids() ->
    director:get_pids(?PROC).

%% -------------------------------------------------------------------------------------------------
%% director callbacks

init(undefined) ->
    Opts = [{delete_table_before_terminate, false}
           ,{table_module, director_table_mnesia}
           ,{table_init_argument, ?TAB}],
    {ok, undefined, [], Opts}.


terminate(_Rsn, _State) ->
    ok.


plan(_, done, _, St) ->
    {delete, St};
plan(_, _, 100, St) ->
    {stop, St};
plan(_, _, _, St) ->
    {restart, St}.