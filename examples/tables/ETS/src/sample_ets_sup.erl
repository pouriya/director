-module(sample_ets_sup).
-behaviour(director).

%% API
-export([start_link/0
        ,start_worker/2
        ,stop_worker/1
        ,get_pid/1
        ,get_pids/0]).

%% Director callbacks
-export([init/1
        ,terminate/2]).
-export([plan/4
        ,child_log_validator/4
        ,sup_log_validator/4]).

-record(sup_state, {}).
-record(child_state, {crash_timestamp = 0}).
-define(PROC, ?MODULE).

-include("sample_ets_table.hrl").

%% -------------------------------------------------------------------------------------------------
%% API

start_link() ->
    director:start_link({local, ?PROC}, ?MODULE, []).


start_worker(Id, Mod) ->
    director:start_child(?PROC, #{id => Id, append => true, start => {Mod, start_link, []}}).


stop_worker(Id) ->
    director:terminate_and_delete_child(?PROC, Id).


get_pids() ->
    director_table_ets:get_pids(?TAB).


get_pid(Id) ->
    director_table_ets:get_pid(?TAB, Id).

%% -------------------------------------------------------------------------------------------------
%% Director callbacks

init([]) ->
    State = #sup_state{},
    Children = [],
    DefChild = #{plan => fun ?MODULE:plan/4
                ,log_validator => fun ?MODULE:child_log_validator/4
                ,state => #child_state{}},
    Opts = [{log_validator, fun ?MODULE:sup_log_validator/4}
           ,{table_module, director_table_ets}
           ,{table_init_argument, ?TAB}
           ,{delete_table_before_terminate, false}],
    {ok, State, Children, DefChild, Opts}.


terminate(_Rsn, #sup_state{}) ->
    ok.


plan(_Id, normal, _RestartCount, State) ->
    {delete, State};
plan(_Id, _Rsn, 100, State) ->
    {stop, State};
plan(_Id, _Rsn, _RestartCount, #child_state{crash_timestamp = TS}=State) ->
    TS2 = timestamp2(),
    State2 = State#child_state{crash_timestamp = TS2},
    Action =
        case TS2 - TS of
            Diff when Diff < 5000 ->
                restart;
            _ ->
                {restart, 3000}
        end,
    {Action, State2}.


child_log_validator(Id, info, start, _State) ->
    error_logger:info_msg("Worker ~p started~n", [Id]),
    none;
child_log_validator(Id, error, normal, _State) ->
    error_logger:info_msg("Worker ~p stopped after doing job~n", [Id]),
    none;
child_log_validator(Id, error, Rsn, _State) ->
    error_logger:error_msg("Worker ~p crashed with reason  ~p~n", [Id, Rsn]),
    none.


sup_log_validator(_Name, warning, _Extra, _State) ->
    none;
sup_log_validator(Name, error, Rsn, _State) ->
    error_logger:error_msg("Sup ~p crashed with reason  ~p~n", [Name, Rsn]),
    none.

%% -------------------------------------------------------------------------------------------------
%% Internal functions

timestamp2() ->
    {Mega, S, _Micro} = os:timestamp(),
    ((Mega*1000000)+S)*1000.