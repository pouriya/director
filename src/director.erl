%%% ------------------------------------------------------------------------------------------------
%%% Director is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.9.16
%% @doc
%%           Director is a production-ready supervisor and manager for Erlang/Elixir processes with
%%           focus on speed and flexibility.
%% @end
%% -------------------------------------------------------------------------------------------------


-module(director).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% supervisor-like API:
-export([start_link/2
        ,start_link/3
        ,start_child/2
        ,restart_child/2
        ,terminate_child/2
        ,delete_child/2
        ,count_children/1
        ,which_children/1
        ,get_childspec/2
        ,check_childspec/1]).

%% Director Specific API:
-export([get_pid/2
        ,get_pids/1
        ,get_plan/2
        ,get_restart_count/2
        ,change_plan/3
        ,get_default_childspec/1
        ,change_default_childspec/2
        ,change_log_validator/2
        ,change_log_validator/3
        ,start_link/4
        ,start/2
        ,start/3
        ,start/4
        ,stop/1
        ,stop/2
        ,stop/3
        ,plan/4
        ,log_validator/2
        ,terminate_and_delete_child/2
        ,default_childspec/0
        ,default_plan/0]).

%% Previous APIs with timeout argument:
-export([start_child/3
        ,restart_child/3
        ,terminate_child/3
        ,delete_child/3
        ,count_children/2
        ,which_children/2
        ,get_childspec/3
        ,get_plan/3
        ,get_restart_count/3
        ,change_plan/4
        ,get_pid/3
        ,get_pids/2
        ,get_default_childspec/2
        ,change_default_childspec/3
        ,terminate_and_delete_child/3]).

%% gen callback:
-export([init_it/6]).

%% sys callbacks:
-export([system_code_change/4
        ,system_continue/3
        ,system_get_state/1
        ,system_replace_state/2
        ,system_terminate/4
        ,format_status/2]).

%% -------------------------------------------------------------------------------------------------
%% Types:

%% 'id' is mandatory.
%% If you don't set 'start' for default childspec, 'start' is mandatory.
%% Default values for other keys:
%%  #{plan => fun director:plan/4
%%   ,terminate_timeout => infinity % For supervisors
%%                         1000     % For workers
%%   ,type => worker
%%   ,modules => [Module] % Will get from value of 'start' key
%%   ,append => false
%%   ,log_validator => fun director:log_validator/2
%%   ,pass_if_started => false}
-type childspec() :: #{'id' => id()
                      ,'start' => start()
                      ,'plan' => plan()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()
                      ,'log_validator' => log_validator()
                      ,'pass_if_started' => pass_if_started()}.
-type  id() :: term().
-type  start() :: module() % default is {module(), start_link, []}
                | {module(), function()} % default Args is []
                | mfa().
-type  plan() :: fun((Id::term(), Reason::term(), RestartCount::pos_integer(), State::any()) ->
                     'restart'                  |
                     {'restart', pos_integer()} |
                     'wait'                     |
                     'stop'                     |
                     {'stop', Reason::term()})  .
-type  terminate_timeout() :: 'infinity' | non_neg_integer().
-type  type() :: 'worker' | 'supervisor'.
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().
-type  log_validator() :: fun((Type:: log_type(), Extra::term()) -> log_mode()).
-type   log_type() :: 'info' | 'error' | 'warning'.
-type   log_mode() :: 'short' | 'long' | 'none'.
-type  pass_if_started() :: boolean().

%% Default is:
%% #{plan => fun director:plan/4
%%  ,terminate_timeout => 0
%%  ,modules => []}
-type default_childspec() :: #{'start' => start()
                              ,'plan' => plan()
                              ,'terminate_timeout' => terminate_timeout()
                              ,'type' => type()
                              ,'modules' => modules()
                              ,'log_validator' => log_validator()}.

-type start_return() :: {'ok', pid()} | {'ok', pid(), any()} | 'ignore' | {'error', term()}.

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type director() :: pid() | atom().

-type start_options() :: [start_option()] | [].
-type  start_option() :: debug_option()
                       | spawn_options()
                       | timeout_option()
                       | log_validator_option()
                       | table_module_option()
                       | table_init_argument_option()
                       | delete_table_before_terminate_option().
-type   debug_option() :: {'debug', ['trace'|'log'|'statistics'|'debug'] | []}.
-type   spawn_options() :: {'spawn_opt', proc_lib:spawn_option()}.
-type   timeout_option() :: {'timeout', timeout()}.
-type   log_validator_option() :: {'log_validator', log_validator()}.
-type   table_module_option() :: {'table_module', module()}.
-type   table_init_argument_option() :: {'table_module_init_argument', any()}.
-type   delete_table_before_terminate_option() :: {'delete_table_before_terminate_option'
                                                  ,boolean()}.

-export_type([childspec/0
             ,id/0
             ,start/0
             ,plan/0
             ,terminate_timeout/0
             ,type/0
             ,modules/0
             ,append/0
             ,log_validator/0
             ,log_mode/0
             ,log_type/0
             ,default_childspec/0
             ,start_return/0
             ,register_name/0
             ,director/0
             ,start_options/0
             ,start_option/0
             ,pass_if_started/0]).





%% -------------------------------------------------------------------------------------------------
%% Behaviour information:

-callback
init(InitArg) ->
    {'ok', State, Childspecs}                        |
    {'ok', State, Childspecs, DefaultChildspec}      |
    {'ok', State, Childspec, Opts}                   |
    {'ok', State, Childspec, DefaultChildspec, Opts} |
    'ignore'                                         |
    {'stop', Reason}
    when
        State :: any(),
        InitArg :: any(),
        Childspecs :: [childspec()] | [],
        DefaultChildspec :: default_childspec(),
        Opts :: start_options(),
        Reason :: term().


-callback
terminate(Reason, State) ->
    'ok' | {'error', Reason2}
when
    Reason :: any(),
    State :: any(),
    Reason2 :: any().

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/director_defaults.hrl").

-include("internal/director_child.hrl").

-define(is_director(Director), (erlang:is_pid(Director) orelse
                                erlang:is_atom(Director) orelse
                                erlang:is_tuple(Director))).
-define(is_timeout(Timeout), ((erlang:is_integer(Timeout) andalso Timeout >= 0) orelse
                               Timeout =:= infinity)).

-define(STATE, director_state).
-record(?STATE, {data
                ,name
                ,module
                ,init_argument
                ,table_module
                ,table_state
                ,default_childspec
                ,log_validator
                ,delete_table_before_terminate}).

%% -------------------------------------------------------------------------------------------------
%% supervisor-like API:

-spec
start_link(module(), InitArg::term()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Mod, InitArg) when erlang:is_atom(Mod) ->
    gen:start(?MODULE, link, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start_link(register_name()|module(), module()|term(), term()|start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Name, Mod, InitArg) when erlang:is_tuple(Name) andalso erlang:is_atom(Mod) ->
    gen:start(?MODULE, link, Name, Mod, InitArg, ?DEF_START_OPTIONS);
start_link(Mod, InitArg, Opts) when erlang:is_atom(Mod) andalso erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Mod, InitArg, Opts).


-spec
start_child(director(), childspec()) ->
    start_return().
%% @doc
%%      Starts a child using childspec.
%%      Reason of error may be for childspec not starting of process.
%% @end
start_child(Director, ChildSpec) when ?is_director(Director) andalso erlang:is_map(ChildSpec) ->
    gen_server:call(Director, {?START_CHILD_TAG, ChildSpec}).


-spec
restart_child(director(), id()) ->
    {'ok', pid()}                 |
    {'ok', pid(), Extra::term()} |
    {'error', Reason::'running'|'restarting'|'not_found'|'not_owner'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.
%%      Reason of error may be for starting process or table.
%%      If this supervisor is not owner of child and has access to children table, it cannot restart
%%      child and {error, not_owner} error will occur.
%% @end
restart_child(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?RESTART_CHILD_TAG, Id}).


-spec
terminate_child(director(), id() | pid()) ->
    'ok' | {'error', Reason :: 'not_found' | 'not_owner' | term()}.
%% @doc
%%      terminates a child using child id or child pid.
%%      If this supervisor is not owner of child and has access to children table, it cannot
%%      terminate child and {error, not_owner} error will occur.
%% @end
terminate_child(Director, Id_or_Pid) when ?is_director(Director) ->
    gen_server:call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}).


-spec
delete_child(director(), id()) ->
    'ok' | {'error', Reason :: 'not_found' | 'running' | 'not_owner' | term()}.
%% @doc
%%      Deletes a terminated or waited child using child id.
%%      If this supervisor is not owner of child and has access to children table, it cannot delete
%%      child and {error, not_owner} error will occur.
%% @end
delete_child(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?DELETE_CHILD_TAG, Id}).


-spec
count_children(director()) ->
    [{'specs', non_neg_integer()}
    |{'active', non_neg_integer()}
    |{'supervisors', non_neg_integer()}
    |{'workers', non_neg_integer()}]    |
    {'error', term()}.
%% @doc
%%      Returns count of children.
%%      Error maybe occur for reading from table.
%% @end
count_children(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?COUNT_CHILDREN_TAG).


-spec
which_children(director()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [] | {'error', term()}.
%% @doc
%%      Returns information about each children.
%%      Error maybe occur for reading from table.
%% @end
which_children(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?WHICH_CHILDREN_TAG).


-spec
get_childspec(director(), id() | pid()) ->
    {'ok', childspec()} | {'error', 'not_found' | term()}.
%% @doc
%%      Returns childspec of child id or child pid.
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Director, Id_or_Pid) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_CHILDSPEC_TAG, Id_or_Pid}).


-spec
check_childspec(childspec()) ->
    'ok' | {'error', Reason::term()}.
%% @doc
%%      checks childspec.
%% @end
check_childspec(ChildSpec) when erlang:is_map(ChildSpec) ->
    case director_utils:check_childspec(ChildSpec, ?DEF_DEF_CHILDSPEC) of
        {ok, _FixedChildSpec} ->
            ok;
        {error, _Reason}=Error ->
            Error
    end.

%% -------------------------------------------------------------------------------------------------
%% Specific API:

-spec
change_plan(director(), id(), plan()) ->
    'ok' | {'error', 'not_found' | 'not_owner' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.
%%      Be careful about using this, because in next crash of child you may see different behavior.
%%      If this supervisor is not owner of child and has access to children table, it cannot change
%%      plan and {error, not_owner} error will occur.
%% @end
change_plan(Director, Id, Plan) when ?is_director(Director) andalso erlang:is_function(Plan, 4) ->
    gen_server:call(Director, {?CHANGE_PLAN_TAG, Id, Plan}).


-spec
get_plan(director(), id()) ->
    {'ok', plan()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns plan of child id.
%%      Error maybe occur for reading from table.
%% @end
get_plan(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_PLAN_TAG, Id}).


-spec
get_restart_count(director(), id()) ->
    {'ok', non_neg_integer()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns restart count of child id.
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_RESTART_COUNT_TAG, Id}).


-spec
get_pid(director(), id()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @doc
%%      Returns pid of child id if child is running.
%%      Error maybe occur for reading from table.
%% @end
get_pid(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_PID_TAG, Id}).


-spec
get_pids(director()) ->
    {'ok', [{id(), pid()}] | []} | {'error', term()}.
%% @doc
%%      Returns list of {id, pid}s for all running children.
%%      Error maybe occur for reading from table.
%% @end
get_pids(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?GET_PIDS_TAG).


-spec
get_default_childspec(director()) ->
    default_childspec().
%% @doc
%%      Returns director's default childspec.
%% @end
get_default_childspec(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?GET_DEF_CHILDSPEC).


-spec
change_default_childspec(director(), default_childspec()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director's default childspec.
%%      Be careful about using this. This may change all children behavior with append => true in
%%      their next restart.
%% @end
change_default_childspec(Director, DefChildSpec) when ?is_director(Director) andalso 
                                                      erlang:is_map(DefChildSpec) ->
    gen_server:call(Director, {?CHANGE_DEF_CHILDSPEC, DefChildSpec}).


-spec
change_log_validator(director(), log_validator()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes log validator fun for director itself.
%% @end
change_log_validator(Director, LogValidator) when ?is_director(Director) andalso 
                                                  erlang:is_function(LogValidator, 2) ->
    gen_server:call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator}).


-spec
change_log_validator(director(), Id::term()|log_validator(), log_validator()|timeout()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes log validator fun for child id if log validator is third argument otherwise this is
%%      previous function with timeout argument.
%%      Error maybe occur for reading from or inserting to table.
%% @end
change_log_validator(Director, Id, LogValidator) when ?is_director(Director) andalso
                                                      erlang:is_function(LogValidator, 2) ->
    gen_server:call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator, Id});
change_log_validator(Director
                    ,LogValidator
                    ,Timeout) when ?is_director(Director) andalso
                                   erlang:is_function(LogValidator, 2) andalso
                                   ?is_timeout(Timeout) ->
    gen_server:call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator}, Timeout).


-spec
start_link(register_name(), module(), InitArg::term(), start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director with start options.
%% @end
start_link(Name, Mod, InitArg, Opts) when erlang:is_tuple(Name) andalso
                                          erlang:is_atom(Mod) andalso
                                          erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Name, Mod, InitArg, Opts).


-spec
start(module(), InitArg::term()) ->
    start_return().
%% @doc
%%      Starts stand-alone director.
%% @end
start(Mod, InitArg) when erlang:is_atom(Mod) ->
    gen:start(?MODULE, nolink, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start(register_name()|module(), module()|(InitArg::term()), (InitArg::term())|start_options()) ->
    start_return().
%% @doc
%%      Starts stand-alone director.
%% @end
start(Name, Mod, InitArg) when erlang:is_tuple(Name) andalso
                               erlang:is_atom(Mod) ->
    gen:start(?MODULE, nolink, Name, Mod, InitArg, ?DEF_START_OPTIONS);
start(Mod, InitArg, Opts) when erlang:is_atom(Mod) andalso
                               erlang:is_list(Opts) ->
    gen:start(?MODULE, nolink, Mod, InitArg, Opts).


-spec
start(register_name(), module(), InitArg::term(), start_options()) ->
    start_return().
%% @doc
%%      Starts stand-alone director.
%% @end
start(Name, Mod, InitArg, Opts) when erlang:is_tuple(Name) andalso
                                     erlang:is_atom(Mod) andalso
                                     erlang:is_list(Opts) ->
    gen:start(?MODULE, nolink, Name, Mod, InitArg, Opts).


-spec
stop(director()) ->
    'ok'.
%% @doc
%%      Stops director with reason termination 'normal' and 5000 milli-seconds timeout.
%% @end
stop(Director) when ?is_director(Director) ->
    proc_lib:stop(Director, normal, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term()) ->
    'ok'.
%% @doc
%%      Stops director with 5000 milli-seconds timeout.
%% @end
stop(Director, Reason) when ?is_director(Director) ->
    proc_lib:stop(Director, Reason, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term(), timeout()) ->
    'ok'.
%% @doc
%%      Stops director.
%% @end
stop(Director, Reason, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    proc_lib:stop(Director, Reason, Timeout).


-spec
default_plan() ->
    plan().
%% @doc
%%      Returns default plan fun of director.
%% @end
default_plan() ->
    fun director:plan/4.


-spec
plan(Id::term(), Reason::term(), RestartCount::non_neg_integer(), State::any()) ->
    {'restart', State} | {{'restart', 1000}, State} | {'stop', State}.
%% @doc
%%      Restarts child if child crashed with any reason for three times, restart it after 1000
%%      milli-seconds for fourth crash and stop entire supervisor if child crashed five times with
%%      crash reason of child.
%% @end
plan(_Id, _Other, 4, State) ->
    {{restart, 1000}, State};
plan(_Id, _Other, 5, State) ->
    {stop, State};
plan(_Id, _Other, _RestartCount, State) ->
    {restart, State}.


-spec
log_validator(Type::log_type(), Extra::term()) ->
    'short'.
%% @doc
%%     Tells director to call error_logger with Short description for every log.
%% @end
log_validator(_Type, _Extra) ->
    short.


-spec
terminate_and_delete_child(director(), id()|pid()) ->
    'ok' | {'error', 'not_found'|'not_owner'|term()}.
%% @doc
%%      Terminates running child and deletes it using child id or child pid.
%%      If this supervisor is not owner of child and has access to children table, it cannot delete
%%      child and error {error, not_owner} will occur.
%%      Error maybe occur for reading from or inserting to table.
%% @end
terminate_and_delete_child(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?TERMINATE_AND_DELETE_CHILD_TAG, Id}).


-spec default_childspec() ->
    default_childspec().
%% @doc
%%      Returns default childspec of director application (Not a running director process).
%% @end
default_childspec() ->
    ?DEF_DEF_CHILDSPEC.


%% -------------------------------------------------------------------------------------------------
%% previous APIs with Timeout argument:

-spec
start_child(director(), childspec(), timeout()) ->
    start_return().
%% @doc
%%      Starts child using childspec.
%%      Reason of error may be for childspec not starting process or reading from or inserting to 
%%      table.
%% @end
start_child(Director, ChildSpec, Timeout) when ?is_director(Director) andalso 
                                               erlang:is_map(ChildSpec) andalso
                                               ?is_timeout(Timeout) ->
    gen_server:call(Director, {?START_CHILD_TAG, ChildSpec}, Timeout).


-spec
restart_child(director(), id(), timeout()) ->
    {'ok', pid()}                                               |
    {'ok', pid(), Extra::term()}                                |
    {'error', Reason::'running'|'restarting'|'not_found'|'not_owner'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.
%%      Reason of error may be for starting process.
%%      If this supervisor is not owner of child and has access to children table, it cannot restart
%%      child and error {error, not_owner} will occur.
%% @end
restart_child(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?RESTART_CHILD_TAG, Id}, Timeout).


-spec
terminate_child(director(), id() | pid(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'not_owner'|term()}.
%% @doc
%%      Terminates running child using child id or child pid.
%%      If this supervisor is not owner of child and has access to children table, it cannot delete
%%      child and error {error, not_owner} will occur.
%%      Error maybe occur for reading from or inserting to table.
%% @end
terminate_child(Director, Id_or_Pid, Timeout) when ?is_director(Director) andalso 
                                                   ?is_timeout(Timeout) ->
    gen_server:call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}, Timeout).


-spec
delete_child(director(), id(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'running'|'not_owner'|term()}.
%% @doc
%%      Deletes terminated or waited child using child id.
%%      If this supervisor is not owner of child and has access to children table, it cannot delete
%%      child and error {error, not_owner} will occur.
%%      Error maybe occur for reading from or inserting to table.
%% @end
delete_child(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?DELETE_CHILD_TAG, Id}, Timeout).


-spec
count_children(director(), timeout()) ->
    [{'specs', non_neg_integer()}
    |{'active', non_neg_integer()}
    |{'supervisors', non_neg_integer()}
    |{'workers', non_neg_integer()}]   |
    {'error', term()}.
%% @doc
%%      Returns count of children.
%%      Error is for reading from table.
%% @end
count_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?COUNT_CHILDREN_TAG, Timeout).


-spec
which_children(director(), timeout()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [] | {'error', term()}.
%% @doc
%%      Returns information about each children.
%%      Error is for reading from table.
%% @end
which_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) -> 
    gen_server:call(Director, ?WHICH_CHILDREN_TAG, Timeout).


-spec
get_childspec(director(), id() | pid(), timeout()) ->
    {'ok', childspec()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns childspec of child.
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Director, Name, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_CHILDSPEC_TAG, Name}, Timeout).


-spec
get_restart_count(director(), id(), timeout()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Returns restart count of child id.
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_RESTART_COUNT_TAG, Id}, Timeout).


-spec
change_plan(director(), id(), plan(), timeout()) ->
    'ok' | {'error', 'not_found' | 'not_owner' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.
%%      Be careful about using this, because in next crash of child you may see different behavior.
%%      If this supervisor is not owner of child and has access to children table, it cannot change
%%      plan and {error, not_owner} error will occur.
%% @end
change_plan(Director, Id, Plan, Timeout) when ?is_director(Director) andalso
                                              erlang:is_function(Plan, 4) andalso
                                              ?is_timeout(Timeout) ->
    gen_server:call(Director, {?CHANGE_PLAN_TAG, Id, Plan}, Timeout).


-spec
get_plan(director(), id(), timeout()) ->
    {'ok', plan()} | {'error', 'not_found' | term()}.
%% @doc
%%      Returns plan of child id.
%%      Error maybe occur for reading from table.
%% @end
get_plan(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_PLAN_TAG, Id}, Timeout).


-spec
get_pid(director(), id(), timeout()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @doc
%%      Returns pid of a running child.
%%      Error maybe occur for reading from table.
%% @end
get_pid(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_PID_TAG, Id}, Timeout).


-spec
get_pids(director(), timeout()) ->
    {'ok', [{id(), pid()}] | []} | {'error', term()}.
%% @doc
%%      Returns list of {id, pid}s for all running children.
%%      Error maybe occur for reading from table.
%% @end
get_pids(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_PIDS_TAG, Timeout).


-spec
get_default_childspec(director(), timeout()) ->
    default_childspec().
%% @doc
%%      Returns director's default childspec.
%% @end
get_default_childspec(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_DEF_CHILDSPEC, Timeout).


-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director's default childspec.
%%      Be careful about using this. This may change all children behavior with append => true in
%%      their next restart.
%% @end
change_default_childspec(Director, ChildSpec, Timeout) when ?is_director(Director) andalso
                                                            erlang:is_map(ChildSpec) andalso
                                                            ?is_timeout(Timeout) ->
    gen_server:call(Director, {?CHANGE_DEF_CHILDSPEC, ChildSpec}, Timeout).


-spec
terminate_and_delete_child(director(), id()|pid(), timeout()) ->
    'ok' | {'error', 'not_found'|'not_owner'|term()}.
%% @doc
%%      Terminates running child and deletes it using child id or child pid.
%%      If this supervisor is not owner of child and has access to children table, it cannot delete
%%      child and error {error, not_owner} will occur.
%%      Error maybe occur for reading from or inserting to table.
%% @end
terminate_and_delete_child(Director, Id, Timeout) when ?is_director(Director) andalso
                                                       ?is_timeout(Timeout) ->
    gen_server:call(Director, {?TERMINATE_AND_DELETE_CHILD_TAG, Id}, Timeout).


%% -------------------------------------------------------------------------------------------------
%% 'gen' callback:

%% @hidden
init_it(Starter, self, Name, Mod, InitArg, Opts) ->
    init_it(Starter, erlang:self(), Name, Mod, InitArg, Opts);
init_it(Starter, Parent, Name0, Mod, InitArg, Opts) ->
    Name = name(Name0),
    erlang:process_flag(trap_exit, true),
    case init_module(Name, Mod, InitArg, Opts) of
        {ok, {Data, Children, DefChildSpec, Dbg, LogValidator, TabMod, TabInitArg, DelTab}} ->
            case director_table:create(TabMod, TabInitArg) of
                {ok, TabState} ->
                    case start_children(Name, Children, TabMod, TabState) of
                        {ok, TabState2} ->
                            State = #?STATE{data = Data
                                           ,name = Name
                                           ,module = Mod
                                           ,init_argument = InitArg
                                           ,table_state = TabState2
                                           ,default_childspec = DefChildSpec
                                           ,log_validator = LogValidator
                                           ,table_module = TabMod
                                           ,delete_table_before_terminate = DelTab},
                            proc_lib:init_ack(Starter, {ok, erlang:self()}),
%%                    exit(element(2, (catch loop(Parent, Dbg, State))));
                            loop(Parent, Dbg, State);
                        {error, Rsn0} ->
                            unregister_name(Name0),
                            Rsn =
                                case call_terminate(Mod, Data, Rsn0) of
                                    ok ->
                                        Rsn0;
                                    {error, Rsn2} ->
                                        Rsn2;
                                    {crash, Rsn2} ->
                                        [Rsn0, Rsn2]
                                end,
                            proc_lib:init_ack(Starter, {error, Rsn}),
                            erlang:exit(Rsn)
                    end;
                {error, Rsn0} ->
                    unregister_name(Name0),
                    Rsn =
                        case call_terminate(Mod, Data, Rsn0) of
                            ok ->
                                Rsn0;
                            {error, Rsn2} ->
                                Rsn2;
                            {crash, Rsn2} ->
                                [Rsn0, Rsn2]
                        end,
                    proc_lib:init_ack(Starter, {error, Rsn}),
                    erlang:exit(Rsn)
            end;
        Other ->
            Reason =
                case Other of
                    {error, Reason2} ->
                        Reason2;
                    ignore ->
                        normal
                end,
            unregister_name(Name0),
            proc_lib:init_ack(Starter, Other),
            erlang:exit(Reason)
    end.

%% -------------------------------------------------------------------------------------------------
%% 'sys' callbacks:

%% @hidden
system_continue(Parent, Dbg, State) ->
    loop(Parent, Dbg, State).


%% @hidden
system_terminate(Reason, _Parent, Dbg, State) ->
    terminate(Dbg, State, Reason).


%% @hidden
system_get_state(#?STATE{data = State}) ->
    {ok, State}.


%% @hidden
system_replace_state(ReplaceStateFun, #?STATE{data = Data}=State) ->
    NewData = ReplaceStateFun(Data),
    {ok, State#?STATE{data = NewData}, NewData}.


%% @hidden
system_code_change(#?STATE{name = Name
                          ,module = Mod
                          ,init_argument = InitArg
                          ,table_module = TabMod
                          ,table_state = TabState}=State
                  ,_Module
                  ,_OldVsn
                  ,_Extra) ->
    case init_module(Name, Mod, InitArg, []) of
        {ok, {_Data2, Children, DefChildSpec, _Dbg, _LogValidator, _TabMod, _TabInitArg, _DelTab}} ->
            case check_duplicate_ids(Children) of
                ok ->
                    case change_old_children_pids(Children, TabMod, TabState) of
                        {ok, TabState2} ->
                            {ok, State#?STATE{table_state = TabState2
                                             ,default_childspec = DefChildSpec}};
                        {error, _}=Err ->
                            Err
                    end;
                {error, _}=Err ->
                    Err
            end;
        ignore ->
            {ok, State};
        {error, _}=Err ->
            Err
    end.


%% @hidden
format_status(_, [_PDict, SysState, Parent, Debug, #?STATE{name = Name, module = Mod}=State]) ->
    Header = {header, "Status for director " ++ io_lib:print(Name)},
    Data = {data, [{"Status", SysState}
                  ,{"Parent", Parent}
                  ,{"Logged events", sys:get_debug(log, Debug, [])}]},
    State2 =
        case string:to_integer(erlang:system_info(otp_release)) of
            {Ver, []} when Ver >= 19->
                {state
                ,name
                ,strategy
                ,children
                ,dynamics
                ,intensity
                ,period
                ,restarts
                ,Mod
                ,Mod
                ,args};
            _ ->
                State
        end,
    Specific = [{data, [{"State", State2}]}
               ,{supervisor, [{"Callback", Mod}]}],
    [Header, Data, Specific].


%% -------------------------------------------------------------------------------------------------
%% Process main loop and its main subcategories:

loop(Parent, Dbg, State) ->
    process_message(Parent
                   ,Dbg
                   ,State
                   ,receive
                        Msg ->
                            Msg
                    end).


process_message(Parent
               ,Dbg
               ,#?STATE{name = Name}=State
               ,{?GEN_CALL_TAG, From, Request}=Msg) ->
    {Dbg3, State2} = process_request(director_utils:debug(Dbg, Name, Msg), State, From, Request),
    loop(Parent, Dbg3, State2);
process_message(Parent, Dbg, #?STATE{name= Name}=State, {'EXIT', Pid, Reason}=Msg) ->
    process_exit(Parent, director_utils:debug(Dbg, Name, Msg), State, Pid, Reason);
process_message(Parent, Dbg, #?STATE{name = Name}=State, {timeout, TimerRef, Id}=Msg) ->
    process_timeout(Parent, director_utils:debug(Dbg, Name, Msg), State, TimerRef, Id);
process_message(Parent, Dbg, State, {cancel_timer, _TimerRef, _Result}) ->
    loop(Parent, Dbg, State);
process_message(Parent, Dbg, State, {system, From, Msg}) ->
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, State);
%% Catch clause:
process_message(Parent, Dbg, #?STATE{name = Name, log_validator = LogValidator}=State, Msg) ->
    case director_utils:run_log_validator(LogValidator
                                         ,warning
                                         ,{receive_unexpected_message, Msg}) of
        short ->
            error_logger:error_msg("Director ~p received an unexpected message~n", [Name]);
        none ->
            ok;
        long ->
            error_logger:error_msg("Director ~p received unexpected message: ~p~n", [Name, Msg])
    end,
    loop(Parent, Dbg, State).


process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState,  name = Name}=State
               ,From
               ,{?GET_PID_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {reply(Dbg, Name, From, {ok, Pid}), State};
        {ok, #?CHILD{pid = Other}} ->
            {reply(Dbg, Name, From, {error, Other}), State};
        {error, Rsn}=Reply ->
            Dbg2 = reply(Dbg, Name, From, Reply),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?GET_PIDS_TAG) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            Reply = {ok, [{Id, Pid}
                         || #?CHILD{pid = Pid, id = Id} <- Children, erlang:is_pid(Pid)]},
            {reply(Dbg, Name, From, Reply), State};
        {error, Rsn}=Reply ->
            Dbg2 = reply(Dbg, Name, From, Reply),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?COUNT_CHILDREN_TAG) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            Fun =
                fun(#?CHILD{pid = Pid, type = Type}, {Specs, Actives, Sups, Workers}) ->
                    Actives2 =
                        if
                            erlang:is_pid(Pid) ->
                                Actives+1;
                            true ->
                                Actives
                        end,
                    {Sups2, Workers2} =
                        if
                            Type =:= supervisor ->
                                {Sups+1, Workers};
                            Type =:= worker ->
                                {Sups, Workers+1}
                        end,
                    {Specs+1, Actives2, Sups2, Workers2}
                end,
            {Specs, Actives, Sups, Workers} = lists:foldl(Fun, {0, 0, 0, 0}, Children),
            Reply = [{specs, Specs}
                    ,{active, Actives}
                    ,{supervisors, Sups}
                    ,{workers, Workers}],
            {reply(Dbg, Name, From, Reply), State};
        {error, Rsn}=Reply ->
            Dbg2 = reply(Dbg, Name, From, Reply),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?DELETE_CHILD_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {reply(Dbg, Name, From, {error, running}), State};
        {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
            {reply(Dbg, Name, From, {error, not_owner}), State};
        {ok, Child} ->
            case director_table:delete(TabMod, TabState, Child) of
                {ok, TabState2} ->
                    {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                {error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        {ok, Child} ->
            {reply(Dbg, Name, From, {ok, director_utils:c2cs(Child)}), State};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    case do_restart_child(Name, Id, TabMod, TabState) of
        {ok, Pid, TabState2} ->
            {reply(Dbg, Name, From, {ok, Pid}), State#?STATE{table_state = TabState2}};
        {ok, Pid, Extra, TabState2} ->
            {reply(Dbg, Name, From, {ok, Pid, Extra}), State#?STATE{table_state = TabState2}};
        {error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod
                       ,table_state = TabState
                       ,name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,{?START_CHILD_TAG, ChildSpec}) ->
    case director_utils:check_childspec(ChildSpec, DefChildSpec) of
        {ok, Child} ->
            case do_start_child(Name, Child, TabMod, TabState) of
                {ok, Pid, TabState2} ->
                    {reply(Dbg, Name, From, {ok, Pid}), State#?STATE{table_state = TabState2}};
                {ok, Pid, Extra, TabState2} ->
                    {reply(Dbg, Name, From, {ok, Pid, Extra})
                    ,State#?STATE{table_state = TabState2}};
                {error, TabState2, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
                {error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {error, _}=Err ->
            {reply(Dbg, Name, From, Err), State}
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?TERMINATE_CHILD_TAG, Term}) ->
    case do_terminate_child(Name, Term, TabMod, TabState) of
        {ok, TabState2} ->
            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn);
        not_found ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        not_owner ->
            {reply(Dbg, Name, From, {error, not_owner}), State}
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            Reply = [{Id, Pid, Type, Mods}
                    || #?CHILD{id = Id
                              ,pid = Pid
                              ,type = Type
                              ,modules = Mods} <- Children],
            {reply(Dbg, Name, From, Reply), State};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_PLAN_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        {ok, #?CHILD{plan = Plan}} ->
            {reply(Dbg, Name, From, {ok, Plan}), State};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?CHANGE_PLAN_TAG, Id, Plan}) ->
    case director_utils:filter_plan(Plan) of
        {ok, Plan2} ->
            case director_table:lookup_id(TabMod, TabState, Id) of
                {ok, not_found} ->
                    {reply(Dbg, Name, From, {error, not_found}), State};
                {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
                    {reply(Dbg, Name, From, {error, not_owner}), State};
                {ok, Child} ->
                    Child2 = Child#?CHILD{plan = Plan2},
                    case director_table:insert(TabMod, TabState, Child2) of
                        {ok, TabState2} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                        {error, Rsn}=Err ->
                            Dbg2 = reply(Dbg, Name, From, Err),
                            terminate(Dbg2, State, Rsn)
                    end;
                {error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {error, _}=Err ->
            {reply(Dbg, Name, From, Err), State}
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_RESTART_COUNT_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        {ok, #?CHILD{restart_count = Count}} ->
            {reply(Dbg, Name, From, {ok, Count}), State};
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{name = Name, default_childspec = DefChildSpec}=State
               ,From
               ,?GET_DEF_CHILDSPEC) ->
    {reply(Dbg, Name, From, DefChildSpec), State};
process_request(Dbg
               ,#?STATE{table_module = TabMod
                       ,table_state = TabState
                       ,name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,{?CHANGE_DEF_CHILDSPEC, ChildSpec}) ->
    case director_utils:check_default_childspec(ChildSpec) of
        {ok, DefChildSpec2} ->
            case director_table:separate_children(TabMod, TabState, DefChildSpec) of
                {ok, TabState2} ->
                    case director_table:combine_children(TabMod, TabState2, DefChildSpec2) of
                        {ok, TabState3} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState3}};
                        {error, Rsn}=Err ->
                            Dbg2 = reply(Dbg, Name, From, Err),
                            terminate(Dbg2, State, Rsn)
                    end;
                {error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {error, _}=Err ->
            {reply(Dbg, Name, From, Err), State}
    end;
%%process_request(Dbg
%%               ,#?STATE{name = Name, log_validator = LogValidator}=State
%%               ,From
%%               ,?GET_LOG_VALIDATOR) ->
%%    {reply(Dbg, Name, From, LogValidator), State};
process_request(Dbg
               ,#?STATE{name = Name}=State
               ,From
               ,{?CHANGE_LOG_VALIDATOR, LogValidator}) ->
    {State2, Result} =
        if
            erlang:is_function(LogValidator, 2) ->
                {State#?STATE{log_validator = LogValidator}, ok};
            true ->
                {State, {error, {bad_log_validator, [{log_validator, LogValidator}]}}}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?CHANGE_LOG_VALIDATOR, LogValidator, Id}) ->
    if
        erlang:is_function(LogValidator, 2) ->
            case director_table:lookup_id(TabMod, TabState, Id) of
                {ok, not_found} ->
                    {reply(Dbg, Name, From, {error, not_found}), State};
                {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
                    {reply(Dbg, Name, From, {error, not_owner}), State};
                {ok, Child} ->
                    case director_table:insert(TabMod
                                              ,TabState
                                              ,Child#?CHILD{log_validator = LogValidator}) of
                        {ok, TabState2} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                        {error, _}=Err ->
                            {reply(Dbg, Name, From, Err), State}
                    end;
                {error, _}=Err ->
                    {reply(Dbg, Name, From, Err), State}
            end;
        true ->
            {reply(Dbg, Name, From, {error, {bad_log_validator, [{log_validator, LogValidator}]}})
            ,State}
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?TERMINATE_AND_DELETE_CHILD_TAG, Term}) ->
    case do_terminate_child(Name, Term, TabMod, TabState) of
        {ok, TabState2} ->
            LookupFunc =
                if
                    erlang:is_pid(Term) ->
                        lookup_pid;
                    true ->
                        lookup_id
                end,
            case director_table:LookupFunc(TabMod, TabState2, Term) of
                {ok, not_found} ->
                    {reply(Dbg, Name, From, {error, not_found}), State};
                {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
                    {reply(Dbg, Name, From, {error, running}), State};
                {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
                    {reply(Dbg, Name, From, {error, not_owner}), State};
                {ok, Child} ->
                    case director_table:delete(TabMod, TabState2, Child) of
                        {ok, TabState3} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState3}};
                        {error, Rsn}=Err ->
                            Dbg2 = reply(Dbg, Name, From, Err),
                            terminate(Dbg2, State, Rsn)
                    end
            end;
        {error, Rsn}=Err ->
            Dbg2 = reply(Dbg, Name, From, Err),
            terminate(Dbg2, State, Rsn);
        not_found ->
            {reply(Dbg, Name, From, {error, not_found}), State};
        not_owner ->
            {reply(Dbg, Name, From, {error, not_owner}), State}
    end;
%% Catch clause:
process_request(Dbg, #?STATE{name = Name, log_validator = LogValidator}=State, From, Other) ->
    case director_utils:run_log_validator(LogValidator
                                         ,warning
                                         ,{receive_unexpected_call, From, Other}) of
        short ->
            error_logger:error_msg("Director ~p received an unexpected call request~n", [Name]);
        none ->
            ok;
        long ->
            error_logger:error_msg("Director ~p received unexpected call request ~p with from ~p~n"
                                  ,[Name, Other, From])
    end,
    {reply(Dbg, Name, From, {error, {unknown_request, Other}}), State}.


process_exit(Parent, Dbg, State, Parent, Reason) ->
    terminate(Dbg, State, Reason);
process_exit(Parent
            ,Dbg
            ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
            ,Pid
            ,Reason) ->
    case director_table:lookup_pid(TabMod, TabState, Pid) of
        {ok, not_found} ->
            loop(Parent, Dbg, State);
        {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
            loop(Parent, Dbg, State);
        {ok, Child} ->
            director_utils:error_report(Name
                                       ,child_terminated
                                       ,Reason
                                       ,Child),
            Child2 = Child#?CHILD{pid = undefined, extra = undefined},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    handle_exit(Parent, Dbg, State#?STATE{table_state = TabState2}, Child2, Reason);
                {error, Rsn} ->
                    terminate(Dbg, State, Rsn)
            end;
        {error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


handle_exit(Parent
           ,Dbg
           ,#?STATE{name = Name
                   ,table_module = TabMod
                   ,table_state = TabState
                   ,data = Data}=State
           ,#?CHILD{id = Id
                   ,plan = Plan
                   ,restart_count = ResCount}=Child
           ,Reason) ->
    ResCount2 = ResCount + 1,
    Child2 = Child#?CHILD{restart_count = ResCount2},
    case director_table:insert(TabMod, TabState, Child2) of
        {ok, TabState2} ->
            Strategy =
                try
                    {value, Plan(Id, Reason, ResCount2, Data)}
                catch
                    _:Rsn ->
                        {error, {plan_crash, [{reason, Rsn}
                                             ,{plan, Plan}
                                             ,{id, Id}
                                             ,{reason_argument, Reason}
                                             ,{restart_count, ResCount2}
                                             ,{state, Data}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                end,
            _ = director_utils:debug(Dbg, Name, {plan, Id, Strategy}),
            case Strategy of
                {value, {restart, Data2}} ->
                    case do_restart_child(Name, Id, TabMod, TabState2) of
                        {error, Rsn2} ->
                            terminate(Dbg
                                     ,State#?STATE{table_state = TabState2, data = Data2}
                                     ,Rsn2);
                        {error, TabState3, _Reason3} ->
                            TimeRef = restart_timer(0, Id),
                            case director_table:insert(TabMod
                                                      ,TabState3
                                                      ,Child2#?CHILD{pid = restarting
                                                                    ,timer_reference = TimeRef}) of
                                {ok, TabState4} ->
                                    loop(Parent
                                        ,Dbg
                                        ,State#?STATE{table_state = TabState4, data = Data2});
                                {error, Rsn3} ->
                                    terminate(Dbg
                                             ,State#?STATE{table_state = TabState3, data = Data2}
                                             ,Rsn3)
                            end;
                        {ok, _Pid, TabState3} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState3, data = Data2});
                        {ok, _Pid, _Extra, TabState3} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState3, data = Data2})
                    end;
                {value, {stop, Data2}} ->
                    terminate(Dbg, State#?STATE{table_state = TabState2, data = Data2}, Reason);
                {value, {{stop, Reason3}, Data2}} ->
                    terminate(Dbg, State#?STATE{table_state = TabState2, data = Data2}, Reason3);
                {value, {delete, Data2}} ->
                    case director_table:delete(TabMod, TabState2, Child2) of
                        {ok, TabState3} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState3, data = Data2});
                        {error, Rsn2} ->
                            terminate(Dbg
                                     ,State#?STATE{table_state = TabState2, data = Data2}
                                     ,Rsn2)
                    end;
                {value, {wait, Data2}} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState2, data = Data2});
                {value, {{restart, PosInt}, Data2}} when erlang:is_integer(PosInt) andalso PosInt >= 0 ->
                    TimeRef = restart_timer(PosInt, Id),
                    case director_table:insert(TabMod
                                              ,TabState2
                                              ,Child2#?CHILD{pid = restarting
                                                            ,timer_reference = TimeRef}) of
                        {ok, TabState3} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState3, data = Data2});
                        {error, Rsn2} ->
                            terminate(Dbg
                                     ,State#?STATE{table_state = TabState2, data = Data2}
                                     ,Rsn2)
                    end;
                {error, Reason3} ->
                    terminate(Dbg
                             ,State#?STATE{table_state = TabState2}
                             ,Reason3);
                {value, Other} ->
                    terminate(Dbg
                             ,State#?STATE{table_state = TabState2}
                             ,{plan_return, [{returned_value, Other}
                                            ,{plan, Plan}
                                            ,{id, Id}
                                            ,{reason_argument, Reason}
                                            ,{restart_count, ResCount2}]})
            end;
        {error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


process_timeout(Parent
               ,Dbg
               ,#?STATE{name = Name, table_module = TabMod, table_state = TabState}=State
               ,TimerRef
               ,Id) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {Dbg, State};
        {ok, #?CHILD{timer_reference = TimerRef}} ->
            case do_restart_child(Name, Id, TabMod, TabState) of
                {error, Rsn} ->
                    terminate(Dbg, State, Rsn);
                {error, TabState2, not_found} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState2});
                {error, TabState2, not_owner} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState2});
                {error, TabState2, Reason} ->
                    case director_table:lookup_id(TabMod, TabState2, Id) of
                        {ok, not_found} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
                        {ok, Child} ->
                            handle_exit(Parent
                                       ,Dbg
                                       ,State#?STATE{table_state = TabState2}
                                       ,Child
                                       ,Reason)
                    end;
                {ok, _Pid, TabState3} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState3});
                {ok, _Pid, _Extra, TabState3} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState3})
            end;
        _Child ->
            {Dbg, State}
    end.

%% -------------------------------------------------------------------------------------------------
%% Other internal functions:


name({local, Name}) ->
    Name;
name({global, Name}) ->
    Name;
name({via, _Mod, Name}) ->
    Name;
name(Other) ->
    Other.


unregister_name({local,Name}) ->
    try unregister(Name) of
        _Result -> ok
    catch
        _ErrorType:_Reason -> ok
    end;
unregister_name({global,Name}) ->
    catch global:unregister_name(Name),
    ok;
unregister_name({via, Mod, Name}) ->
    catch Mod:unregister_name(Name),
    ok;
unregister_name(_Other) ->
    ok.


init_module(Name, Mod, InitArg, Opts) ->
    Rslt =
        try Mod:init(InitArg) of
            {ok, Data} ->
                {ok, Data, [], ?DEF_DEF_CHILDSPEC, ?DEF_START_OPTIONS};
            {ok, Data, ChildSpecs} ->
                case director_utils:check_childspecs(ChildSpecs) of
                    {ok, ChildSpecs2} ->
                        {ok, Data, ChildSpecs2, ?DEF_DEF_CHILDSPEC, ?DEF_START_OPTIONS};
                    {error, _}=Error ->
                        Error
                end;
            {ok, Data, ChildSpecs, Opts2} when erlang:is_list(Opts2) ->
                case director_utils:check_childspecs(ChildSpecs) of
                    {ok, ChildSpecs2} ->
                        {ok, Data, ChildSpecs2, ?DEF_DEF_CHILDSPEC, Opts2};
                    {error, _}=Error ->
                        Error
                end;
            {ok, Data, ChildSpecs, DefChildSpec} ->
                case director_utils:check_default_childspec(DefChildSpec) of
                    {ok, DefChildSpec2} ->
                        case director_utils:check_childspecs(ChildSpecs) of
                            {ok, ChildSpecs2} ->
                                {ok, Data, ChildSpecs2, DefChildSpec2, ?DEF_START_OPTIONS};
                            {error, _}=Error ->
                                Error
                        end;
                    {error, _}=Err ->
                        Err
                end;
            {ok, Data, ChildSpecs, DefChildSpec, Opts2} when erlang:is_list(Opts2) ->
                case director_utils:check_default_childspec(DefChildSpec) of
                    {ok, DefChildSpec2} ->
                        case director_utils:check_childspecs(ChildSpecs) of
                            {ok, ChildSpecs2} ->
                                {ok, Data, ChildSpecs2, DefChildSpec2, Opts2};
                            {error, _}=Error ->
                                Error
                        end;
                    {error, _}=Err ->
                        Err
                end;
            {stop, Rsn} ->
                {error, Rsn};
            ignore ->
                ignore;
            Other ->
                {error, {init_bad_return, [{returned_value, Other}
                                          ,{module, Mod}
                                          ,{function, init}
                                          ,{init_argument, InitArg}]}}
        catch
            _:Rsn ->
                {error, {init_crash, [{reason, Rsn}
                                     ,{module, Mod}
                                     ,{function, init}
                                     ,{init_argument, InitArg}
                                     ,{stacktrace, erlang:get_stacktrace()}]}}
        end,
    case Rslt of
        {ok, Data2, ChildSpecs3, DefChildSpec3, Opts3} ->
            Opts4 = director_utils:concat(Opts3, Opts),
            Dbg = director_utils:get_debug_options(Name, Opts4),
            LogValidator = director_utils:get_log_validator(Name, Opts4),
            TabMod = director_utils:get_table_module(Name, Opts4),
            TabInitArg = director_utils:get_table_init_argument(Name, Opts4),
            DelTab = director_utils:get_delete_table_before_terminate(Name, Opts4),
            {ok, {Data2
                 ,ChildSpecs3
                 ,DefChildSpec3
                 ,Dbg
                 ,LogValidator
                 ,TabMod
                 ,TabInitArg
                 ,DelTab}};
        ignore ->
            ignore;
        {error, _}=Err2 ->
            Err2
    end.



start_children(Name
              ,[#?CHILD{id=Id, pass_if_started = PassIfStarted}=Child|Children]
              ,TabMod
              ,TabState) ->
    case do_start_child(Name, Child, TabMod, TabState) of
        {ok, _Pid, TabState2} ->
            start_children(Name, Children, TabMod, TabState2);
        {ok, _Pid, _Extra, TabState2} ->
            start_children(Name, Children, TabMod, TabState2);
        {error, TabState2, already_present} when PassIfStarted ->
            start_children(Name, Children, TabMod, TabState2);
        {error, TabState2, {already_started, _}} when PassIfStarted ->
            start_children(Name, Children, TabMod, TabState2);
        {error, _TabState2, already_present} ->
            {error, {duplicate_child_name, Id}}; % Like OTP/supervisor
        {error, _TabState2, {already_started, _}} ->
            {error, {duplicate_child_name, Id}}; % Like OTP/supervisor
        {error, _Reason}=Error ->
            _ = terminate_children(Name, TabMod, TabState),
            Error
    end;
start_children(_, [], _, TabState) ->
    {ok, TabState}.


do_start_child(Name, #?CHILD{id = Id}=Child ,TabMod, TabState) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            start_mfa(Name, Child, TabMod, TabState);
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {error, TabState, {already_started, Pid}};
        {ok, _} ->
            {error, TabState, already_present};
        {error, _}=Err ->
            Err
    end.


do_restart_child(Name, Id, TabMod, TabState) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, not_found} ->
            {error, TabState, not_found};
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {error, TabState, running};
        {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
            {error, TabState, not_owner};
        {ok, #?CHILD{pid = restarting, timer_reference = Ref}=Child} ->
            _ = erlang:cancel_timer(Ref, [{async, true}]),
            start_mfa(Name
                     ,Child#?CHILD{pid = undefined
                                  ,timer_reference = undefined}
                     ,TabMod
                     ,TabState);
        {ok, Child} ->
            start_mfa(Name, Child, TabMod, TabState);
        {error, Rsn} ->
            {error, Rsn}
    end.


start_mfa(Name
         ,#?CHILD{start = {Mod, Func, Args}}=Child
         ,TabMod
         ,TabState) ->
    try erlang:apply(Mod, Func, Args) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid, extra = undefined},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    director_utils:progress_report(Name, Child2),
                    {ok, Pid, TabState2};
                {error, _}=Err ->
                    Err
            end;
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid, extra = {value, Extra}},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    director_utils:progress_report(Name, Child2),
                    {ok, Pid, Extra, TabState2};
                {error, _}=Err ->
                    Err
            end;
        ignore ->
            {ok, undefined, TabState};
        {error, _}=Err ->
            Err;
        Other ->
            {error, {start_bad_return, [{returned_value, Other}
                                       ,{module, Mod}
                                       ,{function, Func}
                                       ,{arguments, Args}]}}
    catch
        _:Rsn ->
            {error, {start_crash, [{reason, Rsn}
                                  ,{module, Mod}
                                  ,{function, Func}
                                  ,{arguments, Args}
                                  ,{stacktace, erlang:get_stacktrace()}]}}
    end.


terminate(Dbg
         ,#?STATE{module = Mod
                 ,data = Data
                 ,table_module = TabMod
                 ,table_state = TabState
                 ,name = Name
                 ,log_validator = LogValidator
                 ,delete_table_before_terminate = Bool}
         ,Rsn) ->
    Rsn2 =
        case call_terminate(Mod, Data, Rsn) of
            ok ->
                [Rsn];
            {error, Rsn3} ->
                [Rsn3];
            {crash, Rsn3} ->
                [Rsn, Rsn3]
        end,
    {Rsn5, Children, TabState2} =
        case director_table:tab2list(TabMod, TabState) of
            {ok, Children2} ->
                case terminate_children(Name, TabMod, TabState, Children2) of
                    {ok, TabState3} ->
                        {Rsn2, Children2, TabState3};
                    {error, TabState3, Errs} ->
                        {Rsn2 ++ [erlang:element(2, Err) || Err <- Errs], Children2, TabState3}
                end;
            {error, Rsn4} ->
                {Rsn4, [], TabState}
        end,
    Rsn6 =
        if
            Bool ->
                case director_table:delete_table(TabMod, TabState2) of
                    ok ->
                        Rsn5;
                    {error, Rsn7} ->
                        Rsn5 ++ [Rsn7]
                end;
            true ->
                Rsn5
        end,
    Rsn8 =
        case Rsn6 of
            [_] ->
                erlang:hd(Rsn6);
            _ ->
                Rsn6
        end,
    case director_utils:run_log_validator(LogValidator, error, Rsn8) of
        none ->
            ok;
        short ->
            error_logger:error_msg("** Director ~p terminating \n"
                                   "** Reason for termination == ~p\n"
                                  ,[Name,Rsn8]);
        long ->
            ChildrenStr = string:join([io_lib:print(director_utils:c_r2p(Child, short))
                                      || Child <- Children]
                                     ,"\n"),
            error_logger:error_msg("** Director \"~p\" terminating \n"
                                   "** Reason for termination == ~p\n"
                                   "** Children == \n~s\n"
                                  ,[Name, Rsn8, ChildrenStr])
    end,
    sys:print_log(Dbg),
    erlang:exit(Rsn8).




call_terminate(Mod, Data, Rsn) ->
    try Mod:terminate(Data, Rsn) of
        {error, _}=Err ->
            Err;
        _ ->
            ok
    catch
        _:Rsn2 ->
            {crash, {terminate_crash, [{reason, Rsn2}
                                      ,{module, Mod}
                                      ,{state, Data}
                                      ,{stacktrace, erlang:get_stacktrace()}]}}
    end.



terminate_children(Name, TabMod, TabState) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            terminate_children(Name, TabMod, TabState, Children);
        {error, _}=Err ->
            Err
    end.


terminate_children(Name, TabMod, TabState, Children) ->
    Terminate =
        fun(#?CHILD{id = Id}, {TabState2, Errs}) ->
            case do_terminate_child(Name, Id, TabMod, TabState2) of
                {ok, TabState3} ->
                    {TabState3, Errs};
                {error, _}=Err ->
                    {TabState2, [Err|Errs]};
                not_found ->
                    {TabState2, Errs};
                not_owner ->
                    {TabState2, Errs}
            end
        end,
    case lists:foldl(Terminate, {TabState, []}, Children) of
        {TabState2, []} ->
            {ok, TabState2};
        {TabState2, Errs} ->
            {error, TabState2, Errs}
    end.


do_terminate_child(Name, Id_or_Pid, TabMod, TabState) ->
    Self = erlang:self(),
    Search =
        if
            erlang:is_pid(Id_or_Pid) ->
                director_table:lookup_pid(TabMod, TabState, Id_or_Pid);
            true ->
                director_table:lookup_id(TabMod, TabState, Id_or_Pid)
        end,
    _ =
        case Search of
            {ok, #?CHILD{pid = Pid, supervisor = Self}=Child2} when erlang:is_pid(Pid) ->
                ok = do_terminate_child(Name, Child2);
            {ok, #?CHILD{pid = restarting, timer_reference = Ref, supervisor = Self}} ->
                _ = erlang:cancel_timer(Ref, [{async, true}]),
                ok;
            _ ->
                ok
        end,
    case Search of
        {ok, not_found} ->
            not_found;
        {ok, #?CHILD{supervisor = Self}=Child3} ->
            case director_table:insert(TabMod
                                      ,TabState
                                      ,Child3#?CHILD{pid = undefined
                                                    ,extra = undefined
                                                    ,timer_reference = undefined}) of
                {ok, _}=Ok ->
                    Ok;
                {error, _}=Err2 ->
                    Err2
            end;
        {ok, _} ->
            not_owner;
        {error, _}=Err2 ->
            Err2
    end.


do_terminate_child(Name
                  ,#?CHILD{pid=Pid
                          ,terminate_timeout = TerminateTimeout}=Child) when erlang:is_pid(Pid) ->
    BrutalKill =
        fun() ->
            erlang:exit(Pid, kill),
            receive
                {'DOWN', _Ref, process, Pid, killed} ->
                    ok;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    director_utils:error_report(Name, shutdown_error, Reason, Child),
                    ok
            end
        end,
    case monitor_child(Pid) of
        ok ->
            if
                TerminateTimeout =:= 0 ->
                    BrutalKill();
                true ->
                    erlang:exit(Pid, shutdown),
                    receive
                        {'DOWN', _Ref, process, Pid, shutdown} ->
                            ok;
                        {'DOWN', _Ref, process, Pid, Reason2} ->
                            director_utils:error_report(Name
                                                       ,shutdown_error
                                                       ,Reason2
                                                       ,Child),
                            ok
                    after TerminateTimeout ->
                        BrutalKill()
                    end
            end;
        {error, Reason3} ->
            director_utils:error_report(Name, shutdown_error, Reason3, Child),
            ok
    end.


monitor_child(Pid) ->
    erlang:monitor(process, Pid),
        catch erlang:unlink(Pid),
    receive
        {'EXIT', Pid, Reason} ->
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    {error, Reason}
            end
    after 0 ->
        ok
    end.


restart_timer(PosInt, Id) ->
    erlang:start_timer(PosInt, erlang:self(), Id, []).


reply(Dbg, Name, {Pid, Tag}=_From, Result) ->
    Pid ! {Tag, Result},
    director_utils:debug(Dbg, Name, {out, Pid, Result}).


check_duplicate_ids(Children) ->
    check_duplicate_ids([Id || #?CHILD{id = Id} <- Children], []).


check_duplicate_ids([Id|Ids], Ids2) ->
    case lists:member(Id, Ids2) of
        true ->
            {error, {duplicate_child_name, Id}}; %% Like OTP/supervisor
        false ->
            check_duplicate_ids(Ids, [Id|Ids2])
    end;
check_duplicate_ids([], _Ids2) ->
    ok.


change_old_children_pids([#?CHILD{id = Id}=Child|Children], TabMod, TabState) ->
    Pid =
        case director_table:lookup_id(TabMod, TabState, Id) of
            {ok, not_found}=Ok ->
                Ok;
            {ok, #?CHILD{pid = Pid2}} ->
                {ok, Pid2};
            {error, _}=Err ->
                Err
        end,
    case Pid of
        {ok, Pid3} ->
            case director_table:insert(TabMod, TabState, Child#?CHILD{pid = Pid3}) of
                {ok, TabState2} ->
                    change_old_children_pids(Children, TabMod, TabState2);
                {error, _}=Err2 ->
                    Err2
            end;
        {error, _}=Err2 ->
            Err2
    end;
change_old_children_pids([], _TabMod, TabState) ->
    {ok, TabState}.