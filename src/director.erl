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
%% @version  17.12.7
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
        ,log_validator/4
        ,terminate_and_delete_child/2
        ,default_childspec/0
        ,default_plan/0
        ,become_supervisor/3
        ,delete_running_child/2]).

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
        ,terminate_and_delete_child/3
        ,become_supervisor/4
        ,delete_running_child/3]).

%% gen callback:
-export([init_it/6]).

%% sys callbacks:
-export([system_code_change/4
        ,system_continue/3
        ,system_get_state/1
        ,system_replace_state/2
        ,system_terminate/4
        ,format_status/2]).

%% Do not use ! just for tables
-export([self_start/1]).

%% -------------------------------------------------------------------------------------------------
%% Types:

-type childspec() :: #{'id' => id()
                      ,'start' => start()
                      ,'plan' => plan()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()
                      ,'log_validator' => log_validator()
                      ,'delete_before_terminate' => delete_before_terminate()
                      ,'state' => state()}.
-type  id() :: term().
-type  start() :: module() % will be {module(), start_link, []}
                | {module(), function()} % will be {module(), function(), []}
                | mfa().
-type  plan() :: fun((Id::term(), Reason::term(), RestartCount::pos_integer(), state()) ->
                     {'restart', state()}                 |
                     {{'restart', pos_integer()}, state()}|
                     {'wait', state()}                    |
                     {'delete', state()}                  |
                     {'stop', state()}                    |
                     {{'stop', Reason::term()}, state()}) .
-type  terminate_timeout() :: 'infinity' | non_neg_integer().
-type  type() :: 'worker' | 'supervisor' | 'sup' | 's' | 'w'.
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().
-type  log_validator() :: fun((Name::any(), Type:: log_level(), Extra::term(), state()) ->
                              log_mode()).
-type   log_level() :: 'info' | 'error' | 'warning'.
-type   log_mode() :: 'short' | 'long' | 'none'.
-type  delete_before_terminate() :: boolean().
-type  state() :: any().

-type default_childspec() :: #{'start' => start()
                              ,'plan' => plan()
                              ,'terminate_timeout' => terminate_timeout()
                              ,'type' => type()
                              ,'modules' => modules()
                              ,'log_validator' => log_validator()
                              ,'delete_before_terminate' => delete_before_terminate()
                              ,'state' => state()}.

-type start_return() :: {'ok', pid()} | {'ok', pid(), any()} | 'ignore' | {'error', term()}.

-type init_return() :: {'ok', state(), [childspec()]|[]}
                     | {'ok', state(), [childspec()]|[], default_childspec()}
                     | {'ok', state(), [childspec()]|[], start_options()}
                     | {'ok', state(), [childspec()]|[], default_childspec(), start_options()}
                     | 'ignore'
                     | {'stop', Reason::any()}.

-type terminate_return() :: {'error', NewReason::any()} | any().

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type director() :: pid() | atom() | tuple().

-type start_options() :: [start_option()] | [].
-type  start_option() :: {'debug', [sys:dbg_opt()]|[]}
                       | {'spawn_opt', proc_lib:spawn_option()}
                       | {'timeout', timeout()}
                       | {'log_validator', log_validator()}
                       | {'table_module', module()}
                       | {'table_init_argument', any()}
                       | {'delete_table_before_terminate', boolean()}.

-export_type([childspec/0
             ,id/0
             ,start/0
             ,plan/0
             ,terminate_timeout/0
             ,type/0
             ,modules/0
             ,append/0
             ,log_validator/0
             ,log_level/0
             ,log_mode/0
             ,delete_before_terminate/0
             ,state/0
             ,default_childspec/0
             ,start_return/0
             ,init_return/0
             ,terminate_return/0
             ,register_name/0
             ,director/0
             ,start_options/0
             ,start_option/0]).

%% -------------------------------------------------------------------------------------------------
%% Behaviour information:

-callback
init(InitArg::any()) ->
    init_return().


-callback
terminate(Reason::any(), state()) ->
    terminate_return().

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
%%      Starts a child using childspec.<br/>
%%      {'ok', 'undefined'} is for a child which ignored to start.<br/>
%%      Reason of error may be for childspec not starting of process.
%% @end
start_child(Director, ChildSpec) when ?is_director(Director) andalso erlang:is_map(ChildSpec) ->
    gen_server:call(Director, {?START_CHILD_TAG, ChildSpec}).


-spec
restart_child(director(), id()) ->
    {'ok', pid()}                 |
    {'ok', pid(), Extra::term()} |
    {'error', Reason::'running'|'restarting'|'not_found'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.<br/>
%%      Reason of error may be for starting process or table.
%% @end
restart_child(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?RESTART_CHILD_TAG, Id}).


-spec
terminate_child(director(), id() | pid()) ->
    'ok' | {'error', Reason :: 'not_found' | 'not_parent' | term()}.
%% @doc
%%      terminates a running child using child id or child pid.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot<br/>
%%      terminate child and {error, not_parent} error will return.
%% @end
terminate_child(Director, Id_or_Pid) when ?is_director(Director) ->
    gen_server:call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}).


-spec
delete_child(director(), id()) ->
    'ok' | {'error', Reason :: 'not_found' | 'running' | 'not_parent' | term()}.
%% @doc
%%      Deletes a terminated or waited child using child id.<br/>
%%      Error maybe occur for reading from or inserting to table.
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
%%      Returns count of children.<br/>
%%      Error maybe occur for reading from table.
%% @end
count_children(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?COUNT_CHILDREN_TAG).


-spec
which_children(director()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [] | {'error', term()}.
%% @doc
%%      Returns information about each child.<br/>
%%      Error maybe occur for reading from table.
%% @end
which_children(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?WHICH_CHILDREN_TAG).


-spec
get_childspec(director(), id() | pid()) ->
    {'ok', childspec()} | {'error', 'not_found' | term()}.
%% @doc
%%      Returns childspec of child id or child pid.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Director, Id_or_Pid) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_CHILDSPEC_TAG, Id_or_Pid}).


-spec
check_childspec(childspec()) ->
    'ok' | {'error', Reason::term()}.
%% @doc
%%      checks childspec.<br/>
%%      This code will execute in process of caller.
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
    'ok' | {'error', 'not_found' | 'not_parent' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.<br/>
%%      Be careful about using this, because in next crash of child you may see different behavior.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot change<br/>
%%      plan and {error, not_parent} error will occur.
%% @end
change_plan(Director, Id, Plan) when ?is_director(Director) andalso erlang:is_function(Plan, 4) ->
    gen_server:call(Director, {?CHANGE_PLAN_TAG, Id, Plan}).


-spec
get_plan(director(), id()) ->
    {'ok', plan()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns plan of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_plan(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_PLAN_TAG, Id}).


-spec
get_restart_count(director(), id()) ->
    {'ok', non_neg_integer()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns restart count of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_RESTART_COUNT_TAG, Id}).


-spec
get_pid(director(), id()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @doc
%%      Returns pid of child id if child is running.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pid(Director, Id) when ?is_director(Director) ->
    gen_server:call(Director, {?GET_PID_TAG, Id}).


-spec
get_pids(director()) ->
    {'ok', [{id(), pid()}] | []} | {'error', term()}.
%% @doc
%%      Returns list of {id, pid}s for all running children.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pids(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?GET_PIDS_TAG).


-spec
get_default_childspec(director()) ->
    default_childspec().
%% @doc
%%      Returns director process's default childspec.
%% @end
get_default_childspec(Director) when ?is_director(Director) ->
    gen_server:call(Director, ?GET_DEF_CHILDSPEC).


-spec
change_default_childspec(director(), default_childspec()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director's default childspec.<br/>
%%      Be careful about using this. This may change all children behavior with append => true in<br/>
%%      their next restart.
%% @end
change_default_childspec(Director, DefChildSpec) when ?is_director(Director) andalso
                                                      erlang:is_map(DefChildSpec) ->
    gen_server:call(Director, {?CHANGE_DEF_CHILDSPEC, DefChildSpec}).


-spec
change_log_validator(director(), log_validator()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes log validator fun for director itself.<br/>
%% @end
change_log_validator(Director, LogValidator) when ?is_director(Director) andalso
                                                  erlang:is_function(LogValidator, 4) ->
    gen_server:call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator}).


-spec
change_log_validator(director(), Id::term()|log_validator(), log_validator()|timeout()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes log validator fun for child id if log validator is third argument otherwise this is<br/>
%%      previous function with timeout argument.<br/>
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
%%      Stops director process with reason termination 'normal' and 'infinity' as timeout.
%% @end
stop(Director) when ?is_director(Director) ->
    proc_lib:stop(Director, normal, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term()) ->
    'ok'.
%% @doc
%%      Stops director process with 'infinity' timeout.
%% @end
stop(Director, Reason) when ?is_director(Director) ->
    proc_lib:stop(Director, Reason, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term(), timeout()) ->
    'ok'.
%% @doc
%%      Stops director process.
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
%%      This is default plan of a childspec. <br/>
%%      Restarts child if child crashed with any reason for three times, restart it after 1000<br/>
%%      milli-seconds for fourth crash and stop entire supervisor if child crashed five times with<br/>
%%      crash reason of child.
%% @end
plan(_Id, _Other, 4, State) ->
    {{restart, 1000}, State};
plan(_Id, _Other, 5, State) ->
    {stop, State};
plan(_Id, _Other, _RestartCount, State) ->
    {restart, State}.


-spec
log_validator(Id_Or_Name::any(), Type::log_level(), Extra::term(), State::any()) ->
    'short'.
%% @doc
%%      This is default log validator of director itself and its children.<br/>
%%      Tells director to call error_logger with Short description for every log.
%% @end
log_validator(_Id, _Type, _Extra, _State) ->
    short.


-spec
terminate_and_delete_child(director(), id()|pid()) ->
    'ok' | {'error', 'not_found'|'not_parent'|term()}.
%% @doc
%%      Terminates running child and deletes it using child id or pid of child.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot<br/>
%%      terminate child and error {error, not_parent} will occur.<br/>
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


-spec
become_supervisor(director(), childspec(), pid()) ->
    'ok' | {'error', 'noproc' | term()}.
%% @doc
%%      Tells director process become supervisor of this Pid with this Childspec.<br/>
%%      Pid should be alive Erlang process on same node.<br/>
%%      After this, new child process will have an Erlang message {'change_parent', NewParentPid} in<br/>
%%      Its mailbox.<br/>
%%      Error maybe occur for reading from or inserting to table.
%% @end
become_supervisor(Director, ChildSpec, Pid) when ?is_director(Director) andalso
                                                 erlang:is_map(ChildSpec) andalso
                                                 erlang:is_pid(Pid) ->
    gen_server:call(Director, {?BECOME_SUPERVISOR_TAG, ChildSpec, Pid}).


-spec
delete_running_child(director(), pid()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Tells to director process delete running child with process id Pid and let it free!<br/>
%%      Sometime it's useful to make a process unsupervised or you may want to change the parent.<br/>
%%      For changing parent after calling this you should call 'become_supervisor'/3-4 with new<br/>
%%      director as new parent.
%% @end
delete_running_child(Director, Pid) when ?is_director(Director) andalso erlang:is_pid(Pid) ->
    gen_server:call(Director, {?DELETE_RUNNING_CHILD_TAG, Pid}).


%% -------------------------------------------------------------------------------------------------
%% previous APIs with Timeout argument:

-spec
start_child(director(), childspec(), timeout()) ->
    start_return().
%% @doc
%%      Starts child using childspec.
%%      Reason of error may be for childspec not starting process or reading from or inserting<br/>
%%      children to table.
%% @end
start_child(Director, ChildSpec, Timeout) when ?is_director(Director) andalso
                                               erlang:is_map(ChildSpec) andalso
                                               ?is_timeout(Timeout) ->
    gen_server:call(Director, {?START_CHILD_TAG, ChildSpec}, Timeout).


-spec
restart_child(director(), id(), timeout()) ->
    {'ok', pid()}                                                            |
    {'ok', pid(), Extra::term()}                                             |
    {'error', Reason::'running'|'restarting'|'not_found'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.<br/>
%%      Reason of error may be for starting process or table.
%% @end
restart_child(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?RESTART_CHILD_TAG, Id}, Timeout).


-spec
terminate_child(director(), id() | pid(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'not_parent'|term()}.
%% @doc
%%      terminates a running child using child id or child pid.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot<br/>
%%      terminate child and {error, not_parent} error will return.
%% @end
terminate_child(Director, Id_or_Pid, Timeout) when ?is_director(Director) andalso
                                                   ?is_timeout(Timeout) ->
    gen_server:call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}, Timeout).


-spec
delete_child(director(), id(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'running'|'not_parent'|term()}.
%% @doc
%%      Deletes a terminated or waited child using child id.<br/>
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
%%      Returns count of children.<br/>
%%      Error is for reading from table.
%% @end
count_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?COUNT_CHILDREN_TAG, Timeout).


-spec
which_children(director(), timeout()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [] | {'error', term()}.
%% @doc
%%      Returns information about each child.<br/>
%%      Error is for reading from table.
%% @end
which_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?WHICH_CHILDREN_TAG, Timeout).


-spec
get_childspec(director(), id() | pid(), timeout()) ->
    {'ok', childspec()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns childspec of child.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Director, Name, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_CHILDSPEC_TAG, Name}, Timeout).


-spec
get_restart_count(director(), id(), timeout()) ->
    {'ok', non_neg_integer()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns restart count of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_RESTART_COUNT_TAG, Id}, Timeout).


-spec
change_plan(director(), id(), plan(), timeout()) ->
    'ok' | {'error', 'not_found' | 'not_parent' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.<br/>
%%      Be careful about using this, because in next crash of child you may see different behavior.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot change<br/>
%%      plan and {error, not_parent} error will occur.
%% @end
change_plan(Director, Id, Plan, Timeout) when ?is_director(Director) andalso
                                              erlang:is_function(Plan, 4) andalso
                                              ?is_timeout(Timeout) ->
    gen_server:call(Director, {?CHANGE_PLAN_TAG, Id, Plan}, Timeout).


-spec
get_plan(director(), id(), timeout()) ->
    {'ok', plan()} | {'error', 'not_found' | term()}.
%% @doc
%%      Returns plan of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_plan(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_PLAN_TAG, Id}, Timeout).


-spec
get_pid(director(), id(), timeout()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @doc
%%      Returns pid of a running child.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pid(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_PID_TAG, Id}, Timeout).


-spec
get_pids(director(), timeout()) ->
    {'ok', [{id(), pid()}] | []} | {'error', term()}.
%% @doc
%%      Returns list of {id, pid}s for all running children.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pids(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_PIDS_TAG, Timeout).


-spec
get_default_childspec(director(), timeout()) ->
    default_childspec().
%% @doc
%%      Returns director process's default childspec.
%% @end
get_default_childspec(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_DEF_CHILDSPEC, Timeout).


-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director's default childspec.<br/>
%%      Be careful about using this. This may change all children behavior with append => true in<br/>
%%      their next restart.
%% @end
change_default_childspec(Director, ChildSpec, Timeout) when ?is_director(Director) andalso
                                                            erlang:is_map(ChildSpec) andalso
                                                            ?is_timeout(Timeout) ->
    gen_server:call(Director, {?CHANGE_DEF_CHILDSPEC, ChildSpec}, Timeout).


-spec
terminate_and_delete_child(director(), id()|pid(), timeout()) ->
    'ok' | {'error', 'not_found'|'not_parent'|term()}.
%% @doc
%%      Terminates running child and deletes it using child id or pid of child.<br/>
%%      If this supervisor is not owner of child and has access to children table, it cannot<br/>
%%      terminate child and error {error, not_parent} will occur.<br/>
%%      Error maybe occur for reading from or inserting to table.
%% @end
terminate_and_delete_child(Director, Id, Timeout) when ?is_director(Director) andalso
                                                       ?is_timeout(Timeout) ->
    gen_server:call(Director, {?TERMINATE_AND_DELETE_CHILD_TAG, Id}, Timeout).


-spec
become_supervisor(director(), childspec(), pid(), timeout()) ->
    ok | {'error', 'no_proc' | 'no_response' | 'proc_exited' | term()}.
%% @doc
%%      Tells director process become supervisor of this Pid with this Childspec.<br/>
%%      Pid should be alive Erlang process on same node.<br/>
%%      After this, new child process will have an Erlang message {'change_parent', NewParentPid} in<br/>
%%      Its mailbox.<br/>
%%      Error maybe occur for reading from or inserting to table.
%% @end
become_supervisor(Director, ChildSpec, Pid, Timeout) when ?is_director(Director) andalso
                                                          erlang:is_map(ChildSpec) andalso
                                                          erlang:is_pid(Pid) andalso
                                                          ?is_timeout(Timeout) ->
    gen_server:call(Director, {?BECOME_SUPERVISOR_TAG, ChildSpec, Pid}, Timeout).


-spec
delete_running_child(director(), pid(), timeout()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Tells to director process delete running child with process id Pid and let it free!<br/>
%%      Sometime it's useful to make a process unsupervised or you may want to change the parent.<br/>
%%      For changing parent after calling this you should call 'become_supervisor'/3-4 with new<br/>
%%      director as new parent.
%% @end
delete_running_child(Director, Pid, Timeout) when ?is_director(Director) andalso
                                                  erlang:is_pid(Pid) andalso
                                                  ?is_timeout(Timeout) ->
    gen_server:call(Director, {?DELETE_RUNNING_CHILD_TAG, Pid}, Timeout).

%% @hidden
self_start(Id) ->
    restart_timer(0, Id).


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
                                    {new_error, Rsn2} ->
                                        Rsn2;
                                    {crash, Rsn2} ->
                                        [Rsn0, Rsn2]
                                end,
                            proc_lib:init_ack(Starter, {error, Rsn}),
                            erlang:exit(Rsn)
                    end;
                {hard_error, Rsn0} ->
                    unregister_name(Name0),
                    Rsn =
                        case call_terminate(Mod, Data, Rsn0) of
                            ok ->
                                Rsn0;
                            {new_error, Rsn2} ->
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
        {ok, {_, Children, DefChildSpec, _, _, _, _, _}} ->
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
format_status(_, [_PDict, SysState, Parent, Debug, #?STATE{name = Name, module = Mod}=_State]) ->
    Header = {header, "Status for director " ++ io_lib:print(Name)},
    Data = {data, [{"Status", SysState}
                  ,{"Parent", Parent}
                  ,{"Logged events", sys:get_debug(log, Debug, [])}]},
    case erlang:list_to_integer(erlang:system_info(otp_release)) of
        Ver when Ver >= 19 ->
            State2 = {state
                     ,name
                     ,strategy
                     ,children
                     ,dynamics
                     ,intensity
                     ,period
                     ,restarts
                     ,Mod
                     ,Mod
                     ,ags},
            Specific = {data, [{"State", State2}]},
            [Header, Data, Specific, {supervisor, [{"Callback", Mod}]}];
        _ ->
            State2 = {state
                     ,name
                     ,strategy
                     ,children
                     ,dynamics
                     ,intensity
                     ,period
                     ,restarts
                     ,dynamic_restarts
                     ,Mod
                     ,ags},
            Specific = {data, [{"State", State2}]},
            [Header, Data , Specific]
    end.


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
process_message(Parent, Dbg, #?STATE{name = Name
                                    ,table_module = TabMod
                                    ,table_state = TabState
                                    ,log_validator = LogValidator
                                    ,data = Data}=State, Msg) ->
    case director_table:handle_message(TabMod, TabState, Msg) of
        {ok, TabState2} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {soft_error, TabState2, unknown} ->
            NewData =
                case director_utils:run_log_validator(LogValidator
                                                     ,Name
                                                     ,warning
                                                     ,{receive_unexpected_message, Msg}
                                                     ,Data) of
                    {short, NewData0} ->
                        error_logger:error_msg("Director ~p received an unexpected message~n"
                                              ,[Name]),
                        NewData0;
                    {none, NewData0} ->
                        NewData0;
                    {long, NewData0} ->
                        error_logger:error_msg("Director ~p received unexpected message: ~p~n"
                                              ,[Name, Msg]),
                        NewData0
                end,
            loop(Parent, Dbg, State#?STATE{table_state = TabState2, data = NewData});
        {hard_error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState,  name = Name}=State
               ,From
               ,{?GET_PID_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {reply(Dbg, Name, From, {ok, Pid}), State};
        {ok, #?CHILD{pid = restarting, timer_reference = undefined}} ->
            {reply(Dbg, Name, From, {error, undefined}), State};
        {ok, #?CHILD{pid = Pid}} ->
            {reply(Dbg, Name, From, {error, Pid}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?DELETE_CHILD_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {reply(Dbg, Name, From, {error, running}), State};
        {ok, #?CHILD{supervisor = Sup}} when erlang:is_pid(Sup) andalso erlang:self() =/= Sup ->
            {reply(Dbg, Name, From, {error, not_parent}), State};
        {ok, Child} ->
            case director_table:delete(TabMod, TabState, Child) of
                {ok, TabState2} ->
                    {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, Child} ->
            {reply(Dbg, Name, From, {ok, director_utils:c2cs(Child)}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    case do_restart_child(Name, Id, TabMod, TabState) of
        {ok, #?CHILD{pid = Pid, extra = undefined}, TabState2} ->
            {reply(Dbg, Name, From, {ok, Pid}), State#?STATE{table_state = TabState2}};
        {ok, #?CHILD{pid = Pid, extra = {value, Extra}}, TabState2} ->
            {reply(Dbg, Name, From, {ok, Pid, Extra}), State#?STATE{table_state = TabState2}};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
                {ok, #?CHILD{pid = Pid, extra = undefined}, TabState2} ->
                    {reply(Dbg, Name, From, {ok, Pid}), State#?STATE{table_state = TabState2}};
                {ok, #?CHILD{pid = Pid, extra = {value, Extra}}, TabState2} ->
                    {reply(Dbg, Name, From, {ok, Pid, Extra})
                    ,State#?STATE{table_state = TabState2}};
                {soft_error, TabState2, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn} ->
                    Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
        {ok, _Child, TabState2} ->
            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            Wrap =
                fun
                    (#?CHILD{id = Id
                            ,type = Type
                            ,pid = restarting
                            ,modules = Mods
                            ,timer_reference = undefined}) ->
                        {Id, undefined, Type, Mods};
                    (#?CHILD{id = Id, type = Type, pid = Pid, modules = Mods}) ->
                        {Id, Pid, Type, Mods}
                end,
            Reply = [Wrap(Child) || Child <- Children],
            {reply(Dbg, Name, From, Reply), State};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_PLAN_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{plan = Plan}} ->
            {reply(Dbg, Name, From, {ok, Plan}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?CHANGE_PLAN_TAG, Id, Plan}) ->
    case director_utils:filter_plan(Plan) of
        {ok, Plan2} ->
            case director_table:lookup_id(TabMod, TabState, Id) of
                {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
                    {reply(Dbg, Name, From, {error, not_parent}), State};
                {ok, Child} ->
                    Child2 = Child#?CHILD{plan = Plan2},
                    case director_table:insert(TabMod, TabState, Child2) of
                        {ok, TabState2} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                        {hard_error, Rsn}=Err ->
                            Dbg2 = reply(Dbg, Name, From, Err),
                            terminate(Dbg2, State, Rsn)
                    end;
                {soft_error, TabState2, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn} ->
                    Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
        {ok, #?CHILD{restart_count = Count}} ->
            {reply(Dbg, Name, From, {ok, Count}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
                        {soft_error, TabState2, Rsn} ->
                            {reply(Dbg, Name, From, {error, Rsn})
                            ,State#?STATE{table_state = TabState2}};
                        {hard_error, Rsn} ->
                            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
                            terminate(Dbg2, State, Rsn)
                    end;
                {soft_error, TabState2, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn} ->
                    Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
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
                {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
                    {reply(Dbg, Name, From, {error, not_parent}), State};
                {ok, Child} ->
                    case director_table:insert(TabMod
                                              ,TabState
                                              ,Child#?CHILD{log_validator = LogValidator}) of
                        {ok, TabState2} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                        {hard_error, _}=Err ->
                            {reply(Dbg, Name, From, Err), State}
                    end;
                {soft_error, TabState2, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn} ->
                    Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
                    terminate(Dbg2, State, Rsn)
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
        {ok, Child, TabState2} ->
            case director_table:delete(TabMod, TabState2, Child) of
                {ok, TabState3} ->
                    {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState3}};
                {hard_error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;
process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name, default_childspec = DefChildSpec}=State
               ,From
               ,{?BECOME_SUPERVISOR_TAG, ChildSpec, Pid}) ->
    case erlang:is_process_alive(Pid) of % Proc should be on same node
        true ->
            case director_utils:check_childspec(ChildSpec, DefChildSpec) of
                {ok, #?CHILD{id = Id, start = Start}=Child} ->
                    case director_table:lookup_id(TabMod, TabState, Id) of
                        {soft_error, TabState2, not_found} ->
                            try erlang:link(Pid) of
                                _ ->
                                    case director_table:change_parent(TabMod
                                                                     ,TabState2
                                                                     ,Child#?CHILD{pid = Pid}) of
                                        {ok, TabState3} ->
                                            Pid ! {?CHANGE_PARENT_TAG, erlang:self()},
                                            {reply(Dbg, Name, From, ok)
                                            ,State#?STATE{table_state = TabState3}};
                                        {soft_error, TabState3, Rsn} ->
                                            {reply(Dbg, Name, From, {error, Rsn})
                                            ,State#?STATE{table_state = TabState3}};
                                        {hard_error, Rsn} ->
                                            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
                                            terminate(Dbg2, State, Rsn)
                                    end
                            catch
                                _:Rsn ->
                                    {reply(Dbg, Name, From, {error, Rsn})
                                    ,State#?STATE{table_state = TabState2}}
                            end;
                        {ok, #?CHILD{pid = Pid2}} when Pid2 == Pid ->
                            {reply(Dbg, Name, From, {error, {already_started, Pid2}}), State};
                        {ok, #?CHILD{start = Start2}} when Start == Start2 ->
                            {reply(Dbg, Name, From, {error, {already_present, Id}}), State};
                        {ok, _} ->
                            {reply(Dbg, Name, From, {error, {duplicate_child_name, Id}}), State};
                        {hard_error, Rsn} ->
                            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
                            terminate(Dbg2, State, Rsn)
                    end;
                {error, Rsn} ->
                    {reply(Dbg, Name, From, {error, Rsn}), State}
            end;
        false ->
            {reply(Dbg, Name, From, {error, noproc}), State}
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?DELETE_RUNNING_CHILD_TAG, Pid}) ->
    case director_table:lookup_pid(TabMod, TabState, Pid) of
        {ok, Child} ->
            try erlang:unlink(Pid) of
                _ ->
                    case director_table:delete(TabMod, TabState, Child) of
                        {ok, TabState2} ->
                            {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                        {soft_error, TabState2, Rsn} ->
                            {reply(Dbg, Name, From, {error, Rsn})
                            ,State#?STATE{table_state = TabState2}};
                        {hard_error, Rsn} ->
                            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
                            terminate(Dbg2, State, Rsn)
                    end
            catch
                _:Rsn ->
                    {reply(Dbg, Name, From, {error, Rsn}), State}
            end;
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            Dbg2 = reply(Dbg, Name, From, {error, Rsn}),
            terminate(Dbg2, State, Rsn)
    end;


%% Catch clause:
process_request(Dbg, #?STATE{name = Name, log_validator = LogValidator, data = Data}=State, From, Other) ->
    NewData =
        case director_utils:run_log_validator(LogValidator
                                             ,Name
                                             ,warning
                                             ,{receive_unexpected_call, From, Other}
                                             ,Data) of
            {short, NewData0} ->
                error_logger:error_msg("Director ~p received an unexpected call request~n", [Name]),
                NewData0;
            {none, NewData0} ->
                NewData0;
            {long, NewData0} ->
                error_logger:error_msg("Director ~p received unexpected call request ~p with from ~p~n"
                                      ,[Name, Other, From]),
                NewData0
        end,
    {reply(Dbg, Name, From, {error, {unknown_request, Other}}), State#?STATE{data = NewData}}.


process_exit(Parent, Dbg, State, Parent, Reason) ->
    terminate(Dbg, State, Reason);
process_exit(Parent
            ,Dbg
            ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
            ,Pid
            ,Reason) ->
    case director_table:lookup_pid(TabMod, TabState, Pid) of
        {ok, #?CHILD{supervisor = Sup}} when erlang:self() =/= Sup ->
            loop(Parent, Dbg, State);
        {ok, Child} ->
            Child2 = Child#?CHILD{pid = restarting
                                 ,extra = undefined
                                 ,state = director_utils:error_report(Name
                                                                     ,child_terminated
                                                                     ,Reason
                                                                     ,Child)},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    handle_exit(Parent, Dbg, State#?STATE{table_state = TabState2}, Child2, Reason);
                {hard_error, Rsn} ->
                    terminate(Dbg, State, Rsn)
            end;
        {soft_error, TabState2, _} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {hard_error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


handle_exit(Parent
           ,Dbg
           ,#?STATE{name = Name
                   ,table_module = TabMod
                   ,table_state = TabState}=State
           ,#?CHILD{id = Id
                   ,plan = Plan
                   ,restart_count = ResCount
                   ,state = ChState}=Child
           ,Reason) ->
    ResCount2 = ResCount + 1,
    {Dbg2, Strategy} =
        try
            Strategy0 = Plan(Id, Reason, ResCount2, ChState),
            {director_utils:debug(Dbg, Name, {plan, Id, Strategy0}), {value, Strategy0}}
        catch
            _:Rsn ->
                {Dbg, {error, {plan_crash, [{reason, Rsn}
                                           ,{plan, Plan}
                                           ,{arguments, [Id, Reason, ResCount2, ChState]}
                                           ,{stacktrace, erlang:get_stacktrace()}]}}}
        end,
    case Strategy of
        {value, {restart, ChState2}} ->
            case do_restart_child(Name, Id, TabMod, TabState) of
                {ok, _Child, TabState2} when ChState =:= ChState2 ->
                    loop(Parent, Dbg2, State#?STATE{table_state = TabState2});
                {ok, Child2, TabState2} ->
                    case director_table:insert(TabMod, TabState2, Child2#?CHILD{state = ChState2}) of
                        {ok, TabState3} ->
                            loop(Parent, Dbg2, State#?STATE{table_state = TabState3});
                        {hard_error, Rsn3} ->
                            terminate(Dbg2
                                     ,State#?STATE{table_state = TabState2}
                                     ,Rsn3)
                    end;
                {soft_error, TabState2, _Reason3} ->
                    TimeRef = restart_timer(0, Id),
                    case director_table:insert(TabMod
                                              ,TabState2
                                              ,Child#?CHILD{pid = restarting
                                                           ,timer_reference = TimeRef
                                                           ,state = ChState2}) of
                        {ok, TabState3} ->
                            loop(Parent
                                ,Dbg2
                                ,State#?STATE{table_state = TabState3});
                        {hard_error, Rsn3} ->
                            terminate(Dbg2
                                     ,State#?STATE{table_state = TabState2}
                                     ,Rsn3)
                    end;
                {hard_error, Rsn2} ->
                    terminate(Dbg2, State, Rsn2)
            end;
        {value, {stop, ChState2}} ->
            case director_table:insert(TabMod, TabState, Child#?CHILD{state = ChState2}) of
                {ok, TabState3} ->
                    terminate(Dbg2, State#?STATE{table_state = TabState3}, Reason);
                {hard_error, Rsn3} ->
                    terminate(Dbg2, State, Rsn3)
            end;
        {value, {{stop, Reason3}, ChState2}} ->
            case director_table:insert(TabMod, TabState, Child#?CHILD{state = ChState2}) of
                {ok, TabState3} ->
                    terminate(Dbg2, State#?STATE{table_state = TabState3}, Reason3);
                {hard_error, Rsn3} ->
                    terminate(Dbg2, State, Rsn3)
            end;
        {value, {delete, ChState2}} ->
            case director_table:delete(TabMod, TabState, Child#?CHILD{state = ChState2}) of
                {ok, TabState2} ->
                    loop(Parent, Dbg2, State#?STATE{table_state = TabState2});
                {soft_error, TabState2, not_found} ->
                    loop(Parent, Dbg2, State#?STATE{table_state = TabState2});
%%                {soft_error, TabState2, Rsn2} ->
%%                    terminate(Dbg2, State#?STATE{table_state = TabState2}, Rsn2);
                {hard_error, Rsn2} ->
                    terminate(Dbg2, State, Rsn2)
            end;
        {value, {wait, ChState2}} ->
            case director_table:insert(TabMod, TabState, Child#?CHILD{state = ChState2}) of
                {ok, TabState3} ->
                    loop(Parent, Dbg2, State#?STATE{table_state = TabState3});
                {hard_error, Rsn3} ->
                    terminate(Dbg2, State, Rsn3)
            end;
        {value, {{restart, PosInt}, ChState2}} when erlang:is_integer(PosInt) andalso PosInt >= 0 ->
            TimeRef = restart_timer(PosInt, Id),
            case director_table:insert(TabMod
                                      ,TabState
                                      ,Child#?CHILD{pid = restarting
                                                   ,timer_reference = TimeRef
                                                   ,state = ChState2}) of
                {ok, TabState2} ->
                    loop(Parent, Dbg2, State#?STATE{table_state = TabState2});
                {hard_error, Rsn2} ->
                    terminate(Dbg2, State, Rsn2)
            end;
        {error, Reason3} ->
            terminate(Dbg2, State, Reason3);
        {value, Other} ->
            terminate(Dbg2
                     ,State
                     ,{plan_return, [{returned_value, Other}
                                    ,{plan, Plan}
                                    ,{id, Id}
                                    ,{reason_argument, Reason}
                                    ,{restart_count, ResCount2}]})
    end.


process_timeout(Parent
               ,Dbg
               ,#?STATE{name = Name, table_module = TabMod, table_state = TabState}=State
               ,TimerRef
               ,Id) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{timer_reference = TimerRef}} ->
            case do_restart_child(Name, Id, TabMod, TabState) of
                {ok, _Child, TabState3} ->
                    loop(Parent, Dbg, State#?STATE{table_state = TabState3});
                {soft_error, TabState2, Reason} ->
                    case director_table:lookup_id(TabMod, TabState2, Id) of
                        {ok, Child} ->
                            handle_exit(Parent
                                       ,Dbg
                                       ,State#?STATE{table_state = TabState2}
                                       ,Child
                                       ,Reason);
                        {soft_error, TabState3, _} ->
                            loop(Parent, Dbg, State#?STATE{table_state = TabState3});
                        {hard_error, Rsn} ->
                            terminate(Dbg, State#?STATE{table_state = TabState2}, Rsn)
                    end;
                {hard_error, Rsn} ->
                    terminate(Dbg, State, Rsn)
            end;
        {ok, _} ->
            loop(Parent, Dbg, State);
        {soft_error, TabState2, _} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {hard_error, Rsn} ->
            terminate(Dbg, State, Rsn)
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
                        case director_utils:check_childspecs(ChildSpecs, DefChildSpec2) of
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
                        case director_utils:check_childspecs(ChildSpecs, DefChildSpec2) of
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
                                          ,{arguments, [InitArg]}]}}
        catch
            _:Rsn ->
                {error, {init_crash, [{reason, Rsn}
                                     ,{module, Mod}
                                     ,{function, init}
                                     ,{arguments, [InitArg]}
                                     ,{stacktrace, erlang:get_stacktrace()}]}}
        end,
    case Rslt of
        {ok, Data2, ChildSpecs3, DefChildSpec3, Opts3} ->
            Opts4 = director_utils:concat(Opts3, Opts),
            DbgFilter =
                fun(Name2, _, Val) ->
                    try
                        sys:debug_options(Val)
                    catch
                        _:_ ->
                            error_logger:format("~p: ignoring erroneous debug options: ~p\n"
                                               ,[Name2, Val]),
                            ?DEF_DEBUG_OPTIONS
                    end
                end,
            Dbg = director_utils:option(Name, Opts4, debug, DbgFilter, ?DEF_DEBUG_OPTIONS),
            LogValidatorFilter =
                fun
                    (_, _, Val) when erlang:is_function(Val, 4) ->
                        Val;
                    (Name2, _, Val) ->
                        error_logger:format("~p: ignoring erroneous log validator: ~p\n"
                                           ,[Name2, Val]),
                        ?DEF_LOG_VALIDATOR
                end,
            LogValidator = director_utils:option(Name
                                                ,Opts4
                                                ,log_validator
                                                ,LogValidatorFilter
                                                ,?DEF_LOG_VALIDATOR),
            TabModFilter =
                fun
                    (_, _, Val) when erlang:is_atom(Val) ->
                        Val;
                    (Name2, _, Val) ->
                        error_logger:format("~p: ignoring erroneous table module: ~p\n"
                                           ,[Name2, Val]),
                        ?DEF_TABLE_MOD
                end,
            TabMod = director_utils:option(Name, Opts4, table_module, TabModFilter, ?DEF_TABLE_MOD),
            TabInitArgFilter =
                fun
                    (_, _, ?DEF_TABLE_INIT_ARG) ->
                        ?DEF_TABLE_INIT_ARG;
                    (_, _, Val) ->
                        {value, Val}
                end,
            TabInitArg = director_utils:option(Name
                                              ,Opts4
                                              ,table_init_argument
                                              ,TabInitArgFilter
                                              ,?DEF_TABLE_INIT_ARG),
            DelTabFilter =
                fun
                    (_, _, Val) when erlang:is_boolean(Val) ->
                        Val;
                    (Name2, _, Val) ->
                        error_logger:format("~p: ignoring erroneous flag for deleting table before "
                                            "termination: ~p\n"
                                           ,[Name2, Val]),
                        ?DEF_DELETE_TABLE
                    end,
            DelTab = director_utils:option(Name
                                          ,Opts4
                                          ,delete_table
                                          ,DelTabFilter
                                          ,?DEF_DELETE_TABLE),
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


start_children(Name, [#?CHILD{id=Id}=Child|Children], TabMod, TabState) ->
    case do_start_child(Name, Child, TabMod, TabState) of
        {ok, _Child, TabState2} ->
            start_children(Name, Children, TabMod, TabState2);
        {soft_error, _TabState2, already_present} ->
            _ = terminate_and_delete_children(Name, TabMod, TabState),
            {error, {duplicate_child_name, Id}}; % Like OTP/supervisor
        {soft_error, _TabState2, Rsn} ->
            _ = terminate_and_delete_children(Name, TabMod, TabState),
            {error, Rsn}; % Like OTP/supervisor
        {hard_error, Rsn} ->
            _ = terminate_and_delete_children(Name, TabMod, TabState),
            {error, Rsn}
    end;
start_children(_, [], _, TabState) ->
    {ok, TabState}.


do_start_child(Name, #?CHILD{id = Id}=Child ,TabMod, TabState) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {soft_error, TabState2, not_found} ->
            start_mfa(Name, Child, TabMod, TabState2);
        {ok, #?CHILD{pid = Pid, supervisor = Sup}} when erlang:is_pid(Pid) andalso
                                                        Sup =:= erlang:self() ->
            {soft_error, TabState, {already_started, Pid}};
        {ok, #?CHILD{supervisor = Sup}} when Sup =:= erlang:self() ->
            {soft_error, TabState, already_present};
        {ok, Child2} ->
            {ok, Child2, TabState};
%%        {soft_error, _, _}=SErr ->
%%            SErr;
        {hard_error, _}=HErr ->
            HErr
    end.


do_restart_child(Name, Id, TabMod, TabState) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {hard_error, TabState, running};
        {ok, #?CHILD{supervisor = Sup
                    ,timer_reference = Ref
                    ,restart_count = RstrtCount}=Child} when Sup =:= erlang:self() ->
            _ =
                if
                    erlang:is_reference(Ref) ->
                        erlang:cancel_timer(Ref, [{async, true}]);
                    true ->
                        ok
                end,
            Child2 = Child#?CHILD{pid = restarting
                                 ,timer_reference = undefined
                                 ,extra = undefined},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    start_mfa(Name
                             ,Child2#?CHILD{restart_count = RstrtCount + 1}
                             ,TabMod
                             ,TabState2);
                {hard_error, _}=HErr ->
                    HErr
            end;
        {ok, #?CHILD{supervisor = undefined
                    ,restart_count = RstrtCount}=Child} ->
            Child2 = Child#?CHILD{supervisor = erlang:self()},
            case director_table:change_parent(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    Child3 = Child2#?CHILD{pid = restarting
                                          ,timer_reference = undefined
                                          ,extra = undefined},
                    case director_table:insert(TabMod, TabState2, Child3) of
                        {ok, TabState3} ->
                            start_mfa(Name
                                     ,Child3#?CHILD{restart_count = RstrtCount + 1}
                                     ,TabMod
                                     ,TabState3);
                        {hard_error, _}=HErr ->
                            HErr
                    end;
                {soft_error, _, _}=SErr ->
                    SErr;
                {hard_error, _}=HErr ->
                    HErr
            end;
        {ok, _} ->
            {soft_error, TabState, not_parent};
        {soft_error, _, _}=SErr ->
            SErr;
        {hard_error, _}=HErr ->
            HErr
    end.


start_mfa(Name
         ,#?CHILD{start = {Mod, Func, Args}}=Child
         ,TabMod
         ,TabState) ->
    try erlang:apply(Mod, Func, Args) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid
                                 ,extra = undefined
                                 ,state = director_utils:progress_report(Name, Child)},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    {ok, Child2, TabState2};
                {hard_error, _}=HErr ->
                    HErr
            end;
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid
                                 ,extra = {value, Extra}
                                 ,state = director_utils:progress_report(Name, Child)},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    {ok, Child2, TabState2};
                {hard_error, _}=HErr ->
                    HErr
            end;
        ignore ->
            {ok, Child, TabState};
        {error, Rsn} ->
            {soft_error, TabState, Rsn};
        Other ->
            {soft_error, TabState, {start_bad_return, [{returned_value, Other}
                                                      ,{module, Mod}
                                                      ,{function, Func}
                                                      ,{arguments, Args}]}}
    catch
        _:Rsn ->
            {soft_error, TabState, {start_crash, [{reason, Rsn}
                                                 ,{module, Mod}
                                                 ,{function, Func}
                                                 ,{arguments, Args}
                                                 ,{stacktace, erlang:get_stacktrace()}]}}
    end.


terminate(Dbg
         ,#?STATE{module = Mod
                 ,data = Data
                 ,table_module = TabMod
                 ,table_state = TabState1
                 ,name = Name
                 ,log_validator = LogValidator
                 ,delete_table_before_terminate = Bool}
         ,Rsn1) ->
    St1 = erlang:get_stacktrace(),
    {Rsn2, TabState} =
        case terminate_and_delete_children(Name, TabMod, TabState1) of
            {ok, TabState2} ->
                {[Rsn1], TabState2};
            {hard_error, TabState2, Rsn3} ->
                {[Rsn1, Rsn3], TabState2}
        end,
    Rsn4 =
        if
            Bool ->
                case director_table:delete_table(TabMod, TabState) of
                    ok ->
                        Rsn2;
                    {hard_error, Rsn5} ->
                        Rsn2 ++ [Rsn5]
                end;
            true ->
                Rsn2
        end,
    Rsn6 =
        case Rsn4 of
            [Rsn7] ->
                Rsn7;
            _ ->
                Rsn4
        end,
    Rsn =
        case call_terminate(Mod, Data, Rsn6) of
            ok ->
                Rsn6;
            {new_error, Rsn8} ->
                Rsn8;
            {crash, Rsn8} ->
                Rsn4 ++ [Rsn8]
        end,
    case director_utils:run_log_validator(LogValidator, Name, error, Rsn, Data) of
        {none, _} ->
            ok;
        {short, _} ->
            error_logger:error_msg("** Director ~p terminating \n"
                                   "** Reason for termination == ~p\n"
                                  ,[Name, Rsn]);
        {long, _} ->
            error_logger:error_msg("** Director \"~p\" terminating \n"
                                   "** Reason for termination == ~p\n"
                                   "** Stacktrace == ~p\n"
                                  ,[Name, Rsn, St1])
    end,
    sys:print_log(Dbg),
    erlang:exit(Rsn).




call_terminate(Mod, Data, Rsn) ->
    try Mod:terminate(Rsn, Data) of
        {new_error, _}=Err ->
            Err;
        _ ->
            ok
    catch
        _:Rsn2 ->
            {crash, {terminate_crash, [{reason, Rsn2}
                                      ,{module, Mod}
                                      ,{arguments, [Rsn, Data]}
                                      ,{stacktrace, erlang:get_stacktrace()}]}}
    end.



terminate_and_delete_children(Name, TabMod, TabState) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            terminate_and_delete_children(Name, TabMod, TabState, Children);
        {hard_error, Rsn} ->
            {hard_error, TabState, Rsn}
    end.


terminate_and_delete_children(Name, TabMod, TabState, Children) ->
    TerminateAndDelete =
        fun(#?CHILD{id = Id, delete_before_terminate = Bool}, {TabState2, Rsns}) ->
            case do_terminate_child(Name, Id, TabMod, TabState2) of
                {ok, Child, TabState3} ->
                    if
                        Bool ->
                            case director_table:delete(TabMod, TabState3, Child) of
                                {ok, TabState4} ->
                                    {TabState4, Rsns};
                                {hard_error, Rsn} ->
                                    {TabState3, [Rsn|Rsns]}
                            end;
                        true ->
                            {TabState3, Rsns}
                    end;
                {soft_error, TabState3, _} ->
                    {TabState3, Rsns};
                {hard_error, Rsn} ->
                    {TabState2, [Rsn|Rsns]}
            end
        end,
    case lists:foldl(TerminateAndDelete, {TabState, []}, Children) of
        {TabState2, []} ->
            {ok, TabState2};
        {TabState2, Errs} ->
            {error, TabState2, Errs}
    end.


do_terminate_child(Name, Id_or_Pid, TabMod, TabState) ->
    SearchFunc =
        if
            erlang:is_pid(Id_or_Pid) ->
                lookup_pid;
            true ->
                lookup_id
        end,
    case director_table:SearchFunc(TabMod, TabState, Id_or_Pid) of
        {ok, #?CHILD{supervisor = Sup}=Child} when Sup =:= self() orelse Sup =:= undefined ->
            NewChState = do_terminate_child(Name, Child),
            Child2 = Child#?CHILD{pid = undefined
                                 ,extra = undefined
                                 ,timer_reference = undefined
                                 ,state = NewChState
                                 ,supervisor = undefined},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    {ok, Child2, TabState2};
                {hard_error, _}=HErr ->
                    HErr
            end;
        {ok, _} ->
            {soft_error, TabState, not_parent};
        {soft_error, _, _}=SErr ->
            SErr;
        {hard_error, _}=HErr ->
            HErr
    end.


do_terminate_child(Name
                  ,#?CHILD{pid=Pid
                          ,terminate_timeout = TerminateTimeout
                          ,state = ChState}=Child) when erlang:is_pid(Pid) ->
    BrutalKill =
        fun() ->
            erlang:exit(Pid, kill),
            receive
                {'DOWN', _Ref, process, Pid, killed} ->
                    ChState;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    director_utils:error_report(Name, shutdown_error, Reason, Child)
            end
        end,
    case monitor_child(Pid) of
        ok ->
            if
                TerminateTimeout =:= 0 ->
                    BrutalKill();
                true ->
                    receive
                        {?GEN_CALL_TAG, From, {?DELETE_RUNNING_CHILD_TAG, Pid}} ->
                            _ = reply(?DEF_DEBUG_OPTIONS, Name, From, ok),
                            ok
                    after 0 ->
                        erlang:exit(Pid, shutdown),
                        receive
                            {'DOWN', _Ref, process, Pid, shutdown} ->
                                ChState;
                            {'DOWN', _Ref, process, Pid, Reason2} ->
                                director_utils:error_report(Name
                                                           ,shutdown_error
                                                           ,Reason2
                                                           ,Child)
                        end
                    end
            end;
        {error, Reason3} ->
            director_utils:error_report(Name, shutdown_error, Reason3, Child)
    end;
do_terminate_child(_Name, #?CHILD{state = ChState}) ->
    ChState.


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


reply(Dbg, Name, {Pid, Tag}, Result) when erlang:is_pid(Pid) ->
    Pid ! {Tag, Result},
    director_utils:debug(Dbg, Name, {out, Pid, Result});
reply(Dbg, Name, Pid, Result) when erlang:is_pid(Pid) ->
    Pid ! Result,
    director_utils:debug(Dbg, Name, {out, Pid, Result});
reply(Dbg, Name, Unknown, Result) ->
    error_logger:error_msg("Director ~p could not send response ~p to unknown destination ~p"
                          ,[Name, Result, Unknown]),
    Dbg.

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
            {ok, #?CHILD{pid = Pid2}} ->
                {ok, Pid2};
            {soft_error, _, not_found} ->
                {ok, undefined};
            {hard_error, Rsn} ->
                {error, Rsn}
        end,
    case Pid of
        {ok, Pid3} ->
            case director_table:insert(TabMod, TabState, Child#?CHILD{pid = Pid3}) of
                {ok, TabState2} ->
                    change_old_children_pids(Children, TabMod, TabState2);
                {hard_error, _}=Err2 ->
                    Err2
            end;
        {error, _}=Err2 ->
            Err2
    end;
change_old_children_pids([], _TabMod, TabState) ->
    {ok, TabState}.