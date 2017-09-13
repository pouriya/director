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
%% @version  17.9.10
%% @doc
%%           Flexible, fast and powerful process supervisor and manager.
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
        ,get_count/2
        ,change_plan/3
        ,change_count/3
        ,get_default_childspec/1
        ,change_default_childspec/2
        ,change_log_validator/2
        ,start_link/4
        ,start/2
        ,start/3
        ,start/4
        ,stop/1
        ,stop/2
        ,stop/3
        ,plan_element_fun/3
        ,log_validator/2]).

%% Previous APIs with timeout argument:
-export([start_child/3
        ,restart_child/3
        ,terminate_child/3
        ,delete_child/3
        ,count_children/2
        ,which_children/2
        ,get_childspec/3
        ,get_plan/3
        ,get_count/3
        ,change_plan/4
        ,change_count/4
        ,get_pid/3
        ,get_pids/2
        ,get_default_childspec/2
        ,change_default_childspec/3
        ,change_log_validator/3]).

%% gen callback:
-export([init_it/6]).

%% sys callbacks:
-export([system_code_change/4
        ,system_continue/3
        ,system_get_state/1
        ,system_replace_state/2
        ,system_terminate/4]).

%% -------------------------------------------------------------------------------------------------
%% Types:

%% 'id' is mandatory.
%% If you don't set 'start' for default childspec, 'start' is mandatory.
%% Default values for other keys:
%%  #{plan => [fun
%%                 (_Id, _Reason, _RestartCount) ->
%%                     restart
%%             end]
%%   ,count => 5
%%   ,terminate_timeout => infinity % For supervisors
%%                         1000     % For workers
%%   ,type => worker
%%   ,modules => [Module] % Will get from value of 'start' key
%%   ,append => false}
-type childspec() :: #{'id' => id()
                      ,'start' => start()
                      ,'plan' => plan()
                      ,'count' => count()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()}.
-type  id() :: term().
-type  start() :: module() % default is {module(), start_link, []}
                | {module(), function()} % default Args is []
                | mfa().
-type  plan() :: [plan_element()] | [].
-type   plan_element() :: 'restart'
                        | {'restart', pos_integer()}
                        | 'wait'
                        | 'stop'
                        | {'stop', 'reason'}
                        | {'stop', Reason::term()}
                        | fun((Id::term(), Reason::term(), RestartCount::pos_integer()) ->
                              'restart'                  |
                              {'restart', pos_integer()} |
                              'wait'                     |
                              'stop'                     |
                              {'stop', 'reason'}         |
                              {'stop', Reason::term()})  .
-type  count() :: 'infinity' | non_neg_integer().
-type  terminate_timeout() :: 'infinity' | non_neg_integer().
-type  type() :: 'worker' | 'supervisor'.
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().

-type default_childspec() :: #{'start' => start()
                              ,'plan' => plan()
                              ,'count' => count()
                              ,'terminate_timeout' => terminate_timeout()
                              ,'type' => type()
                              ,'modules' => modules()}.

-type start_return() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type director() :: pid() | atom().

-type start_options() :: [start_option()] | [].
-type  start_option() :: debug_option()
                       | spawn_options()
                       | timeout_option()
                       | log_validator_option().
-type   debug_option() :: {'debug'
                          ,['trace'
                           |'log'
                           |'statistics'
                           |'debug']
                          |[]}.
-type   spawn_options() :: {'spawn_opt', proc_lib:spawn_option()}.
-type   timeout_option() :: {'timeout', timeout()}.
-type   log_validator_option() :: {'log_validator', log_validator()}.
-type    log_validator() :: fun((Id:: '$director' | term()
                                ,Type:: {'info', 'start'}
                                      | {'warning', term()}
                                      | {'error', term()}) ->
                                   log_mode()).
-type     log_mode() :: 'short' | 'long' | 'none'.

-export_type([childspec/0
             ,default_childspec/0
             ,start_options/0
             ,start_option/0
             ,start_return/0
             ,log_validator/0
             ,log_mode/0]).





%% -------------------------------------------------------------------------------------------------
%% Behaviour information:

-callback
init(InitArg) ->
    {'ok', Childspecs}                   |
    {'ok', Childspecs, DefaultChildspec} |
    'ignore'                             |
    {'stop', Reason}
    when
        InitArg :: term(),
        Childspecs :: [childspec()] | [],
        DefaultChildspec :: default_childspec(),
        Reason :: term().

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

%% Dependencies:
%% all macros
-include("internal/director_defaults.hrl").

%% Dependecies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

-define(STATE, director_state_record).
-record(?STATE, {name
                ,module
                ,init_argument
                ,table
                ,default_childspec
                ,log_validator
                ,table_type}).

%% -------------------------------------------------------------------------------------------------
%% supervisor-like API:

-spec
start_link(module(), InitArg::term()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Mod, InitArg) ->
    gen:start(?MODULE, link, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start_link(register_name()|module(), module()|term(), term()|start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts) when is_tuple(Name_or_Mod) ->
    gen:start(?MODULE, link, Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts, ?DEF_START_OPTIONS);
start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts) ->
    gen:start(?MODULE, link, Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts).


-spec
start_child(director(), childspec()) ->
    start_return().
%% @doc
%%      Starts a child using childspec.
%%      Reason of error may be for childspec not starting process.
%% @end
start_child(Director, ChildSpec) ->
    do_call(Director, {?START_CHILD_TAG, ChildSpec}).


-spec
restart_child(director(), id()) ->
    {'ok', pid()}                 |
    {'ok', pid(), Extra::term()} |
    {'error', Reason::'running'|'restarting'|'not_found'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.
%%      Reason of error may be for starting process.
%% @end
restart_child(Director, Id) ->
    do_call(Director, {?RESTART_CHILD_TAG, Id}).


-spec
terminate_child(director(), id() | pid()) ->
    'ok' | {'error', Reason :: 'not_found' | term()}.
%% @doc
%%      Restarts a running child using child id or child pid.
%% @end
terminate_child(Director, Id_or_Pid) ->
    do_call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}).


-spec
delete_child(director(), id()) ->
    'ok' | {'error', Reason :: 'not_found' | 'running' | term()}.
%% @doc
%%      Deletes a terminated or waited child using child id.
%% @end
delete_child(Director, Id) ->
    do_call(Director, {?DELETE_CHILD_TAG, Id}).


-spec
count_children(director()) ->
    [{'specs', non_neg_integer()}
    |{'active', non_neg_integer()}
    |{'supervisors', non_neg_integer()}
    |{'workers', non_neg_integer()}].
%% @doc
%%      Returns count of children.
%% @end
count_children(Director) ->
    do_call(Director, ?COUNT_CHILDREN_TAG).


-spec
which_children(director()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [].
%% @doc
%%      Returns information about each children.
%% @end
which_children(Director) ->
    do_call(Director, ?WHICH_CHILDREN_TAG).


-spec
get_childspec(director(), id() | pid()) ->
    {'ok', childspec()} | {'error', 'not_found'}.
%% @doc
%%      Returns childspec of child.
%% @end
get_childspec(Director, Name) ->
    do_call(Director, {?GET_CHILDSPEC_TAG, Name}).


-spec
check_childspec(childspec()) ->
    'ok' | {'error', Reason::term()}.
%% @doc
%%      Returns childspec of child.
%% @end
check_childspec(ChildSpec) ->
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
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.
%%      If 'count' is equal to crash count, new plan will never run, Because in next crash director
%%      will crash with reason {reached_max_restart_plan, ...
%% @end
change_plan(Director, Id, Plan) ->
    do_call(Director, {?CHANGE_PLAN_TAG, Id, Plan}).


-spec
change_count(director(), id(), count()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Changes the count of running or waited or terminated child.
%% @end
change_count(Director, Id, Count) ->
    do_call(Director, {?CHANGE_COUNT_TAG, Id, Count}).


-spec
get_plan(director(), id()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Returns plan of a child.
%% @end
get_plan(Director, Id) ->
    do_call(Director, {?GET_PLAN_TAG, Id}).


-spec
get_count(director(), id()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Returns count of a childspec for child.
%% @end
get_count(Director, Id) ->
    do_call(Director, {?GET_COUNT_TAG, Id}).


-spec
get_pid(director(), id()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'}.
%% @doc
%%      Returns pid of a running child.
%% @end
get_pid(Director, Id) ->
    do_call(Director, {?GET_PID_TAG, Id}).


-spec
get_pids(director()) ->
    [{id(), pid()}] | [].
%% @doc
%%      Returns list of {id, pid}s for all running children.
%% @end
get_pids(Director) ->
    do_call(Director, ?GET_PIDS_TAG).


-spec
get_default_childspec(director()) ->
    default_childspec().
%% @doc
%%      Returns director default childspec.
%% @end
get_default_childspec(Director) ->
    do_call(Director, ?GET_DEF_CHILDSPEC).


-spec
change_default_childspec(director(), default_childspec()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function. This will change all children with append => true in
%%      their next restart.
%% @end
change_default_childspec(Director, DefChildSpec) ->
    do_call(Director, {?CHANGE_DEF_CHILDSPEC, DefChildSpec}).


-spec
change_log_validator(director(), log_validator()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes validate fun.
%% @end
change_log_validator(Director, LogValidator) ->
    do_call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator}).


-spec
start_link(register_name(), module(), InitArg::term(), start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Name, Mod, InitArg, Opts) ->
    gen:start(?MODULE, link, Name, Mod, InitArg, Opts).


-spec
start(module(), InitArg::term()) ->
    start_return().
%% @doc
%%      Starts a director.
%% @end
start(Mod, InitArg) ->
    gen:start(?MODULE, nolink, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start(register_name()|module(), module()|term(), term()|start_options()) ->
    start_return().
%% @doc
%%      Starts a director.
%% @end
start(Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts) when is_tuple(Name_or_Mod) ->
    gen:start(?MODULE,nolink,Name_or_Mod,Mod_or_InitArg, InitArg_or_Opts, ?DEF_START_OPTIONS);
start(Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts) ->
    gen:start(?MODULE, nolink, Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts).


-spec
start(register_name(), module(), InitArg::term(), start_options()) ->
    start_return().
%% @doc
%%      Starts a director.
%% @end
start(Name, Mod, InitArg, Opts) ->
    gen:start(?MODULE, nolink, Name, Mod, InitArg, Opts).


-spec
stop(director()) ->
    'ok'.
%% @doc
%%      Stops a director with reason termination 'normal' and 5000 mili
%%      seconds timeout.
%% @end
stop(Director) ->
    proc_lib:stop(Director, normal, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term()) ->
    'ok'.
%% @doc
%%      Stops a director with 5000 milli-seconds timeout.
%% @end
stop(Director, Reason) ->
    proc_lib:stop(Director, Reason, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), Reason::term(), timeout()) ->
    'ok'.
%% @doc
%%      Stops a director.
%% @end
stop(Director, Reason, Timeout) ->
    proc_lib:stop(Director, Reason, Timeout).


-spec
plan_element_fun(Id::term(), Reason::term(), RestartCount::non_neg_integer()) ->
    'restart'.
%% @doc
%%      Restarting if child crashed with any reason.
%% @end
plan_element_fun(_Id, _Other, _Count) ->
    restart.


-spec
log_validator(Id::term(), Extra::term()) ->
    'short'.
%% @doc
%%     Short description for every log.
%% @end
log_validator(_Id, _Extra) ->
    short.

%% -------------------------------------------------------------------------------------------------
%% previous APIs with Timeout argument:

-spec
start_child(director(), childspec(), timeout()) ->
    start_return().
%% @doc
%%      Starts a child using childspec.
%%      Reason of error may be for childspec not starting process.
%% @end
start_child(Director, ChildSpec, Timeout) ->
    do_call(Director, {?START_CHILD_TAG, ChildSpec}, Timeout).


-spec
restart_child(director(), id(), timeout()) ->
    {'ok', pid()}                                               |
    {'ok', pid(), Extra::term()}                                |
    {'error', Reason::'running'|'restarting'|'not_found'|term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.
%%      Reason of error may be for starting process.
%% @end
restart_child(Director, Id, Timeout) ->
    do_call(Director, {?RESTART_CHILD_TAG, Id}, Timeout).


-spec
terminate_child(director(), id() | pid(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|term()}.
%% @doc
%%      Restarts a running child using child id or child pid.
%% @end
terminate_child(Director, Id_or_Pid, Timeout) ->
    do_call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}, Timeout).


-spec
delete_child(director(), id(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'running'|term()}.
%% @doc
%%      Deletes a terminated or waited child using child id.
%% @end
delete_child(Director, Id, Timeout) ->
    do_call(Director, {?DELETE_CHILD_TAG, Id}, Timeout).


-spec
count_children(director(), timeout()) ->
    [{'specs', non_neg_integer()}
    |{'active', non_neg_integer()}
    |{'supervisors', non_neg_integer()}
    |{'workers', non_neg_integer()}].
%% @doc
%%      Returns count of children.
%% @end
count_children(Director, Timeout) ->
    do_call(Director, ?COUNT_CHILDREN_TAG, Timeout).


-spec
which_children(director(), timeout()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [].
%% @doc
%%      Returns information about each children.
%% @end
which_children(Director, Timeout) ->
    do_call(Director, ?WHICH_CHILDREN_TAG, Timeout).


-spec
get_childspec(director(), id() | pid(), timeout()) ->
    {'ok', childspec()} | {'error', 'not_found'}.
%% @doc
%%      Returns childspec of child.
%% @end
get_childspec(Director, Name, Timeout) ->
    do_call(Director, {?GET_CHILDSPEC_TAG, Name}, Timeout).


-spec
get_count(director(), id(), timeout()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Returns count of a childspec for child.
%% @end
get_count(Director, Id, Timeout) ->
    do_call(Director, {?GET_COUNT_TAG, Id}, Timeout).


-spec
change_plan(director(), id(), plan(), timeout()) ->
    'ok' | {'error', not_found | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.
%%      If 'count' is equal to crash count, new plan will never run, Because in next crash director
%%      will crash with reason {reached_max_restart_plan, ...
%% @end
change_plan(Director, Id, Plan, Timeout) ->
    do_call(Director, {?CHANGE_PLAN_TAG, Id, Plan}, Timeout).


-spec
change_count(director(), id(), count(), timeout()) ->
    'ok' | {'error', 'not_found'|term()}.
%% @doc
%%      Changes the count of running or waited or terminated child.
%% @end
change_count(Director, Id, Count, Timeout) ->
    do_call(Director, {?CHANGE_COUNT_TAG, Id, Count}, Timeout).


-spec
get_plan(director(), id(), timeout()) ->
    'ok' | {'error', not_found | term()}.
%% @doc
%%      Returns plan of a child.
%% @end
get_plan(Director, Id, Timeout) ->
    do_call(Director, {?GET_PLAN_TAG, Id}, Timeout).


-spec
get_pid(director(), id(), timeout()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'}.
%% @doc
%%      Returns pid of a running child.
%% @end
get_pid(Director, Id, Timeout) ->
    do_call(Director, {?GET_PID_TAG, Id}, Timeout).


-spec
get_pids(director(), timeout()) ->
    [{id(), pid()}] | [].
%% @doc
%%      Returns list of {id, pid}s for all running children.
%% @end
get_pids(Director, Timeout) ->
    do_call(Director, ?GET_PIDS_TAG, Timeout).


-spec
get_default_childspec(director(), timeout()) ->
    default_childspec().
%% @doc
%%      Returns director default childspec.
%% @end
get_default_childspec(Director, Timeout) ->
    do_call(Director, ?GET_DEF_CHILDSPEC, Timeout).


-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function. This will change all children with append => true in
%%      next restart.
%% @end
change_default_childspec(Director, ChildSpec, Timeout) ->
    do_call(Director, {?CHANGE_DEF_CHILDSPEC, ChildSpec}, Timeout).


-spec
change_log_validator(director(), log_validator(), timeout()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes validate fun of director.
%% @end
change_log_validator(Director, LogValidator, Timeout) ->
    do_call(Director, {?CHANGE_LOG_VALIDATOR, LogValidator}, Timeout).

%% -------------------------------------------------------------------------------------------------
%% 'gen' callback:

%% @hidden
init_it(Starter, self, Name, Mod, InitArg, Opts) ->
    init_it(Starter, erlang:self(), Name, Mod, InitArg, Opts);
init_it(Starter, Parent, Name0, Mod, InitArg, Opts) ->
    Name = name(Name0),
    LogValidator = director_utils:get_log_validator(Name, Opts, ?DEF_LOG_VALIDATOR),
    TabType = director_utils:get_table_type(Name, Opts, ?DEF_TABLE_TYPE),
    Dbg = director_utils:get_debug_options(Name, Opts, ?DEF_DEBUG_OPTIONS),
    erlang:process_flag(trap_exit, true),
    case init_module(Mod, InitArg) of
        {ok, Children, DefChildSpec} ->
            Tab = director_table:create(TabType),
            case start_children(Name, Children, Tab, TabType, LogValidator) of
                {ok, Tab2} ->
                    State = #?STATE{name = Name
                                   ,module = Mod
                                   ,init_argument = InitArg
                                   ,table = Tab2
                                   ,default_childspec = DefChildSpec
                                   ,log_validator = LogValidator
                                   ,table_type = TabType},
                    proc_lib:init_ack(Starter, {ok, erlang:self()}),
%%                    exit(element(2, (catch loop(Parent, Dbg, State))));
                    loop(Parent, Dbg, State);
                {error, Reason}=Error ->
                    case director_utils:run_log_validator(LogValidator
                                                         ,?DIRECTOR_ID
                                                         ,{error, Reason}) of
                        none ->
                            ok;
                        _ ->
                            error_logger:error_msg("** Director ~p terminating in initialize state~"
                                                   "n** Reason for termination == ~p\n"
                                                  ,[Name,Reason])
                    end,
                    unregister_name(Name0),
                    proc_lib:init_ack(Starter, Error),
                    erlang:exit(Reason)
            end;
        Other ->
            Reason =
                case Other of
                    {error, Reason2} ->
                        Reason2;
                    ignore ->
                        normal
                end,
            case director_utils:run_log_validator(LogValidator, ?DIRECTOR_ID, {error, Reason}) of
                none ->
                    ok;
                _ ->
                    error_logger:error_msg("** Director ~p terminating in initialize state\n"
                                           "** Reason for termination == ~p\n"
                                          ,[Name,Reason])
            end,
            unregister_name(Name0),
            proc_lib:init_ack(Starter, Other),
            erlang:exit(Reason)
    end.

%% -------------------------------------------------------------------------------------------------
%% 'sys' callbacks:

%% @hidden
system_continue(Parent, Dbg, [State|_]) ->
    loop(Parent, Dbg, State).


%% @hidden
system_terminate(Reason, _Parent, Dbg, [State|_]) ->
    terminate(Dbg, State, Reason).


%% @hidden
system_get_state([State|_]) ->
    {ok, State}.


%% @hidden
system_replace_state(ReplaceStateFun, [State|Rest]) ->
    NewState = ReplaceStateFun(State),
    {ok, NewState, [NewState|Rest]}.


%% @hidden
system_code_change([#?STATE{module = Mod
                           ,init_argument = InitArg
                           ,table = Tab
                           ,table_type = TabType}=State
                   |Rest]
                  ,_Module
                  ,_OldVsn
                  ,_Extra) ->
    case init_module(Mod, InitArg) of
        {ok, Children, DefChildSpec} ->
            case check_duplicate_ids(Children) of
                ok ->
                    {ok, [State#?STATE{table = change_old_children_pids(Children
                                                                       ,Tab
                                                                       ,TabType)
                                      ,default_childspec = DefChildSpec}
                         |Rest]};
                {error, _}=Err ->
                    Err
            end;
        ignore ->
            {ok, [State|Rest]};
        {error, _}=Err ->
            Err
    end.

%% -------------------------------------------------------------------------------------------------
%% Process main loop and its main subcategories:

loop(Parent, Dbg, State) ->
    process_message(Parent, Dbg, State, receive
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
    {Dbg2, State2} = process_timeout(director_utils:debug(Dbg, Name, Msg), State, TimerRef, Id),
    loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, State, {cancel_timer, _TimerRef, _Result}) ->
    loop(Parent, Dbg, State);
process_message(Parent, Dbg, #?STATE{module = Mod}=State, {system, From, Msg}) ->
    sys:handle_system_msg(Msg
                         ,From
                         ,Parent
                         ,?MODULE
                         ,Dbg
                         ,[State, {supervisor, [{"Callback", Mod}]}]);
%% Catch clause:
process_message(Parent, Dbg, #?STATE{name = Name, log_validator = LogValidator}=State, Msg) ->
    case director_utils:run_log_validator(LogValidator
                                        ,?DIRECTOR_ID
                                        ,{warning, receive_unexpected_message}) of
        short ->
            error_logger:error_msg("Director ~p received an unexpected message~n", [Name]);
        none ->
            ok;
        long ->
            error_logger:error_msg("Director ~p received unexpected message: ~p~n", [Name, Msg])
    end,
    loop(Parent, Dbg, State).


process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?GET_PID_TAG, Id}) ->
    Result =
        case director_table:lookup(Table, Id, TabType) of
            not_found ->
                {error, not_found};
            #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                {ok, Pid};
            #?CHILD{pid = Other} ->
                {error, Other}
        end,
    {reply(Dbg, Name, From, Result), State};
process_request(Dbg
               ,#?STATE{table = Tab, name = Name, table_type = TabType}=State
               ,From
               ,?GET_PIDS_TAG) ->
    Pids = [{Id, Pid}
           || #?CHILD{pid = Pid, id = Id} <- director_table:tab2list(Tab, TabType)
           ,  erlang:is_pid(Pid)],
    {reply(Dbg, Name, From, Pids), State};
process_request(Dbg
               ,#?STATE{table = Tab, name = Name, table_type = TabType}=State
               ,From
               ,?COUNT_CHILDREN_TAG) ->
    Specs = director_table:count(Tab, TabType),
    Fun =
        fun(#?CHILD{pid = Pid, type = Type}, {Actives, Sups, Workers}) ->
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
            {Actives2, Sups2, Workers2}
        end,
    {Actives, Sups, Workers} = lists:foldl(Fun, {0, 0, 0}, director_table:tab2list(Tab, TabType)),
    Result = [{specs, Specs}
             ,{active, Actives}
             ,{supervisors, Sups}
             ,{workers, Workers}],
    {reply(Dbg, Name, From, Result), State};
process_request(Dbg
               ,#?STATE{table = Tab, name = Name, table_type = TabType}=State
               ,From
               ,{?DELETE_CHILD_TAG, Id}) ->
    {Result, State2} =
        case director_table:lookup(Tab, Id, TabType) of
            not_found ->
                {{error, not_found}, State};
            #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                {{error, running}, State};
            _Child ->
                Tab2 = director_table:delete(Tab, Id, TabType),
                {ok, State#?STATE{table = Tab2}}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Term}) ->
    Result =
        case director_table:lookup(Table, Term, TabType) of
            not_found ->
                if
                    erlang:is_pid(Term) ->
                        case director_table:lookup_by_pid(Table, Term, TabType) of
                            not_found ->
                                {error, not_found};
                            Child ->
                                {ok, director_utils:c2cs(Child)}
                        end;
                    true ->
                        {error, not_found}
                end;
            Child ->
                {ok, director_utils:c2cs(Child)}
        end,
    {reply(Dbg, Name, From, Result), State};
process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,log_validator = LogValidator
                       ,table_type = TabType}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    {Result, State2} =
        case do_restart_child(Name, Id, Tab, TabType, LogValidator) of
            {ok, Pid, Tab2} ->
                {{ok, Pid}, State#?STATE{table = Tab2}};
            {ok, Pid, Extra, Tab2} ->
                {{ok, Pid, Extra}, State#?STATE{table = Tab2}};
            {error, _}=Error ->
                {Error, State}
        end,
    {reply(Dbg ,Name ,From ,Result), State2};
process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,default_childspec = DefChildSpec
                       ,log_validator = LogValidator
                       ,table_type = TabType}=State
               ,From
               ,{?START_CHILD_TAG, ChildSpec}) ->
    {Result, State2} =
        case director_utils:check_childspec(ChildSpec, DefChildSpec) of
            {ok, Child} ->
                case do_start_child(Name, Child, Tab, TabType, LogValidator) of
                    {ok, Pid, Tab2} ->
                        {{ok, Pid}, State#?STATE{table = Tab2}};
                    {ok, Pid, Extra, Tab2} ->
                        {{ok, Pid, Extra}, State#?STATE{table = Tab2}};
                    {error, _}=Error ->
                        {Error, State}
                end;
            {error, _}=Error ->
                {Error, State}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,log_validator = LogValidator
                       ,table_type = TabType}=State
               ,From
               ,{?TERMINATE_CHILD_TAG, Term}) ->
    {Result, State2} =
        case do_terminate_child(Name, Term, Tab, TabType, LogValidator) of
            not_found ->
                {{error, not_found}, State};
            Tab2 ->
                {ok, State#?STATE{table = Tab2}}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{name = Name, table = Tab, table_type = TabType}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    Result = [{Id, Pid, Type, Mods}
             || #?CHILD{id = Id
                       ,pid = Pid
                       ,type = Type
                       ,modules = Mods} <- director_table:tab2list(Tab, TabType)],
    {reply(Dbg, Name, From, Result), State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?GET_PLAN_TAG, Id}) ->
    Result =
        case director_table:lookup(Table, Id, TabType) of
            not_found ->
                {error, not_found};
            #?CHILD{plan = Plan} ->
                {ok, Plan}
        end,
    {reply(Dbg, Name, From, Result), State};
process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?CHANGE_PLAN_TAG, Id, Plan}) ->
    {Result, State2} =
        case director_utils:filter_plan(Plan) of
            {ok, Plan2} ->
                case director_table:lookup(Table, Id, TabType) of
                    not_found ->
                        {{error, not_found}, State};
                    Child ->
                        PlanElemIndex =
                            if
                                Plan =:= [] ->
                                    0;
                                true ->
                                    1
                            end,
                        PlanLen = erlang:length(Plan2),
                        Child2 = Child#?CHILD{plan = Plan2
                                             ,plan_element_index = PlanElemIndex
                                             ,plan_length = PlanLen},
                        Tab2 = director_table:insert(Table, Child2, TabType),
                        {ok, State#?STATE{table = Tab2}}
                end;
            {error, _Reason}=Error ->
                {Error, State}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?CHANGE_COUNT_TAG, Id, Count0}) ->
    CountCheck =
        case director_utils:is_whole_integer(Count0) of
            true ->
                {ok, Count0};
            false when Count0 == infinity ->
                {ok, Count0};
            false ->
                error
        end,
    {Result, State2} =
        case CountCheck of
            {ok, Count} ->
                case director_table:lookup(Table, Id, TabType) of
                    not_found ->
                        {{error, not_found}, State};
                    Child ->
                        Tab2 = director_table:insert(Table,Child#?CHILD{count = Count}, TabType),
                        {ok, State#?STATE{table = Tab2}}
                end;
            error ->
                {{error, {count_format, [{count, Count0}]}}, State}
        end,
    {reply(Dbg, Name, From, Result), State2};
process_request(Dbg
               ,#?STATE{table = Table, name = Name, table_type = TabType}=State
               ,From
               ,{?GET_COUNT_TAG, Id}) ->
    Result =
        case director_table:lookup(Table, Id, TabType) of
            not_found ->
                {error, not_found};
            #?CHILD{count = Count} ->
                {ok, Count}
        end,
    {reply(Dbg, Name, From, Result), State};

process_request(Dbg
               ,#?STATE{name = Name, default_childspec = DefChildSpec}=State
               ,From
               ,?GET_DEF_CHILDSPEC) ->
    {reply(Dbg, Name, From, DefChildSpec), State};
process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,default_childspec = DefChildSpec
                       ,table_type = TabType}=State
               ,From
               ,{?CHANGE_DEF_CHILDSPEC, ChildSpec}) ->
    {State2, Result} =
        case director_utils:check_default_childspec(ChildSpec) of
            {ok, DefChildSpec2} ->
                Tab2 = director_table:separate_children(DefChildSpec, Tab, TabType),
                Tab3 = director_table:combine_children(DefChildSpec2, Tab2, TabType),
                {State#?STATE{default_childspec = DefChildSpec2, table = Tab3}, ok};
            {error, _Reason}=Error ->
                {State, Error}
        end,
    {reply(Dbg, Name, From, Result), State2};
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
%% Catch clause:
process_request(Dbg, #?STATE{name = Name, log_validator = LogValidator}=State, From, Other) ->
    case director_utils:run_log_validator(LogValidator
                                        ,?DIRECTOR_ID
                                        ,{warning, receive_unexpected_call}) of
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
            ,#?STATE{table = Tab
                    ,name = Name
                    ,table_type = TabType
                    ,log_validator = LogValidator}=State
            ,Pid
            ,Reason) ->
    case director_table:lookup_by_pid(Tab, Pid, TabType) of
        not_found ->
            loop(Parent, Dbg, State);
        Child ->
            director_utils:error_report(Name
                                       ,child_terminated
                                       ,Reason
                                       ,Child
                                       ,LogValidator),
            Child2 = Child#?CHILD{pid = undefined, extra = undefined},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            {Dbg2, State2} = handle_exit(Dbg, State#?STATE{table = Tab2}, Child2, Reason),
            loop(Parent, Dbg2, State2)
    end.


handle_exit(Dbg
           ,State
           ,#?CHILD{plan = []}=Child
           ,Reason) ->
    Reason2 = {empty_plan_child_terminated, [{reason, Reason}|director_utils:c_r2p(Child, long)]},
    terminate(Dbg, State, Reason2);
handle_exit(Dbg
           ,State
           ,#?CHILD{count = Count
                   ,count2 = _Count2 = Count}=Child
           ,Reason) ->
    Reason2 = {reached_max_restart_plan, [{reason, Reason}|director_utils:c_r2p(Child, long)]},
    terminate(Dbg, State, Reason2);
handle_exit(Dbg
           ,#?STATE{name = Name
                   ,table = Table
                   ,log_validator = LogValidator
                   ,table_type = TabType}=State
           ,#?CHILD{id = Id
                   ,plan = Plan
                   ,count2 = Count2
                   ,restart_count = ResCount
                   ,plan_element_index = PlanElemIndex
                   ,plan_length = PlanLen}=Child
           ,Reason) ->
    PlanElem = lists:nth(PlanElemIndex, Plan),
    {PlanElemIndex2, Count2_2, ResCount2} =
        if
            PlanElemIndex =:= PlanLen ->
                {1, Count2+1, ResCount + 1};
            true ->
                {PlanElemIndex+1, Count2, ResCount + 1}
        end,
    Child2 = Child#?CHILD{plan_element_index = PlanElemIndex2
                         ,count2 = Count2_2
                         ,restart_count = ResCount2},
    Tab2 = director_table:insert(Table, Child2, TabType),
    Strategy =
        case PlanElem of
            Fun when erlang:is_function(Fun) ->
                case catch Fun(Id, Reason, ResCount2) of
                    Term when erlang:is_function(Term) ->
                        {error, {fun_bad_return, [{returned_value, Term}
                                                 ,{'fun', Fun}
                                                 ,{arguments, [Id, Reason, ResCount2]}]}};
                    {'EXIT', Reason2} ->
                        {error, {fun_crash, [{reason, Reason2}
                                            ,{'fun', Fun}
                                            ,{arguments, [Reason, ResCount2]}]}};
                    Term ->
                        case director_utils:filter_plan_element(Term) of
                            {ok, Term2} ->
                                Term2;
                            {error, _Reason2} ->
                                {error, {fun_bad_return, [{returned_value, Term}
                                                         ,{'fun', Fun}
                                                         ,{arguments, [Id, Reason, ResCount2]}]}}
                        end
                end;
            _Other ->
                PlanElem
        end,
    _ = director_utils:debug(Dbg, Name, {plan, Id, Strategy}),
    case Strategy of
        restart ->
            Tab3 =
                case do_restart_child(Name, Id, Tab2, TabType, LogValidator) of
                    {error, _Reason3} ->
                        TimeRef = restart_timer(0, Id),
                        director_table:insert(Tab2
                                             ,Child2#?CHILD{pid = restarting
                                                           ,timer_reference = TimeRef}
                                             ,TabType);
                    {ok, _Pid, Tab4} ->
                        Tab4;
                    {ok, _Pid, _Extra, Tab4} ->
                        Tab4
            end,
            {Dbg, State#?STATE{table = Tab3}};
        stop ->
            terminate(Dbg, State#?STATE{table = Tab2}, Reason);
        {stop, Reason3} ->
            terminate(Dbg, State, Reason3);
        delete ->
            Tab3 = director_table:delete(Tab2, Id, TabType),
            {Dbg, State#?STATE{table = Tab3}};
        wait ->
            {Dbg, State#?STATE{table = Tab2}};
        {restart, PosInt} ->
            TimeRef = restart_timer(PosInt, Id),
            Tab3 = director_table:insert(Tab2
                                      ,Child2#?CHILD{pid = restarting
                                                     ,timer_reference = TimeRef}
                                        ,TabType),
            {Dbg, State#?STATE{table = Tab3}};
        {error, Reason3} ->
            terminate(Dbg
                     ,State#?STATE{table = Tab2}
                     ,{run_plan_element, [{reason, Reason3}
                                         ,{child_last_error_reason, Reason}
                                         |director_utils:c_r2p(Child2, long)]})
    end.


process_timeout(Dbg
               ,#?STATE{name = Name
                       ,table = Tab
                       ,log_validator = LogValidator
                       ,table_type = TabType}=State
               ,TimerRef
               ,Id) ->
    case director_table:lookup(Tab, Id, TabType) of
        not_found ->
            {Dbg, State};
        #?CHILD{timer_reference = TimerRef} ->
            case do_restart_child(Name, Id, Tab, TabType, LogValidator) of
%%                {error, not_found} ->
%%                    {Dbg, State};
                {error, Reason} ->
                    handle_exit(Dbg
                               ,State
                               ,director_table:lookup(Tab, Id, TabType)
                               ,Reason);
                {ok, _Pid, Tab2} ->
                    {Dbg, State#?STATE{table = Tab2}};
                {ok, _Pid, _Extra, Tab2} ->
                    {Dbg, State#?STATE{table = Tab2}}
            end;
        _Child ->
            {Dbg, State}
    end.

%% -------------------------------------------------------------------------------------------------
%% Other internal functions:

do_call(Name, Request) ->
    do_call(Name, Request, ?DEF_CALL_TIMEOUT).

do_call(Name, Request, Timeout) ->
    case catch gen:call(Name, ?GEN_CALL_TAG, Request, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({call_request, [{reason, Reason}
                                       ,{remote_process, Name}
                                       ,{request, Request}]})
    end.


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


init_module(Mod, InitArg) ->
    case catch Mod:init(InitArg) of
        {ok, ChildSpecs} ->
            case director_utils:check_childspecs(ChildSpecs) of
                {ok, ChildSpecs2} ->
                    {ok, ChildSpecs2, ?DEF_DEF_CHILDSPEC};
                {error, _Reason}=Error ->
                    Error
            end;
        {ok, ChildSpecs, DefChildSpec} ->
            case director_utils:check_default_childspec(DefChildSpec) of
                {ok, DefChildSpec2} ->
                    case director_utils:check_childspecs(ChildSpecs, DefChildSpec2) of
                        {ok, ChildSpecs2} ->
                            {ok, ChildSpecs2, DefChildSpec2};
                        {error, _Reason}=Error ->
                            Error
                    end;
                {error, _Reason}=Error ->
                    Error
            end;
        ignore ->
            ignore;
        {stop, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason};
        Other ->
            {error, {init_bad_return, [{returned_value, Other}
                                      ,{module, Mod}
                                      ,{function, init}
                                      ,{argument, InitArg}]}}
    end.


start_children(Name, [#?CHILD{id=Id}=Child|Children], Tab, TabType, LogValidator) ->
    case do_start_child(Name, Child, Tab, TabType, LogValidator) of
        {ok, _Pid, Tab2} ->
            start_children(Name, Children, Tab2, TabType, LogValidator);
        {error, already_present} ->
            {error, {duplicate_child_name, Id}}; % Like OTP/supervisor
        {error, {already_started, _}} ->
            {error, {duplicate_child_name, Id}}; % Like OTP/supervisor
        {error, _Reason}=Error ->
            _ = terminate_children(Name, Tab, TabType, LogValidator),
            Error
    end;
start_children(_Name, [], Tab, _TabType, _LogValidator) ->
    {ok, Tab}.


do_start_child(Name, #?CHILD{id = Id}=Child ,Tab, TabType, LogValidator) ->
    case director_table:lookup(Tab, Id, TabType) of
        not_found ->
            start_mfa(Name, Child, Tab, TabType, LogValidator);
        #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
            {error, {already_started, Pid}};
        _Child ->
            {error, already_present}
    end.


do_restart_child(Name, Id, Tab, TabType, LogValidator) ->
    case director_table:lookup(Tab, Id, TabType) of
        not_found ->
            {error, not_found};
        #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
            {error, running};
        #?CHILD{pid = restarting, timer_reference = Ref}=Child ->
            _ = erlang:cancel_timer(Ref, [{async, true}]),
            start_mfa(Name
                     ,Child#?CHILD{pid = undefined
                                  ,timer_reference = undefined}
                     ,Tab
                     ,TabType
                     ,LogValidator);
        Child ->
            start_mfa(Name, Child, Tab, TabType, LogValidator)
    end.


start_mfa(Name
         ,#?CHILD{start = {Mod, Func, Args}}=Child
         ,Tab
         ,TabType
         ,LogValidator) ->
    case catch erlang:apply(Mod, Func, Args) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid, extra = undefined},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            director_utils:progress_report(Name, Child2, LogValidator),
            {ok, Pid, Tab2};
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid, extra = {extra, Extra}},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            director_utils:progress_report(Name, Child2, LogValidator),
            {ok, Pid, Extra, Tab2};
        ignore ->
            {ok, undefined, Tab};
        {error, _Reason}=Error ->
            Error;
        {'EXIT', Reason} ->
            {error, Reason};
        Other ->
            {error, {start_bad_return, [{returned_value, Other}
                                       ,{module, Mod}
                                       ,{function, Func}
                                       ,{arguments, Args}]}}
    end.


terminate(Dbg
         ,#?STATE{table = Tab
                 ,name = Name
                 ,log_validator = LogValidator
                 ,table_type = TabType}
         ,Reason) ->
    Children = director_table:tab2list(Tab, TabType),
    _Tab2 = terminate_children(Name, Tab, TabType, LogValidator),
    case director_utils:run_log_validator(LogValidator, ?DIRECTOR_ID, {error, Reason}) of
        none ->
            ok;
        short ->
            error_logger:error_msg("** Director ~p terminating \n"
                                   "** Reason for termination == ~p\n"
                                  ,[Name,Reason]);
        long ->
            ChildrenStr = string:join([io_lib:print(director_utils:c_r2p(Child, short))
                                      || Child <- Children]
                                     ,"\n"),
            error_logger:error_msg("** Director \"~p\" terminating \n"
                                   "** Reason for termination == ~p\n"
                                   "** Children == \n~s\n"
                                  ,[Name, Reason, ChildrenStr])
    end,
    sys:print_log(Dbg),
    erlang:exit(Reason).


terminate_children(Name, Tab, TabType, LogValidator) ->
    Terminate =
        fun(Id, Tab2) ->
            case do_terminate_child(Name, Id, Tab2, TabType, LogValidator) of
                not_found ->
                    Tab2;
                Tab3 ->
                    Tab3
            end
        end,
    _Tab3 = lists:foldl(Terminate, Tab, director_table:tab2list(Tab, TabType)).


do_terminate_child(Name, Id_or_Pid, Tab, TabType, LogValidator) ->
    Search =
        case director_table:lookup(Tab, Id_or_Pid, TabType) of
            not_found ->
                director_table:lookup_by_pid(Tab, Id_or_Pid, TabType);
            Child ->
                Child
        end,
    _ =
        case Search of
            #?CHILD{pid = Pid}=Child2 when erlang:is_pid(Pid) ->
                ok = do_terminate_child(Name, Child2, LogValidator);
            #?CHILD{pid = restarting, timer_reference = Ref} ->
                _ = erlang:cancel_timer(Ref, [{async, true}]),
                ok;
            Other ->
                Other
        end,
    case Search of
        not_found ->
            not_found;
        Child3 ->
            _Tab2 = director_table:insert(Tab
                                         ,Child3#?CHILD{pid = undefined
                                                       ,extra = undefined
                                                       ,timer_reference = undefined}
                                         ,TabType)
    end.


do_terminate_child(Name, #?CHILD{pid=Pid, terminate_timeout = TerminateTimeout}=Child, LogValidator)
    when erlang:is_pid(Pid) ->
    BrutalKill =
        fun() ->
            erlang:exit(Pid, kill),
            receive
                {'DOWN', _Ref, process, Pid, killed} ->
                    ok;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    director_utils:error_report(Name, shutdown_error, Reason, Child, LogValidator),
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
                                                       ,Child
                                                       ,LogValidator),
                            ok
                    after TerminateTimeout ->
                        BrutalKill()
                    end
            end;
        {error, Reason3} ->
            director_utils:error_report(Name, shutdown_error, Reason3, Child, LogValidator),
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


change_old_children_pids([#?CHILD{id = Id}=Child|Children], Tab, TabType) ->
    Pid =
        case director_table:lookup(Tab, Id, TabType) of
            not_found ->
                undefined;
            #?CHILD{pid = Pid2} ->
                Pid2
        end,
    Tab2 = director_table:insert(Tab, Child#?CHILD{pid = Pid}, TabType),
    change_old_children_pids(Children, Tab2, TabType);
change_old_children_pids([], Tab, _TabType) ->
    Tab.
