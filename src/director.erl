%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.8.9
%% @doc
%%           Flexible, fast and powerful process supervisor.
%% @end
%% ---------------------------------------------------------------------


-module(director).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
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
        ,get_debug_mode/1
        ,change_debug_mode/2
        ,start_link/4
        ,start/2
        ,start/3
        ,start/4
        ,stop/1
        ,stop/2
        ,stop/3
        ,default_plan_element_fun/3]).





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
        ,get_debug_mode/2
        ,change_debug_mode/3]).





%% gen callback:
-export([init_it/6]).





%% sys callbacks:
-export([system_code_change/4
        ,system_continue/3
        ,system_get_state/1
        ,system_replace_state/2
        ,system_terminate/4]).





%% ---------------------------------------------------------------------
%% Types:





%% 'id' is mandatory.
%% If you don't set 'start' for default childspec, 'start' is mandatory.
%% Default values for other keys:
%%  #{plan => [fun
%%                 (_Id, normal, _RestartCount) ->
%%                     delete;
%%                 (_Id, shutdown, _RestartCount) ->
%%                     delete;
%%                 (_Id, {shutdown, _Reason}, _RestartCount) ->
%%                     delete;
%%                 (_Id, _Reason, _RestartCount) ->
%%                     restart
%%             end]
%%   ,count => 1
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
                        | fun((Id::term()
                              ,Reason::term()
                              ,RestartCount::pos_integer()) ->
                                  'restart'
                                | {'restart', pos_integer()}
                                | 'wait'
                                | 'stop'
                                | {'stop', 'reason'}
                                | {'stop', Reason::term()}).
-type  count() :: 'infinity' | non_neg_integer().
-type  terminate_timeout() :: 'infinity' | non_neg_integer().
-type  type() :: 'worker' | 'supervisor'.
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().

-type default_childspec() :: #{'start' => start()
                              ,'plan' => plan()
                              ,'count' => count()
                              ,'terminate_timeout' =>
                                   terminate_timeout()
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
                       | debug_mode_option().
-type   debug_option() :: {'debug'
                          ,['trace'
                           |'log'
                           |'statistics'
                           |'debug']
                          |[]}.
-type   spawn_options() :: {'spawn_opt', proc_lib:spawn_option()}.
-type   timeout_option() :: {'timeout', timeout()}.
-type   debug_mode_option() :: {'debug_mode', debug_mode()}.
-type    debug_mode() :: 'short' | 'long' | 'off'.





-export_type([childspec/0
             ,default_childspec/0
             ,start_options/0
             ,start_return/0
             ,debug_mode/0]).





%% ---------------------------------------------------------------------
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





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:




%% Dependencies:
%% all macros
-include("internal/director_defaults.hrl").





%% Dependecies:
%%  #?CHILD{}
-include("internal/director_child.hrl").





-record(director_state_record, {name
                               ,module
                               ,init_argument
                               ,table
                               ,default_childspec
                               ,debug_mode
                               ,table_type}).
-define(STATE, director_state_record).





%% ---------------------------------------------------------------------
%% supervisor-like API:





-spec
start_link(module(), InitArg::term()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Mod, InitArg) ->
    gen:start(?MODULE, link, Mod, InitArg, ?DEFAULT_START_OPTIONS).







-spec
start_link(register_name()|module()
          ,module()|term()
          ,term()|start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Name_or_Mod
          ,Mod_or_InitArg
          ,InitArg_or_Opts) when is_tuple(Name_or_Mod) ->
    gen:start(?MODULE
             ,link
             ,Name_or_Mod
             ,Mod_or_InitArg
             ,InitArg_or_Opts
             ,?DEFAULT_START_OPTIONS);
start_link(Name_or_Mod
          ,Mod_or_InitArg
          ,InitArg_or_Opts) ->
    gen:start(?MODULE
             ,link
             ,Name_or_Mod
             ,Mod_or_InitArg
             ,InitArg_or_Opts).







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
    {'error', Reason :: 'running'
                      | 'restarting'
                      | 'not_found'
                      | term()}.
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
    [{id(), type(), pid() | 'restarting' | 'undefined', modules()}] |
    [].
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
    case director_check:check_childspec(ChildSpec
                                       ,?DEFAULT_DEFAULT_CHILDSPEC) of
        {ok, _FixedChildSpec} ->
            ok;
        {error, _Reason}=Error ->
            Error
    end.





%% ---------------------------------------------------------------------
%% Specific API:





-spec
change_plan(director(), id(), plan()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Changes the plan of running or waited or terminated child.
%%      If 'count' is equal to crash count, new plan will never run,
%%      Because in next crash director will crash with reason
%%      {reached_max_restart_plan, ...
%% @end
change_plan(Director, Id, Plan) ->
    do_call(Director, {?CHANGE_PLAN_TAG, Id, Plan}).







-spec
change_count(director(), id(), count()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Changes the count of running or waited or terminated child.
%% @end
change_count(Director, Id, Count) ->
    do_call(Director, {?CHANGE_COUNT_TAG, Id, Count}).







-spec
get_plan(director(), id()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Returns plan of a child.
%% @end
get_plan(Director, Id) ->
    do_call(Director, {?GET_PLAN_TAG, Id}).







-spec
get_count(director(), id()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Returns count of a childspec for child.
%% @end
get_count(Director, Id) ->
    do_call(Director, {?GET_COUNT_TAG, Id}).







-spec
get_pid(director(), id()) ->
    {'ok', pid()} | {'error', 'not_found' | 'restarting' | 'undefined'}.
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
    do_call(Director, ?GET_DEFAULT_CHILDSPEC).







-spec
change_default_childspec(director(), default_childspec()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function.
%%      This will change all children with append => true in next 
%%      restart.
%% @end
change_default_childspec(Director, DefChildSpec) ->
    do_call(Director, {?CHANGE_DEFAULT_CHILDSPEC, DefChildSpec}).







-spec
get_debug_mode(director()) ->
    debug_mode().
%% @doc
%%      Gets mode of debug for director.
%% @end
get_debug_mode(Director) ->
    do_call(Director, ?GET_DEBUG_MODE_TAG).







-spec
change_debug_mode(director(), debug_mode()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes debug_mode of director.
%% @end
change_debug_mode(Director, DbgMode) ->
    do_call(Director, {?CHANGE_DEBUG_MODE_TAG, DbgMode}).







-spec
start_link(register_name()
          ,module()
          ,InitArg::term()
          ,start_options()) ->
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
    gen:start(?MODULE, nolink, Mod, InitArg, ?DEFAULT_START_OPTIONS).







-spec
start(register_name()|module()
     ,module()|term()
     ,term()|start_options()) ->
    start_return().
%% @doc
%%      Starts a director.
%% @end
start(Name_or_Mod
     ,Mod_or_InitArg
     ,InitArg_or_Opts) when is_tuple(Name_or_Mod) ->
    gen:start(?MODULE
             ,nolink
             ,Name_or_Mod
             ,Mod_or_InitArg
             ,InitArg_or_Opts
             ,?DEFAULT_START_OPTIONS);
start(Name_or_Mod, Mod_or_InitArg, InitArg_or_Opts) ->
    gen:start(?MODULE
             ,nolink
             ,Name_or_Mod
             ,Mod_or_InitArg
             ,InitArg_or_Opts).







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
    proc_lib:stop(Director, normal, ?DEFAULT_STOP_TIMEOUT).







-spec
stop(director(), Reason::term()) ->
    'ok'.
%% @doc
%%      Stops a director with 5000 mili seconds timeout.
%% @end
stop(Director, Reason) ->
    proc_lib:stop(Director, Reason, ?DEFAULT_STOP_TIMEOUT).







-spec
stop(director(), Reason::term(), timeout()) ->
    'ok'.
%% @doc
%%      Stops a director.
%% @end
stop(Director, Reason, Timeout) ->
    proc_lib:stop(Director, Reason, Timeout).







-spec
default_plan_element_fun(Id::term()
                        ,Reason::term()
                        ,RestartCount::non_neg_integer()) ->
    'delete' | 'restart'.
%% @doc
%%      Deletes child if it crashed with reasons 'normal', 'shutdown' or
%%      {'shutdown', Any} and restart it if crashed with other reasons.
%% @end
default_plan_element_fun(_Id, normal, _RestartCount) ->
    delete;
default_plan_element_fun(_Id, shutdown, _RestartCount) ->
    delete;
default_plan_element_fun(_Id, {shutdown, _Reason}, _RestartCount) ->
    delete;
default_plan_element_fun(_Id, _Other, _Count) ->
    restart.







%% ---------------------------------------------------------------------
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
    {'ok', pid()}                 |
    {'ok', pid(), Extra::term()} |
    {'error', Reason :: 'running'
    | 'restarting'
    | 'not_found'
    | term()}.
%% @doc
%%      Restarts a terminated or waited child using child id.
%%      Reason of error may be for starting process.
%% @end
restart_child(Director, Id, Timeout) ->
    do_call(Director, {?RESTART_CHILD_TAG, Id}, Timeout).







-spec
terminate_child(director(), id() | pid(), timeout()) ->
    'ok' | {'error', Reason :: 'not_found' | term()}.
%% @doc
%%      Restarts a running child using child id or child pid.
%% @end
terminate_child(Director, Id_or_Pid, Timeout) ->
    do_call(Director, {?TERMINATE_CHILD_TAG, Id_or_Pid}, Timeout).







-spec
delete_child(director(), id(), timeout()) ->
    'ok' | {'error', Reason :: 'not_found' | 'running' | term()}.
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
    [{id(), type(), pid() | 'restarting' | 'undefined', modules()}] |
    [].
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
    'ok' | {'error', 'not_found' | term()}.
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
%%      If 'count' is equal to crash count, new plan will never run,
%%      Because in next crash director will crash with reason
%%      {reached_max_restart_plan, ...
%% @end
change_plan(Director, Id, Plan, Timeout) ->
    do_call(Director, {?CHANGE_PLAN_TAG, Id, Plan}, Timeout).







-spec
change_count(director(), id(), count(), timeout()) ->
    'ok' | {'error', 'not_found' | term()}.
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
    {'ok', pid()} | {error, 'not_found' | 'restarting' | undefined}.
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
    do_call(Director, ?GET_DEFAULT_CHILDSPEC, Timeout).







-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function.
%%      This will change all children with append => true in next
%%      restart.
%% @end
change_default_childspec(Director, ChildSpec, Timeout) ->
    do_call(Director, {?CHANGE_DEFAULT_CHILDSPEC, ChildSpec}, Timeout).







-spec
get_debug_mode(director(), timeout()) ->
    debug_mode().
%% @doc
%%      Gets mode of debug for director.
%% @end
get_debug_mode(Director, Timeout) ->
    do_call(Director, ?GET_DEBUG_MODE_TAG, Timeout).







-spec
change_debug_mode(director(), debug_mode(), timeout()) ->
    'ok' | {'error', term()}.
%% @doc
%%      Changes debug_mode of director.
%% @end
change_debug_mode(Director, DbgMode, Timeout) ->
    do_call(Director, {?CHANGE_DEBUG_MODE_TAG, DbgMode}, Timeout).





%% ---------------------------------------------------------------------
%% 'gen' callback:





%% @hidden
init_it(Starter, self, Name, Mod, InitArg, Opts) ->
    init_it(Starter, erlang:self(), Name, Mod, InitArg, Opts);
init_it(Starter, Parent, Name0, Mod, InitArg, Opts) ->
    Name = name(Name0),
    DbgMode = director_check:get_debug_mode(Name
                                           ,Opts
                                           ,?DEFAULT_DEBUG_MODE),
    TabType = director_check:get_table_type(Name
                                           ,Opts
                                           ,?DEFAULT_TABLE_TYPE),
    Dbg = director_debug:debug_options(Name, Opts),
    erlang:process_flag(trap_exit, true),
    case init_module(Mod, InitArg) of
        {ok, Children, DefChildSpec} ->
            Tab = director_table:create(TabType),
            case start_children(Name
                               ,Children
                               ,Tab
                               ,TabType
                               ,DbgMode) of
                {ok, Tab2} ->
                    State = #?STATE{name = Name
                                   ,module = Mod
                                   ,init_argument = InitArg
                                   ,table = Tab2
                                   ,default_childspec = DefChildSpec
                                   ,debug_mode = DbgMode
                                   ,table_type = TabType},
                    proc_lib:init_ack(Starter, {ok, erlang:self()}),
                    exit(element(2, (catch loop(Parent, Dbg, State))));
%%                    loop(Parent, Dbg, State);
                {error, Reason}=Error ->
                    unregister_name(Name0),
                    proc_lib:init_ack(Starter, Error),
                    erlang:exit(Reason)
            end;
        ignore ->
            proc_lib:init_ack(Starter, ignore),
            erlang:exit(normal);
        {error, Reason}=Error ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, Error),
            erlang:exit(Reason)
    end.




%% ---------------------------------------------------------------------
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
                           ,table = Table}=State|Rest]
                  ,_Module
                  ,_OldVsn
                  ,_Extra) ->
    case init_module(Mod, InitArg) of
        {ok, Children1, DefChildSpec} ->
            case get_children(Children1, Table) of
                {ok, ChildrenR2} ->
                    UpdateTable =
                        fun(#?CHILD{pid = Pid, extra = Extra}=LastChild
                           ,CurrentChild) ->
                            director_table:delete(Table, LastChild),
                            NewChild =
                                CurrentChild#?CHILD{pid = Pid
                                                   ,extra = Extra},
                            director_table:insert(Table, NewChild)
                        end,
                    [UpdateTable(LastChild, CurrentChild)
                    || {LastChild, CurrentChild} <- ChildrenR2],
                    {ok
                    ,[State#?STATE{default_childspec = DefChildSpec}
                     |Rest]};
                {error, _Reason}=Error ->
                    Error
            end;
        ignore ->
            {ok, [State|Rest]};
        {error, _Reason}=Error ->
            Error
    end.







%% ---------------------------------------------------------------------
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
    {Dbg3, State2} = process_request(director_debug:debug(Dbg
                                                         ,Name
                                                         ,Msg)
                                    ,State
                                    ,From
                                    ,Request),
    loop(Parent, Dbg3, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,{'EXIT', Pid, Reason}=Msg) ->
    process_exit(Parent
                ,director_debug:debug(Dbg, Name, Msg)
                ,State
                ,Pid
                ,Reason);

process_message(Parent
               ,Dbg
               ,#?STATE{name = Name}=State
               ,{timeout, TimerRef, Id}=Msg) ->
    {Dbg2, State2} = process_timeout(director_debug:debug(Dbg
                                                         ,Name
                                                         ,Msg)
                                    ,State
                                    ,TimerRef
                                    ,Id),
    loop(Parent, Dbg2, State2);

process_message(Parent
               ,Dbg
               ,State
               ,{cancel_timer, _TimerRef, _Result}) ->
    loop(Parent, Dbg, State);

process_message(Parent
               ,Dbg
               ,#?STATE{module = Mod}=State
               ,{system, From, Msg}) ->
    sys:handle_system_msg(Msg
                         ,From
                         ,Parent
                         ,?MODULE
                         ,Dbg
                         ,[State, {supervisor, [{"Callback", Mod}]}]);
%% Catch clause:
process_message(Parent, Dbg, #?STATE{name = Name}=State, Msg) ->
    error_logger:error_msg("Director \"~p\" received unexpected message"
                           ": \"~p\"~n"
                          ,[Name, Msg]),
    loop(Parent, Dbg, State).







process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
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
               ,#?STATE{table = Tab
                       ,name = Name
                       ,table_type = TabType}=State
               ,From
               ,?GET_PIDS_TAG) ->
    Pids = [{Id, Pid} || #?CHILD{pid = Pid, id = Id}
        <- director_table:tab2list(Tab, TabType), erlang:is_pid(Pid)],
    {reply(Dbg, Name, From, Pids), State};

process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,table_type = TabType}=State
               ,From
               ,?COUNT_CHILDREN_TAG) ->
    Specs = director_table:count(Tab, TabType),
    Fun =
        fun(#?CHILD{pid = Pid, type = Type}
           ,{Actives, Sups, Workers}) ->
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
               ,#?STATE{table = Tab
                       ,name = Name
                       ,table_type = TabType}=State
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
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Term}) ->
    Result =
        case director_table:lookup(Table, Term, TabType) of
            not_found ->
                if
                    erlang:is_pid(Term) ->
                        case director_table:lookup_by_pid(Table
                                                         ,Term
                                                         ,TabType) of
                            not_found ->
                                {error, not_found};
                            Child ->
                                {ok, director_wrapper:c2cs(Child)}
                        end;
                    true ->
                        {error, not_found}
                end;
            Child ->
                {ok, director_wrapper:c2cs(Child)}
        end,
    {reply(Dbg, Name, From, Result), State};

process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,debug_mode = DbgMode
                       ,table_type = TabType}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    {Result, State2} =
        case do_restart_child(Name, Id, Tab, TabType, DbgMode) of
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
                       ,debug_mode = DbgMode
                       ,table_type = TabType}=State
               ,From
               ,{?START_CHILD_TAG, ChildSpec}) ->
    {Result, State2} =
        case director_check:check_childspec(ChildSpec, DefChildSpec) of
            {ok, Child} ->
                case do_start_child(Name
                                   ,Child
                                   ,Tab
                                   ,TabType
                                   ,DbgMode) of
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
                       ,debug_mode = DbgMode
                       ,table_type = TabType}=State
               ,From
               ,{?TERMINATE_CHILD_TAG, Term}) ->
    {Result, State2} =
        case do_terminate_child(Name, Term, Tab, TabType, DbgMode) of
            not_found ->
                {{error, not_found}, State};
            Tab2 ->
                {ok, State#?STATE{table = Tab2}}
        end,
    {reply(Dbg, Name, From, Result), State2};

process_request(Dbg
               ,#?STATE{name = Name
                       ,table = Tab
                       ,table_type = TabType}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    Result = [{Id, Pid, Type, Mods} || #?CHILD{id = Id
                                              ,pid = Pid
                                              ,type = Type
                                              ,modules = Mods}
             <- director_table:tab2list(Tab, TabType)],
    {reply(Dbg, Name, From, Result), State};

process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
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
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
               ,From
               ,{?CHANGE_PLAN_TAG, Id, Plan}) ->
    {Result, State2} =
        case director_check:filter_plan(Plan) of
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
                                             ,plan_element_index =
                                                     PlanElemIndex
                                             ,plan_length = PlanLen},
                        Tab2 = director_table:insert(Table, Child2),
                        {ok, State#?STATE{table = Tab2}}
                end;
            {error, _Reason}=Error ->
                {Error, State}
        end,
    {reply(Dbg, Name, From, Result), State2};

process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
               ,From
               ,{?CHANGE_COUNT_TAG, Id, Count0}) ->
    CountCheck =
        case director_check:is_whole_integer(Count0) of
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
                        Tab2 = director_table:insert(Table
                                                  ,Child#?CHILD{count
                            = Count}
                                                    ,TabType),
                        {ok, State#?STATE{table = Tab2}}
                end;
            error ->
                {{error, {count_format, [{count, Count0}]}}, State}
        end,
    {reply(Dbg, Name, From, Result), State2};

process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,table_type = TabType}=State
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
               ,#?STATE{name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,?GET_DEFAULT_CHILDSPEC) ->
    {reply(Dbg, Name, From, DefChildSpec), State};

process_request(Dbg
               ,#?STATE{table = Tab
                       ,name = Name
                       ,default_childspec = DefChildSpec
                       ,table_type = TabType}=State
               ,From
               ,{?CHANGE_DEFAULT_CHILDSPEC, ChildSpec}) ->
    {State2, Result} =
        case director_check:check_default_childspec(ChildSpec) of
            {ok, DefChildSpec2} ->
                Tab2 = director_table:separate_children(DefChildSpec
                                                       ,Tab
                                                       ,TabType),
                Tab3 = director_table:combine_children(DefChildSpec2
                                                      ,Tab2
                                                      ,TabType),
                {State#?STATE{default_childspec = DefChildSpec2, table = Tab3}, ok};
            {error, _Reason}=Error ->
                {State, Error}
        end,
    {reply(Dbg, Name, From, Result), State2};

process_request(Dbg
               ,#?STATE{name = Name
                       ,debug_mode = DbgMode}=State
               ,From
               ,?GET_DEBUG_MODE_TAG) ->
    {reply(Dbg, Name, From, DbgMode), State};

process_request(Dbg
               ,#?STATE{name = Name}=State
               ,From
               ,{?CHANGE_DEBUG_MODE_TAG, DbgMode}) ->
    {State2, Result} =
        if
            erlang:is_atom(DbgMode) ->
                if
                    DbgMode == off orelse
                    DbgMode == short orelse
                    DbgMode == long ->
                        {State#?STATE{debug_mode = DbgMode}, ok};
                    true ->
                        {State
                        ,{error
                         ,{debug_mode_value
                          ,[{debug_mode, DbgMode}]}}}
                end ;
            true ->
                {State
                    ,{error
                    ,{debug_mode_type
                     ,[{debug_mode, DbgMode}]}}}
        end,
    {reply(Dbg, Name, From, Result), State2};

%% Catch clause:
process_request(Dbg, #?STATE{name = Name}=State, From, Other) ->
    error_logger:error_msg("Director \"~p\" received unexpected call: \"
                           ""~p\"~n"
                          ,[Name, Other]),
    {reply(Dbg, Name, From, {error, {unknown_call, Other}}), State}.







process_exit(Parent, Dbg, State, Parent, Reason) ->
    terminate(Dbg, State, Reason);
process_exit(Parent
            ,Dbg
            ,#?STATE{table = Tab
                    ,name = Name
                    ,debug_mode = DbgMode
                    ,table_type = TabType}=State
            ,Pid
            ,Reason) ->
    case director_table:lookup_by_pid(Tab, Pid, TabType) of
        not_found ->
            loop(Parent, Dbg, State);
        Child ->
            director_debug:error_report(Name
                                       ,child_terminated
                                       ,Reason
                                       ,Child
                                       ,DbgMode),
            Child2 = Child#?CHILD{pid = undefined, extra = undefined},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            {Dbg2, State2} = handle_exit(Dbg, State#?STATE{table = Tab2}, Child2, Reason),
            loop(Parent, Dbg2, State2)
    end.






handle_exit(Dbg
           ,#?STATE{name = Name, debug_mode = DbgMode}=State
           ,#?CHILD{plan = []}=Child
           ,Reason) ->
    director_debug:error_report(Name
                               ,empty_plan_child_terminated
                               ,Reason
                               ,Child
                               ,DbgMode),
    terminate(Dbg
             ,State
             ,{empty_plan_child_terminated
              ,[{child, director_wrapper:c_r2p(Child, long)}
               ,{child_last_error_reason, Reason}]});

handle_exit(Dbg
           ,#?STATE{name = Name, debug_mode = DbgMode}=State
           ,#?CHILD{count = Count
                   ,count2 = _Count2 = Count}=Child
           ,Reason) ->
    director_debug:error_report(Name
                               ,reached_max_restart_plan
                               ,Reason
                               ,Child
                               ,DbgMode),
    terminate(Dbg
             ,State
             ,{reached_max_restart_plan
              ,[{child, director_wrapper:c_r2p(Child, long)}
               ,{child_last_error_reason, Reason}]});

handle_exit(Dbg
           ,#?STATE{name = Name
                   ,table = Table
                   ,debug_mode = DbgMode
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
                        {error
                            ,{fun_bad_return_value
                            ,[{'fun', Fun}
                             ,{arguments, [Reason, ResCount2]}
                             ,{returned_value, Term}]}};
                    {'EXIT', Reason2} ->
                        {error
                            ,{fun_run_crash
                            ,[{'fun', Fun}
                             ,{arguments, [Reason, ResCount2]}
                             ,{reason, Reason2}]}};
                    Term ->
                        case director_check:filter_plan_element(Term) of
                            {ok, Term2} ->
                                Term2;
                            {error, Reason2} ->
                                {error
                                    ,{fun_bad_return_value
                                    ,[{'fun', Fun}
                                     ,{arguments, [Id
                                                  ,Reason
                                                  ,ResCount2]}
                                     ,{return_value, Term}
                                     ,{reason, Reason2}]}}
                        end
                end;
            _Other ->
                PlanElem
        end,
    _ = director_debug:debug(Dbg, Name, {plan, Id, Strategy}),
    case Strategy of
        restart ->
            Tab3 =
                case do_restart_child(Name, Id, Tab2, TabType, DbgMode) of
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
                                                     ,timer_reference =
                                                          TimeRef}
                                        ,TabType),
            {Dbg, State#?STATE{table = Tab3}};
        {error, Reason3} ->
            terminate(Dbg
                     ,State#?STATE{table = Tab2}
                     ,{run_plan_element
                      ,[{child, director_wrapper:c_r2p(Child2, long)}
                       ,{child_last_error_reason, Reason}
                       ,{run_plan_error_reason, Reason3}]})
    end.







process_timeout(Dbg
               ,#?STATE{name = Name
                       ,table = Tab
                       ,debug_mode = DbgMode
                       ,table_type = TabType}=State
               ,TimerRef
               ,Id) ->
    case director_table:lookup(Tab, Id, TabType) of
        not_found ->
            {Dbg, State};
        #?CHILD{timer_reference = TimerRef} ->
            case do_restart_child(Name, Id, Tab, TabType, DbgMode) of
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







%% ---------------------------------------------------------------------
%% Other internal functions:





do_call(Name, Request) ->
    do_call(Name, Request, ?DEFAULT_CALL_TIMEOUT).

do_call(Name, Request, Timeout) ->
    case catch gen:call(Name, ?GEN_CALL_TAG, Request, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({call_crash, [{reason, Reason}
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
            case director_check:check_childspecs(ChildSpecs) of
                {ok, ChildSpecs2} ->
                    {ok, ChildSpecs2, ?DEFAULT_DEFAULT_CHILDSPEC};
                {error, _Reason}=Error ->
                    Error
            end;
        {ok, ChildSpecs, DefChildSpec} ->
            case director_check:check_default_childspec(DefChildSpec) of
                {ok, DefChildSpec2} ->
                    case director_check:
                         check_childspecs(ChildSpecs, DefChildSpec2) of
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
            {error
            ,{init_bad_return, [{returned_value, Other}
                               ,{module, Mod}
                               ,{function, init}
                               ,{argument, InitArg}]}}
    end.







start_children(Name
              ,[#?CHILD{id=Id}=Child|Children]
              ,Tab
              ,TabType
              ,DbgMode) ->
    case do_start_child(Name, Child, Tab, TabType, DbgMode) of
        {ok, _Pid, Tab2} ->
            start_children(Name, Children, Tab2, TabType, DbgMode);
        {error, already_present} ->
            {error, {repeated_id, [{id, Id}]}};
        {error, _Reason}=Error ->
            _ = terminate_children(Name, Tab, TabType, DbgMode),
            Error
    end;
start_children(_Name, [], Tab, _TabType, _DbgMode) ->
    {ok, Tab}.








do_start_child(Name, #?CHILD{id = Id}=Child ,Tab, TabType, DbgMode) ->
    case director_table:lookup(Tab, Id, TabType) of
        not_found ->
            start_mfa(Name, Child, Tab, TabType, DbgMode);
        #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
            {error, {already_started, Pid}};
        _Child ->
            {error, already_present}
    end.




do_restart_child(Name, Id, Tab, TabType, DbgMode) ->
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
                     ,DbgMode);
        Child ->
            start_mfa(Name, Child, Tab, TabType, DbgMode)
    end.




start_mfa(Name
         ,#?CHILD{start = {Mod, Func, Args}}=Child
         ,Tab
         ,TabType
         ,DbgMode) ->
    case catch erlang:apply(Mod, Func, Args) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid, extra = undefined},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            director_debug:progress_report(Name, Child2, DbgMode),
            {ok, Pid, Tab2};
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            Child2 = Child#?CHILD{pid = Pid
                                 ,extra = {extra, Extra}},
            Tab2 = director_table:insert(Tab, Child2, TabType),
            director_debug:progress_report(Name, Child2, DbgMode),
            {ok, Pid, Extra, Tab2};
        ignore ->
            {ok, undefined};
        {error, _Reason}=Error ->
            Error;
        {'EXIT', Reason} ->
            {error, Reason};
        Other ->
            {error
            ,{bad_start_return_value, [{returned_value, Other}
                                      ,{module, Mod}
                                      ,{function, Func}
                                      ,{arguments, Args}]}}
    end.







terminate(Dbg
         ,#?STATE{table = Tab
                 ,name = Name
                 ,debug_mode = DbgMode
                 ,table_type = TabType}=State
         ,Reason) ->
    Children = director_table:tab2list(Tab, TabType),
    _Tab2 = terminate_children(Name, Tab, TabType, DbgMode),
    error_logger:format("** Director \"~p\" terminating \n** Reason for"
                        " termination == \"~p\"~n** Children == \"~p\"~"
                        "n** State == \"~p\"~n"
                       ,[Name
                        ,Reason
                        ,[director_wrapper:c_r2p(Child, DbgMode)
                         || Child <- Children]
                        ,State]),
    sys:print_log(Dbg),
    erlang:exit(Reason).







terminate_children(Name, Tab, TabType, DbgMode) ->
    Terminate =
        fun(Id, Tab2) ->
            case do_terminate_child(Name, Id, Tab2, TabType, DbgMode) of
                not_found ->
                    Tab2;
                Tab3 ->
                    Tab3
            end
        end,
    _Tab3 = lists:foldl(Terminate
                       ,Tab
                       ,director_table:tab2list(Tab, TabType)).







do_terminate_child(Name, Id_or_Pid, Tab, TabType, DbgMode) ->
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
                ok = do_terminate_child(Name, Child2, DbgMode);
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
                                                    ,timer_reference =
                                                         undefined}
                                         ,TabType)
    end.







do_terminate_child(Name
                  ,#?CHILD{pid=Pid
                          ,terminate_timeout = TerminateTimeout}=Child
                  ,DbgMode)
    when erlang:is_pid(Pid) ->
    BrutalKill =
        fun() ->
            erlang:exit(Pid, kill),
            receive
                {'DOWN', _Ref, process, Pid, killed} ->
                    ok;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    director_debug:error_report(Name
                                               ,shutdown_error
                                               ,Reason
                                               ,Child
                                               ,DbgMode),
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
                            director_debug:error_report(Name
                                                       ,shutdown_error
                                                       ,Reason2
                                                       ,Child
                                                       ,DbgMode),
                            ok
                    after TerminateTimeout ->
                        BrutalKill()
                    end
            end;
        {error, Reason3} ->
            director_debug:error_report(Name
                                       ,shutdown_error
                                       ,Reason3
                                       ,Child
                                       ,DbgMode),
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
    director_debug:debug(Dbg, Name, {out, Pid, Result}).







get_children(Children, Table) ->
    get_children(Children, Table, []).







get_children([ChildR|Children], Table, Children2) ->
    case director_table:lookup(Table, ChildR#?CHILD.id) of
        not_found ->
            {error
            ,{child_not_found, director_wrapper:c_r2p(ChildR, long)}};
        LastChildR ->
            get_children(Children
                        ,Table
                        ,[{LastChildR, ChildR}|Children2])
    end;
get_children([], _Table, Children2) ->
    {ok, Children2}.