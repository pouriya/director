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
%% @version  17.05.04
%% @doc
%%           Flexible, fast and powerful process supervisor.
%% @end
%% ---------------------------------------------------------------------


-module(director).
-author("pouriya.jahanbakhsh@gmail.com").
-vsn("17.05.04").


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
        ,change_plan/3
        ,get_default_childspec/1
        ,change_default_childspec/2
        ,start_link/4
        ,start/2
        ,start/3
        ,start/4
        ,stop/1
        ,stop/2
        ,stop/3
        ,default_plan_element_fun/2]).





%% Previous APIs with timeout argument:
-export([start_child/3
        ,restart_child/3
        ,terminate_child/3
        ,delete_child/3
        ,count_children/2
        ,which_children/2
        ,get_childspec/3
        ,get_plan/3
        ,change_plan/4
        ,get_pid/3
        ,get_pids/2
        ,get_default_childspec/2
        ,change_default_childspec/3]).





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
%%                 (normal, _RestartCount) ->
%%                     delete;
%%                 (shutdown, _RestartCount) ->
%%                     delete;
%%                 ({shutdown, _Reason}, _RestartCount) ->
%%                     delete;
%%                 (_Reason, 5) ->
%%                     stop;
%%                 (_Reason, _RestartCount) ->
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
-type  start() :: {module(), function()} % default Args is []
                | mfa().
-type  plan() :: [plan_element()] | [].
-type   plan_element() :: 'restart'
                        | {'restart', pos_integer()}
                        | 'wait'
                        | 'stop'
                        | {'stop', Reason::term()}
                        | fun((Reason::term()
                              ,RestartCount::pos_integer()) ->
                                  'restart'
                                | {'restart', pos_integer()}
                                | 'wait'
                                | 'stop'
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
-type  start_option() :: debug() | spawn_options() | timeout_option().
-type   debug() :: {'debug'
                   ,['trace' | 'log' | 'statistics' | 'debug'] | []}.
-type   spawn_options() :: {'spawn_opt', proc_lib:spawn_option()}.
-type   timeout_option() :: {'timeout', timeout()}.





-export_type([childspec/0
             ,default_childspec/0
             ,start_options/0
             ,start_return/0]).





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





-record(director_child_record, {id
                               ,pid
                               ,plan
                               ,count
                               ,count2
                               ,restart_count
                               ,start
                               ,plan_element_index
                               ,plan_length
                               ,timer_reference
                               ,terminate_timeout
                               ,extra % for debug
                               ,modules
                               ,type
                               ,append}).
-define(CHILD, director_child_record).





-record(director_state_record, {name
                               ,module
                               ,init_argument
                               ,table
                               ,default_childspec}).
-define(STATE, director_state_record).





-define(DEFAULT_DEBUG, []).
-define(DEFAULT_PLAN_ELEMENT, fun director:default_plan_element_fun/2).
-define(DEFAULT_PLAN, [?DEFAULT_PLAN_ELEMENT]).
-define(DEFAULT_COUNT, 1).
-define(DEFAULT_TYPE, worker).
-define(DEFAULT_APPEND, false).
-define(DEFAULT_WORKER_TERMINATE_TIMEOUT, 1000).
-define(DEFAULT_SUPERVISOR_TERMINATE_TIMEOUT, infinity).






-define(DEFAULT_DEFAULT_CHILDSPEC_PLAN, []).
-define(DEFAULT_DEFAULT_CHILDSPEC_COUNT, 0).
-define(DEFAULT_DEFAULT_CHILDSPEC_TERMINATE_TIMEOUT, 0).
-define(DEFAULT_DEFAULT_CHILDSPEC_MODULES, []).
-define(DEFAULT_DEFAULT_CHILDSPEC
       ,#{plan => ?DEFAULT_DEFAULT_CHILDSPEC_PLAN
         ,count => ?DEFAULT_DEFAULT_CHILDSPEC_COUNT
         ,terminate_timeout =>
          ?DEFAULT_DEFAULT_CHILDSPEC_TERMINATE_TIMEOUT
         ,modules => ?DEFAULT_DEFAULT_CHILDSPEC_MODULES}).





-define(DEFAULT_START_OPTIONS, []).
-define(DEFAULT_STOP_TIMEOUT, 5000).
-define(DEFAULT_CALL_TIMEOUT, 5000).
-define(GEN_CALL_TAG, '$gen_call').
-define(COUNT_CHILDREN_TAG, 'count_children').
-define(DELETE_CHILD_TAG, 'delete_child').
-define(GET_CHILDSPEC_TAG, 'get_childspec').
-define(RESTART_CHILD_TAG, 'restart_child').
-define(START_CHILD_TAG, 'start_child').
-define(TERMINATE_CHILD_TAG, 'terminate_child').
-define(WHICH_CHILDREN_TAG, 'which_children').
-define(GET_PID_TAG, 'get_pid').
-define(GET_PIDS_TAG, 'get_pids').
-define(CHANGE_PLAN_TAG, 'change_plan').
-define(GET_PLAN_TAG, 'get_plan').
-define(GET_DEFAULT_CHILDSPEC, 'get_default_childspec').
-define(CHANGE_DEFAULT_CHILDSPEC, 'change_default_childspec').






-define(ETS_TABLE_OPTIONS, [{keypos,2}]).
-define(trapping_exits, erlang:process_flag(trap_exit, true)).





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
    case check_and_fix_childspec(ChildSpec
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
get_plan(director(), id()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @doc
%%      Returns plan of a child.
%% @end
get_plan(Director, Id) ->
    do_call(Director, {?GET_PLAN_TAG, Id}).







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
    {'ok', default_childspec()}.
%% @doc
%%      Returns director default childspec.
%% @end
get_default_childspec(Director) ->
    do_call(Director, ?GET_DEFAULT_CHILDSPEC).







-spec
change_default_childspec(director(), default_childspec()) ->
    {'ok', default_childspec()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function.
%%      This will change all children with append => true in next 
%%      restart.
%% @end
change_default_childspec(Director, DefChildSpec) ->
    do_call(Director, {?CHANGE_DEFAULT_CHILDSPEC, DefChildSpec}).







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
default_plan_element_fun(Reason::term()
                        ,RestartCount::non_neg_integer()) ->
    'restart'                      |
    'stop'                         |
    'wait'                         |
    'delete'                       |
    {'restart', MS::pos_integer()} |
    {'stop', Reason2::term()}.
%% @doc
%%      Deletes child if it crashed with reasons 'normal', 'shutdown' or
%%      {'shutdown', Any} and restart it if crashed other reasons.
%% @end
default_plan_element_fun(normal, _RestartCount) ->
    delete;
default_plan_element_fun(shutdown, _RestartCount) ->
    delete;
default_plan_element_fun({shutdown, normal}, _RestartCount) ->
    delete;
default_plan_element_fun(_Other, _Count) ->
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
    {'ok', default_childspec()}.
%% @doc
%%      Returns director default childspec.
%% @end
get_default_childspec(Director, Timeout) ->
    do_call(Director, ?GET_DEFAULT_CHILDSPEC, Timeout).







-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()}.
%% @doc
%%      Changes director default childspec.
%%      Be careful about using this function.
%%      This will change all children with append => true in next
%%      restart.
%% @end
change_default_childspec(Director, ChildSpec, Timeout) ->
    do_call(Director, {?CHANGE_DEFAULT_CHILDSPEC, ChildSpec}, Timeout).





%% ---------------------------------------------------------------------
%% gen callback:





%% @hidden
init_it(Starter, self, Name, Mod, InitArg, Opts) ->
    init_it(Starter, erlang:self(), Name, Mod, InitArg, Opts);
init_it(Starter, Parent, Name0, Mod, InitArg, Opts) ->
    Name = name(Name0),
    Dbg = debug_options(Name, Opts),
    ?trapping_exits,
    case init_module(Mod, InitArg) of
        {ok, Children, DefChildSpec} ->
            TableName =
                erlang:list_to_atom(erlang:pid_to_list(erlang:self())),
            Table = ets:new(TableName, ?ETS_TABLE_OPTIONS),
            case start_children(Children, Table, Name) of
                ok ->
                    State = #?STATE{name = Name
                                   ,module = Mod
                                   ,init_argument = InitArg
                                   ,table = Table
                                   ,default_childspec = DefChildSpec},
                    proc_lib:init_ack(Starter, {ok, erlang:self()}),
                    loop(Parent, Dbg, State);
                {error, Reason}=Error ->
                    unregister_name(Name0),
                    proc_lib:init_ack(Starter, Error),
                    erlang:exit(Reason)
            end;
        {error, Reason}=Error ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, Error),
            erlang:exit(Reason)
    end.




%% ---------------------------------------------------------------------
%% sys callbacks:





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
system_replace_state(ReplaceStateFun, [State|_]) ->
    NewState = ReplaceStateFun(State),
    {ok, NewState, [NewState]}.








%% @hidden
system_code_change([#?STATE{module = Mod
                           ,init_argument = InitArg
                           ,table = Table}=State|_]
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
                            ok = do_delete_child(LastChild, Table),
                            NewChild =
                                CurrentChild#?CHILD{pid = Pid
                                                   ,extra = Extra},
                            ok = insert_or_update_child(NewChild, Table)
                        end,
                    [UpdateTable(LastChild, CurrentChild)
                    || {LastChild, CurrentChild} <- ChildrenR2],
                    {ok
                    ,[State#?STATE{default_childspec = DefChildSpec}]};
                {error, _Reason}=Error ->
                    Error
            end;
        ignore ->
            {ok, [State]};
        {error, _Reason}=Error ->
            Error
    end.







%% ---------------------------------------------------------------------
%% Process main loop and its main subcategories:





loop(Parent, Dbg, State) ->
    Msg =
        receive
            Msg0 ->
                Msg0
        end,
    process_message(Parent, Dbg, State, Msg).






process_message(Parent
               ,Dbg
               ,#?STATE{name = Name}=State
               ,{?GEN_CALL_TAG, From, Request}) ->
    Dbg2 = debug(Dbg, Name, {in, {call, From, Request}}),
    {Dbg3, State2} = process_request(Dbg2, State, From, Request),
    loop(Parent, Dbg3, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,{'EXIT', Pid, Reason}) ->
    Dbg2 = debug(Dbg, Name, {in, {exit, Pid, Reason}}),
    process_exit(Parent, Dbg2, State, Pid, Reason);

process_message(Parent
               ,Dbg
               ,#?STATE{name = Name}=State
               ,{timeout, TimerRef, Id}) ->
    Dbg2 = debug(Dbg, Name, {timeout, TimerRef, Id}),
    {Dbg3, State2} = process_timeout(Dbg2, State, TimerRef, Id),
    loop(Parent, Dbg3, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{module = Mod}=State
               ,{system, From, Msg}) ->
    NewState = [State,{supervisor, [{"Callback", Mod}]}],
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, NewState);
%% Catch clause:
process_message(Parent, Dbg, #?STATE{name = Name}=State, Msg) ->
    error_logger:error_msg("Director \"~p\" received unexpected message"
                           ": \"~p\"~n"
                          ,[Name, Msg]),
    loop(Parent, Dbg, State).







process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,?COUNT_CHILDREN_TAG) ->
    Specs = ets:info(Table, size),
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
    {Actives, Sups, Workers} = ets:foldl(Fun, {0, 0, 0}, Table),
    Result = [{specs, Specs}
             ,{active, Actives}
             ,{supervisors, Sups}
             ,{workers, Workers}],
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?DELETE_CHILD_TAG, Id}) ->
    Result =
        case lookup_child_using_its_id(Id, Table) of
            not_found ->
                {error, not_found};
            #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                {error, running};
            Child ->
                ok = do_delete_child(Child, Table)
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Term}) ->
    Result =
        case lookup_child_using_its_id(Term, Table) of
            not_found ->
                if
                    erlang:is_pid(Term) ->
                        case lookup_child_using_its_pid(Term, Table) of
                            not_found ->
                                {error, not_found};
                            Child ->
                                {ok, child_record_to_childspec(Child)}
                        end;
                    true ->
                        {error, not_found}
                end;
            Child ->
                {ok, child_record_to_childspec(Child)}
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    Result =
        case lookup_child_using_its_id(Id, Table) of
            not_found ->
                {error, not_found};
            #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                {error, running};
            Child ->
                case do_start_child(Child, Table, Name, normal) of
                    {ok, restarting} ->
                        {error, restarting};
                    Other ->
                        Other
                end
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,{?START_CHILD_TAG, ChildSpec}) ->
    Result =
        case check_and_fix_childspec(ChildSpec, DefChildSpec) of
            {ok, #?CHILD{id = Id}=Child} ->
                case lookup_child_using_its_id(Id, Table) of
                    not_found ->
                        do_start_child(Child, Table, Name, normal);
                    #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                        {error, {already_started, Pid}};
                    _Child ->
                        {error, already_present}
                end;
            {error, _Reason}=Error ->
                Error
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?TERMINATE_CHILD_TAG, Term}) ->
    Result =
        case lookup_child_using_its_id(Term, Table) of
            not_found ->
                if
                    erlang:is_pid(Term) ->
                        case lookup_child_using_its_pid(Term, Table) of
                            not_found ->
                                {error, not_found};
                            Child ->
                                ok = do_terminate_child(Table
                                                       ,Name
                                                       ,Child)
                        end;
                    true ->
                        {error, not_found}
                end;
            Child ->
                ok = do_terminate_child(Table, Name, Child)
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{name = Name, table = Table}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    Result = [{Id, Pid, Type, Mods} || #?CHILD{id = Id
                                              ,pid = Pid
                                              ,type = Type
                                              ,modules = Mods}
             <- ets:tab2list(Table)],
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?GET_PLAN_TAG, Id}) ->
    Result =
        case lookup_child_using_its_id(Id, Table) of
            not_found ->
                {error, not_found};
            #?CHILD{plan = Plan} ->
                {ok, Plan}
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?CHANGE_PLAN_TAG, Id, Plan}) ->
    Result =
        case filter_plan(Plan) of
            {ok, Plan2} ->
                case lookup_child_using_its_id(Id, Table) of
                    not_found ->
                        {error, not_found};
                    Child1 ->
                        PlanElemIndex =
                            if
                                Plan =:= [] ->
                                    0;
                                true ->
                                    1
                            end,
                        PlanLen = erlang:length(Plan2),
                        Child2 = Child1#?CHILD{plan = Plan2
                                              ,plan_element_index =
                                                     PlanElemIndex
                                              ,plan_length = PlanLen
                                              ,count2 = 0},
                        ok = insert_or_update_child(Child2, Table)
                end;
            {error, _Reason}=Error ->
                Error
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,{?GET_PID_TAG, Id}) ->
    Result =
        case lookup_child_using_its_id(Id, Table) of
            not_found ->
                {error, not_found};
            #?CHILD{pid = Pid} when erlang:is_pid(Pid) ->
                {ok, Pid};
            #?CHILD{pid = Other} ->
                {error, Other}
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table, name = Name}=State
               ,From
               ,?GET_PIDS_TAG) ->
    Pids = [{Id, Pid} || #?CHILD{pid = Pid, id = Id}
           <- ets:tab2list(Table), erlang:is_pid(Pid)],
    Dbg2 = reply(Dbg, Name, From, Pids),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,?GET_DEFAULT_CHILDSPEC) ->
    Dbg2 = reply(Dbg, Name, From, {ok, DefChildSpec}),
    {Dbg2, State};

process_request(Dbg
               ,#?STATE{table = Table
                       ,name = Name
                       ,default_childspec = DefChildSpec}=State
               ,From
               ,{?CHANGE_DEFAULT_CHILDSPEC, ChildSpec}) ->
    {State2, Result} =
        case check_and_fix_default_childspec(ChildSpec) of
            {ok, DefChildSpec2} ->
                AppendTrueChildren2 =
                    [seprate_children(child_record_to_childspec(Child)
                                     ,DefChildSpec)
                    || Child <- lookup_append_true_children(Table)],
                Combine =
                    fun
                        (Child) ->
                            Child2 = combine_children(Child
                                                     ,DefChildSpec2
                                                     ,true),
                            Child3 = childspec_to_child(Child2),
                            ok = insert_or_update_child(Child3, Table)
                    end,
                [Combine(Child) || Child <- AppendTrueChildren2],
                {State#?STATE{default_childspec = DefChildSpec2}, ok};
            {error, _Reason}=Error ->
                {State, Error}
        end,
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State2};

%% Catch clause:
process_request(Dbg, #?STATE{name = Name}=State, From, Other) ->
    error_logger:error_msg("Director \"~p\" received unexpected call: \"
                           ""~p\"~n"
                          ,[Name, Other]),
    Result = {error, {unknown_call, Other}},
    Dbg2 = reply(Dbg, Name, From, Result),
    {Dbg2, State}.







process_exit(Parent, Dbg, State, Parent, Reason) ->
    terminate(Dbg, State, Reason);
process_exit(Parent
            ,Dbg
            ,#?STATE{table = Table, name = Name}=State
            ,Pid
            ,Reason) ->
    case lookup_child_using_its_pid(Pid, Table) of
        not_found ->
            loop(Parent, Dbg, State);
        Child1 ->
            error_report(Name
                        ,child_terminated
                        ,Reason
                        ,child_record_to_proplist(Child1)),
            Child2 = Child1#?CHILD{pid = undefined, extra = undefined},
            ok = insert_or_update_child(Child2, Table),
            {Dbg2, State2} = handle_exit(Dbg, State, Reason, Child2),
            loop(Parent, Dbg2, State2)
    end.







handle_exit(Dbg
           ,#?STATE{name = Name}=State
           ,Reason
           ,#?CHILD{plan = Plan
                   ,count = Count
                   ,count2 = Count2}=ChildR) ->
    if
        Count =:= Count2 -> % Count =/= infinity
            error_report(Name
                        ,reached_max_restart_plan
                        ,Reason
                        ,child_record_to_proplist(ChildR)),
            terminate(Dbg
                     ,State
                     ,{reached_max_restart_plan
                      ,[{child, child_record_to_proplist(ChildR)}
                       ,{child_last_error_reason, Reason}]});
        Plan =:= [] ->
            terminate(Dbg
                     ,State
                     ,{empty_plan_child_terminated
                      ,[{child, child_record_to_proplist(ChildR)}
                       ,{child_last_error_reason, Reason}]});
        true -> % and (Count =:= infinity or Count < Count2)
            run_plan_element(Dbg, State, Reason, ChildR)
    end.







run_plan_element(Dbg
                ,#?STATE{name = Name, table = Table}=State
                ,Reason
                ,#?CHILD{plan = Plan
                        ,count2 = Count2
                        ,restart_count = ResCount
                        ,plan_element_index = PlanElemIndex
                        ,plan_length = PlanLen}=Child1) ->
    PlanElem = lists:nth(PlanElemIndex, Plan),
    {PlanElemIndex2, Count2_2} =
        if
            PlanElemIndex =:= PlanLen ->
                      {1, Count2+1};
            true ->
                {PlanElemIndex+1, Count2}
        end,
    ResCount2 = ResCount + 1,
    Child2 = Child1#?CHILD{plan_element_index = PlanElemIndex2
                          ,count2 = Count2_2
                          ,restart_count = ResCount2},
    Strategy =
        case PlanElem of
            Fun when erlang:is_function(Fun) ->
                case catch Fun(Reason, ResCount2) of
                    Term when erlang:is_function(Term) ->
                        {error
                        ,{fun_bad_return_value
                         ,[{'fun', Fun}
                          ,{arguments, [Reason, ResCount2]}
                          ,{return_value, Term}]}};
                    {'EXIT', Reason2} ->
                        {error
                        ,{fun_run_crash
                         ,[{'fun', Fun}
                          ,{arguments, [Reason, ResCount2]}
                          ,{reason, Reason2}]}};
                    Term ->
                        case filter_plan_element(Term) of
                            {ok, Term2} ->
                                Term2;
                            {error, Reason2} ->
                                {error
                                ,{fun_bad_return_value
                                 ,[{'fun', Fun}
                                  ,{arguments, [Reason, ResCount2]}
                                  ,{return_value, Term}
                                  ,{reason, Reason2}]}}
                        end
                end;
            _Other ->
                PlanElem
        end,
    case Strategy of
        restart ->
            case do_start_child(Child2, Table, Name, normal) of
                {error, Reason3} ->
                    run_plan_element(Dbg, State, Reason3, Child2);
                _Other2 ->
                    {Dbg, State}
            end;
        stop ->
            terminate(Dbg
                     ,State
                     ,{stop, [{child, child_record_to_proplist(Child2)}
                             ,{child_last_error_reason, Reason}]});
        {stop, Reason3} ->
            terminate(Dbg, State, Reason3);
        delete ->
            ok = do_delete_child(Child1, Table),
            {Dbg, State};
        wait ->
            ok = insert_or_update_child(Child2, Table),
            {Dbg, State};
        {restart, PosInt} ->
            TimeRef = restart_timer(PosInt, Child2#?CHILD.id),
            ok = insert_or_update_child(Child2#?CHILD{pid = restarting
                                                     ,timer_reference =
                                                           TimeRef}
                                       ,Table),
            {Dbg, State};
        {error, Reason3} ->
            terminate(Dbg
                     ,State
                     ,{run_plan_element
                      ,[{child, child_record_to_proplist(Child2)}
                       ,{child_last_error_reason, Reason}
                       ,{run_plan_error_reason, Reason3}]})
    end.







process_timeout(Dbg
               ,#?STATE{name = Name, table = Table}=State
               ,TimerRef
               ,Id) ->
    case lookup_child_using_its_id(Id, Table) of
        not_found ->
            {Dbg, State};
        #?CHILD{timer_reference = TimerRef}=Child ->
            case do_start_child(Child, Table, Name, normal) of
                {error, Reason} ->
                    handle_exit(Dbg, State, Reason, Child);
                _Other ->
                    {Dbg, State}
            end;
        _Child ->
            {Dbg, State}
    end.







%% ---------------------------------------------------------------------
%% Other internal functions:





name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.







debug_options(Name, Opts) ->
    case proplists:lookup(debug, Opts) of
        {_,Options} ->
            try sys:debug_options(Options)
            catch _:_ ->
                error_logger:format(
                    "~p: ignoring erroneous debug options - ~p~n",
                    [Name,Options]),
                ?DEFAULT_DEBUG
            end;
        none ->
            ?DEFAULT_DEBUG
    end.







unregister_name({local,Name}) ->
    try unregister(Name) of
        _ -> ok
    catch
        _:_ -> ok
    end;
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name),
    ok;
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name),
    ok;
unregister_name(Pid) when is_pid(Pid) ->
    ok.







init_module(Mod, InitArg) ->
    case catch Mod:init(InitArg) of
        {ok, Childspecs} ->
            case check_and_fix_childspecs(Childspecs
                                         ,?DEFAULT_DEFAULT_CHILDSPEC) of
                {ok, ChildSpecs2} ->
                    {ok, ChildSpecs2, ?DEFAULT_DEFAULT_CHILDSPEC};
                {error, _Reason}=Error ->
                    Error
            end;
        {ok, ChildSpecs, DefChildSpec} ->
            case check_and_fix_default_childspec(DefChildSpec) of
                {ok, DefChildSpec2} ->
                    case check_and_fix_childspecs(ChildSpecs
                                                 ,DefChildSpec2) of
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
            ,{bad_return, [{module, Mod}
                          ,{function, init}
                          ,{arguments, [InitArg]}
                          ,{returne_value, Other}]}}
    end.







do_call(Name, Request) ->
    do_call(Name, Request, ?DEFAULT_CALL_TIMEOUT).

do_call(Name, Request, Timeout) ->
    case catch gen:call(Name, ?GEN_CALL_TAG, Request, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            Request2 =
                case Request of
                    {?GEN_CALL_TAG, Request3} ->
                        Request3;
                    _Other ->
                        Request
                end,
            erlang:exit({call_crash, [{remote_process, Name}
                                     ,{request, Request2}
                                     ,{reason, Reason}]})
    end.







child_record_to_childspec(#?CHILD{id = Id
                                 ,start = Start
                                 ,plan = Plan
                                 ,count = Count
                                 ,terminate_timeout = TerminateTimeout
                                 ,modules = Modules
                                 ,type = Type
                                 ,append = Append}) ->
    #{id => Id
    ,start => Start
    ,plan => Plan
    ,count => Count
    ,terminate_timeout => TerminateTimeout
    ,modules => Modules
    ,type => Type
    ,append => Append}.







child_record_to_proplist(#?CHILD{pid = Pid
                                ,id = Id
                                ,plan = Plan
                                ,count = Count
                                ,count2 = Count2
                                ,restart_count = ResCount
                                ,start = Start
                                ,plan_element_index = PlanElemIndex
                                ,plan_length = PlanLen
                                ,timer_reference = TimerRef
                                ,terminate_timeout = TerminateTimeout
                                ,extra = Extra
                                ,modules = Mods
                                ,type = Type
                                ,append = Append}) ->
    [{id, Id}
    ,{pid, Pid}
    ,{plan, Plan}
    ,{count, Count}
    ,{count2, Count2}
    ,{restart_count, ResCount}
    ,{mfargs, Start} % mfargs for lager : )
    ,{plan_element_index, PlanElemIndex}
    ,{plan_length, PlanLen}
    ,{timer_reference, TimerRef}
    ,{terminate_timeout, TerminateTimeout}
    ,{extra, Extra}
    ,{modules, Mods}
    ,{type, Type}
    ,{append, Append}].







lookup_child_using_its_id(Id, Table) ->
    case ets:lookup(Table, Id) of
        [Child] ->
            Child;
        [] ->
            not_found
    end.







lookup_child_using_its_pid(Pid, Table) ->
    case ets:match_object(Table
                         ,#?CHILD{id = '_'
                                 ,pid = Pid
                                 ,plan = '_'
                                 ,count = '_'
                                 ,count2 = '_'
                                 ,restart_count = '_'
                                 ,start = '_'
                                 ,plan_element_index = '_'
                                 ,plan_length = '_'
                                 ,timer_reference = '_'
                                 ,terminate_timeout = '_'
                                 ,extra = '_'
                                 ,modules = '_'
                                 ,type = '_'
                                 ,append = '_'}) of
        [Child] ->
            Child;
        [] ->
            not_found
    end.







lookup_append_true_children(Table) ->
    ets:match_object(Table
                    ,#?CHILD{id = '_'
                            ,pid = '_'
                            ,plan = '_'
                            ,count = '_'
                            ,count2 = '_'
                            ,restart_count = '_'
                            ,start = '_'
                            ,plan_element_index = '_'
                            ,plan_length = '_'
                            ,timer_reference = '_'
                            ,terminate_timeout = '_'
                            ,extra = '_'
                            ,modules = '_'
                            ,type = '_'
                            ,append = true}).







insert_or_update_child(Child, Table) ->
    true = ets:insert(Table, Child),
    ok.







do_delete_child(Child, Table) ->
    true = ets:delete_object(Table, Child),
    ok.







terminate(Dbg, #?STATE{table = Table, name = Name}=State, Reason) ->
    Children = ets:tab2list(Table),
    terminate_children(Table, Name),
    true = ets:delete_all_objects(Table),
    true = ets:delete(Table),
    error_logger:format("** Director \"~p\" terminating \n** Reason for"
                        " termination == \"~p\"~n** Children == \"~p\"~"
                        "n** State == \"~p\"~n"
                       ,[Name
                        ,Reason
                        ,[child_record_to_proplist(Child)
                         || Child <- Children]
                        ,State]),
    sys:print_log(Dbg),
    erlang:exit(Reason).







terminate_children(Table, Name) ->
    [do_terminate_child(Table, Name, Child)
        || Child <- ets:tab2list(Table)],
    ok.






do_terminate_child(Table
                  ,Name
                  ,#?CHILD{pid=Pid
                          ,terminate_timeout = TerminateTimeout}=ChildR)
    when erlang:is_pid(Pid) ->
    ok = insert_or_update_child(ChildR#?CHILD{pid = undefined
                                             ,extra = undefined}
                               ,Table),
    ChildPL = child_record_to_proplist(ChildR),
    BrutalKill =
        fun() ->
            erlang:exit(Pid, kill),
            receive
                {'DOWN', _Ref, process, Pid, killed} ->
                    ok;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    error_report(Name
                                ,shutdown_error
                                ,Reason
                                ,ChildPL),
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
                            error_report(Name
                                        ,shutdown_error
                                        ,Reason2
                                        ,ChildPL),
                            ok
                    after TerminateTimeout ->
                        BrutalKill()
                    end
            end;
        {error, Reason3} ->
            error_report(Name
                        ,shutdown_error
                        ,Reason3
                        ,ChildPL),
            ok
    end;
do_terminate_child(Table
                  ,_Name
                  ,#?CHILD{pid = restarting
                          ,timer_reference = TimerRef}=Child) ->
    erlang:cancel_timer(TimerRef),
    ok = insert_or_update_child(Child#?CHILD{pid = undefined
                                            ,extra = undefined}
                               ,Table);
do_terminate_child(_Table, _Name, _Child) ->
    ok.







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







check_and_fix_childspecs([], _DefChildSpec) ->
    {ok, []};
check_and_fix_childspecs(Term, DefChildSpec) ->
    check_and_fix_childspecs(Term, DefChildSpec, []).







check_and_fix_childspecs([Elem|Elems], DefChildSpec, Children) ->
    case check_and_fix_childspec(Elem, DefChildSpec) of
        {ok, ChildSpec} ->
            check_and_fix_childspecs(Elems
                                    ,DefChildSpec
                                    ,[ChildSpec|Children]);
        {error, _Reason}=Error ->
            Error
    end;
check_and_fix_childspecs([], _DefChildSpec, Children) ->
    {ok, lists:reverse(Children)};
check_and_fix_childspecs(Other, _DefChildSpec, _Children) ->
    {error, {childspecs_type, [{childspecs, Other}]}}.






childspec_to_child(#{id := Id
                   ,plan := Plan
                   ,count := Count
                   ,start := Start
                   ,terminate_timeout := TerminateTimeout
                   ,modules := Mods
                   ,type := Type
                   ,append := Append}) ->
    PlanLen = erlang:length(Plan),
    PlanElemIndex =
        if
            PlanLen =:= 0 ->
                0;
            true ->
                1
        end,
    #?CHILD{id = Id
           ,pid = undefined
           ,plan = Plan
           ,count = Count
           ,count2 = 0
           ,restart_count = 0
           ,start = Start
           ,plan_element_index = PlanElemIndex
           ,plan_length = PlanLen
           ,timer_reference = undefined
           ,terminate_timeout = TerminateTimeout
           ,extra = undeined
           ,modules = Mods
           ,type = Type
           ,append = Append}.








reply(Dbg, Name, {Pid, Tag}=_From, Result) ->
    Pid ! {Tag, Result},
    debug(Dbg, Name, {out, Pid, Result}).







get_children(Children, Table) ->
    get_children(Children, Table, []).







get_children([ChildR|Children], Table, Children2) ->
    case lookup_child_using_its_id(ChildR#?CHILD.id, Table) of
        not_found ->
            {error
            ,{child_not_found, child_record_to_proplist(ChildR)}};
        LastChildR ->
            get_children(Children
                        ,Table
                        ,[{LastChildR, ChildR}|Children2])
    end;
get_children([], _Table, Children2) ->
    {ok, Children2}.







check_and_fix_childspec(ChildSpec
                       ,DefChildSpec) when erlang:is_map(ChildSpec) ->
    Keys = [{append, fun filter_append/1, ?DEFAULT_APPEND}],
    {ok, #{append := Append}} = {ok, ChildSpec2} = check_map(ChildSpec
                                                            ,Keys
                                                            ,#{}),
    StartKey =
        if
            Append ->
                case DefChildSpec of
                    #{start := {Mod, Func, _Args}} ->
                        {start, fun filter_start/1, {Mod, Func, []}};
                    _Other ->
                        {start, fun filter_start/1}
                end;
            true ->
                {start, fun filter_start/1}
        end,
    Keys2 = [id
            ,StartKey
            ,{plan, fun filter_plan/1, ?DEFAULT_PLAN}
            ,{count, fun filter_count/1, ?DEFAULT_COUNT}
            ,{type, fun filter_type/1, ?DEFAULT_TYPE}],
    case check_map(ChildSpec, Keys2, ChildSpec2) of
        {ok, ChildSpec3} ->
            DefTerminateTimeout =
                case maps:get(type, ChildSpec3) of
                    worker ->
                        ?DEFAULT_WORKER_TERMINATE_TIMEOUT;
                    supervisor ->
                        ?DEFAULT_SUPERVISOR_TERMINATE_TIMEOUT
                end,
            DefMods = [erlang:element(1, maps:get(start, ChildSpec3))],
            Keys3 = [{terminate_timeout
                     ,fun filter_terminate_timeout/1
                     ,DefTerminateTimeout}
                    ,{modules, fun filter_modules/1, DefMods}],
            case check_map(ChildSpec
                                        ,Keys3
                                        ,ChildSpec3) of
                {ok, ChildSpec4} ->
                    {ok
                    ,childspec_to_child(combine_children(ChildSpec4
                                                        ,DefChildSpec
                                                        ,Append))};
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;
check_and_fix_childspec(Other, _DefChildSpec) ->
    {error, {childspec_type, [{childspec, Other}]}}.







check_and_fix_default_childspec(ChildSpec)
when erlang:is_map(ChildSpec) ->
    Keys = [{start, fun filter_start/1}
           ,{plan, fun filter_plan/1}
           ,{count, fun filter_count/1}
           ,{type, fun filter_type/1}
           ,{terminate_timeout, fun filter_terminate_timeout/1}
           ,{modules, fun filter_modules/1}],
    check_map2(ChildSpec, Keys, #{});
check_and_fix_default_childspec(Other) ->
    {error, {default_childspec_type, [{childspec, Other}]}}.





check_map(ChildSpec, [{Key, Filter, Default}|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            case Filter(Value) of
                {ok, Value2} ->
                    check_map(ChildSpec
                             ,Keys
                             ,maps:put(Key
                                      ,Value2
                                      ,ChildSpec2));
                {error, Reason} ->
                    {error, {childspec_value, [{key, Key}
                                              ,{reason, Reason}]}}
            end
    catch
        _:_ ->
            check_map(ChildSpec
                     ,Keys
                     ,maps:put(Key
                              ,Default
                              ,ChildSpec2))

    end;
check_map(ChildSpec, [{Key, Filter}|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            case Filter(Value) of
                {ok, Value2} ->
                    check_map(ChildSpec
                             ,Keys
                             ,maps:put(Key
                                      ,Value2
                                      ,ChildSpec2));
                {error, Reason} ->
                    {error, {childspec_value, [{key, Key}
                                              ,{reason, Reason}]}}
            end
    catch
        _:_->
            {error, {key_not_found, [{key, Key}
                                    ,{childspec, ChildSpec}]}}
    end;
check_map(ChildSpec, [Key|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            check_map(ChildSpec
                     ,Keys
                     ,maps:put(Key, Value, ChildSpec2))
    catch
        _:_ ->
            {error, {key_not_found, [{key, Key}
                                    ,{childspec, ChildSpec}]}}
    end;
check_map(_ChidlSpec, [], ChildSpec2) ->
    {ok, ChildSpec2}.










check_map2(ChildSpec, [{Key, Filter}|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            case Filter(Value) of
                {ok, Value2} ->
                    check_map2(ChildSpec
                              ,Keys
                              ,maps:put(Key
                                       ,Value2
                                       ,ChildSpec2));
                {error, Reason} ->
                    {error, {childspec_value, [{key, Key}
                                              ,{reason, Reason}]}}
            end
    catch
        _:_ ->
            check_map2(ChildSpec, Keys, ChildSpec2)
    end;
check_map2(_ChidlSpec, [], ChildSpec2) ->
    {ok, ChildSpec2}.







filter_start({_Mod, _Func, _Args}=Start) ->
    {ok, Start};
filter_start({Mod, Func}) ->
    {ok, {Mod, Func, []}};
filter_start(Other) ->
    {error, {format, [{start, Other}]}}.







filter_plan(Plan) when erlang:is_list(Plan) ->
    filter_plan(Plan, []);
filter_plan(Other) ->
    {error, {plan_type, [{plan, Other}]}}.







filter_plan([PlanElem|Plan], Plan2) ->
    case filter_plan_element(PlanElem) of
        {ok, PlanElem2} ->
            filter_plan(Plan, [PlanElem2|Plan2]);
        {error, _Reason}=Error ->
            Error
    end;
filter_plan([], Plan2) ->
    {ok, lists:reverse(Plan2)}.







filter_plan_element(restart) ->
    {ok, restart};
filter_plan_element({restart, WholeInt}=PlanElem) ->
    case is_whole_integer(WholeInt) of
        true ->
            {ok, PlanElem};
        false ->
            {error
            ,{plan_restart_time_integer, [{plan_element, PlanElem}]}}
    end;
filter_plan_element(delete) ->
    {ok, delete};
filter_plan_element({stop, _Reason}=PlanElem) ->
    {ok, PlanElem};
filter_plan_element(stop) ->
    {ok, stop};
filter_plan_element(wait) ->
    {ok, wait};
filter_plan_element(Fun) when erlang:is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 2} ->
            {ok, Fun};
        {arity, Other} ->
            {error, {plan_fun_arity, [{'fun', Fun}, {arity, Other}]}}
    end;
filter_plan_element(Other) ->
    {error, {plan_element_type, [{plan_element, Other}]}}.







is_whole_integer(Int) when erlang:is_integer(Int) ->
    if
        Int >= 0 ->
            true;
        true ->
            false
    end;
is_whole_integer(_Other) ->
    false.







filter_count(infinity) ->
    {ok, infinity};
filter_count(Count) ->
    case is_whole_integer(Count) of
        true ->
            {ok, Count};
        false ->
            {error, {count_range_or_type, [{count, Count}]}}
    end.







filter_terminate_timeout(infinity) ->
    {ok, infinity};
filter_terminate_timeout(TerminateTimeout) ->
    case is_whole_integer(TerminateTimeout) of
        true ->
            {ok, TerminateTimeout};
        false ->
            {error
            ,{terminate_timeout_range_or_type
             ,[{terminate_timeout, TerminateTimeout}]}}
    end.







filter_type(worker) ->
    {ok, worker};
filter_type(supervisor) ->
    {ok, supervisor};
filter_type(Other) ->
    {error, {type_type, [{type, Other}]}}.







filter_modules(dynamic) ->
    {ok, dynamic};
filter_modules(Mod) when erlang:is_atom(Mod) ->
    {ok, [Mod]};
filter_modules(Mods) when erlang:is_list(Mods) ->
    {ok, Mods};
filter_modules(Other) ->
    {error, {modules_type, [{modules, Other}]}}.







filter_append(Bool) when erlang:is_boolean(Bool) ->
    {ok, Bool};
filter_append(Other) ->
    {error, {append_type, [{append, Other}]}}.







start_children([Child|Children], Table, Name) ->
    case do_start_child(Child, Table, Name) of
        ok ->
            start_children(Children, Table, Name);
        {error, _Reason}=Error ->
            _ = terminate_children(Table, Name),
            Error
    end;
start_children([], _Table, _Name) ->
    ok.







do_start_child(Child, Table, Name) ->
    do_start_child(Child, Table, Name, no_pid).







do_start_child(#?CHILD{pid = Pid, extra = Extra}
              ,_Table
              ,_Name
              ,RetType) when erlang:is_pid(Pid) ->
    start_child_fix_return(RetType, Pid, Extra);
do_start_child(#?CHILD{timer_reference = TimerRef}=Child
           ,Table
           ,Name
           ,RetType) ->
    ok = do_delete_child(Child, Table),
    if
        erlang:is_reference(TimerRef) ->
            erlang:cancel_timer(TimerRef);
        true ->
            pass
    end,
    case start_mfa(Child#?CHILD.start) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            Child3 = Child#?CHILD{pid = Pid, extra = undefined},
            ok = progress_report(Child3, Name),
            ok = insert_or_update_child(Child3, Table),
            start_child_fix_return(RetType, Pid, undefined);
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            Extra2 = {extra, Extra},
            Child2 = Child#?CHILD{pid = Pid, extra = Extra2},
            ok = progress_report(Child2, Name),
            ok = insert_or_update_child(Child2, Table),
            start_child_fix_return(RetType, Pid, Extra2);
        {ok, Pid} -> % ignored: {ok, undefined}
            ok = insert_or_update_child(Child#?CHILD{pid = Pid
                                                    ,extra = undefined}
                                       ,Table),
            start_child_fix_return(RetType, Pid, undefined);
        {restart, PosInt}=Reason ->
            error_report(Name
                        ,start_child
                        ,Reason
                        ,child_record_to_proplist(Child)),
            TimerRef2 = restart_timer(PosInt, Child#?CHILD.id),
            ok = insert_or_update_child(Child#?CHILD{pid = restarting
                                                    ,extra = undefined
                                                    ,timer_reference =
                                                         TimerRef2}
                                       ,Table),
            start_child_fix_return(RetType, restarting, undefined);
        {error, Reason}=Error ->
            error_report(Name
                        ,start_child
                        ,Reason
                        ,child_record_to_proplist(Child)),
            Error
    end.







start_child_fix_return(no_pid, _Pid, _Extra) ->
    ok;
start_child_fix_return(no_extra, Pid, _Extra) ->
    {ok, Pid};
start_child_fix_return(normal, Pid, undefined) ->
    {ok, Pid};
start_child_fix_return(normal, Pid, {extra, Extra}) ->
    {ok, Pid, Extra}.







combine_children(ChildSpec, DefChildSpec, true) ->
    Fun =
        fun
            (start
            ,{Mod, Func, Args}
            ,#{start := {_Mod2, _Func2, Args2}}=Map) ->
                Map#{start => {Mod, Func, Args2 ++ Args}};
            (count, infinity, Map) ->
                Map#{count => infinity};
            (count, Count, #{count := Count2}=Map) ->
                if
                    Count2 =:= infinity ->
                        Map#{count => Count};
                    true ->
                        Map#{count => Count + Count2}
                end;
            (terminate_timeout, infinity, Map) ->
                Map#{terminate_timeout => infinity};
            (terminate_timeout
            ,TerminateTimeout
            ,#{terminate_timeout := TerminateTimeout2}=Map) ->
                if
                    TerminateTimeout2 =:= infinity ->
                        Map#{terminate_timeout => TerminateTimeout};
                    true ->
                        Map#{terminate_timeout => TerminateTimeout
                                                + TerminateTimeout2}
                end;
            (modules, dynamic, Map) ->
                Map#{modules => dynamic};
            (modules, Mods, #{modules := Mods2}=Map) ->
                if
                    Mods2 =:= dynamic ->
                        Map#{modules => Mods};
                    true ->
                        Map#{modules => Mods2 ++ Mods}
                end;
            (plan, Plan, #{plan := Plan2}=Map) ->
                Map#{plan => Plan2 ++ Plan};
            (Key, Value, Map) ->
                Map#{Key => Value}
        end,
    maps:fold(Fun, DefChildSpec, ChildSpec);
combine_children(ChildSpec, _DefChildSpec, false) ->
    ChildSpec.







seprate_children(ChildSpec, DefChildSpec) ->
    Fun =
        fun
            (start
            ,{Mod, Func, Args}
            ,#{start := {_Mod2, _Func2, Args2}}=Map) ->
                Map#{start => {Mod, Func, Args -- Args2}};
            (count, infinity, Map) ->
                Map#{count => infinity};
            (count, Count, #{count := Count2}=Map) ->
                if
                    Count2 =:= infinity ->
                        Map#{count => Count};
                    true ->
                        Map#{count => Count - Count2}
                end;
            (terminate_timeout, infinity, Map) ->
                Map#{terminate_timeout => infinity};
            (terminate_timeout
            ,TerminateTimeout
            ,#{terminate_timeout := TerminateTimeout2}=Map) ->
                if
                    TerminateTimeout2 =:= infinity ->
                        Map#{terminate_timeout => TerminateTimeout};
                    true ->
                        Map#{terminate_timeout => TerminateTimeout
                                                - TerminateTimeout2}
                end;
            (modules, dynamic, Map) ->
                Map#{modules => dynamic};
            (modules, Mods, #{modules := Mods2}=Map) ->
                if
                    Mods2 =:= dynamic ->
                        Map#{modules => Mods};
                    true ->
                        Map#{modules => Mods -- Mods2}
                end;
            (plan, Plan, #{plan := Plan2}=Map) ->
                Map#{plan => Plan -- Plan2};
            (Key, Value, Map) ->
                Map#{Key => Value}
        end,
    maps:fold(Fun, DefChildSpec, ChildSpec).







start_mfa({Mod, Func, Args}) ->
    Result =
        case catch erlang:apply(Mod, Func, Args) of
            {ok, Pid} when erlang:is_pid(Pid) ->
                {ok, Pid};
            {ok, Pid, Extra} when erlang:is_pid(Pid) ->
                {ok, Pid, Extra};
            ignore ->
                {ok, undefined};
            {restart, Int} ->
                case is_whole_integer(Int) of
                    true ->
                        {restart, Int};
                    false ->
                        {error
                        ,{director_restart_time_range_or_type
                         ,[{director_restart, Int}]}}
                end;
            {error, _Reason}=Error ->
                Error;
            {'EXIT', Reason} ->
                {error, Reason};
            Other ->
                {error, {bad_start_return_value, Other}}
        end,
    ?trapping_exits,
    Result.







progress_report(Child, Name) ->
    error_logger:info_report(progress
                            ,[{supervisor, Name}
                             ,{started
                              ,child_record_to_proplist(Child)}]).







error_report(Name, ErrorContext, Reason, Params) ->
    error_logger:error_report(supervisor_report
                             ,[{supervisor, Name}
                              ,{errorContext, ErrorContext}
                              ,{reason, Reason}
                              ,{offender, Params}]).







debug([], _Name, _MsgInfo) ->
    [];
debug(Dbg, Name, MsgInfo) ->
    sys:handle_debug(Dbg, fun print/3, Name, MsgInfo).







print(IODev, {in, {call, {From, _Ref}, Request}}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got request \"~p\" from \"~w\" ~n"
             ,[Name, Request, From]);
print(IODev, {out, To, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" sent \"~p\" to \"~w\"~n"
             ,[Name, Msg, To]);
print(IODev, {in, {exit, Pid, Reason}}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got exit signal for pid \"~p\" wit"
              "h reason \"~p\"~n"
             ,[Name, Pid, Reason]);
print(IODev, {timeout, TimerRef, Id}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got timer event for child-id \"~p"
              "\" with timer reference \"~p\"~n"
             ,[Name, Id, TimerRef]);
print(IODev, Other, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got debug \"~p\" ~n"
             ,[Name, Other]).