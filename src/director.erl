%%% ------------------------------------------------------------------------------------------------
%%% Director is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
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
%% @version  18.4.19
%% @doc
%%           Director is a production-ready supervisor and manager for Erlang/Elixir processes that
%%           focuses on speed and flexibility.
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
        ,get_restart_count/2
        ,get_default_childspec/1
        ,change_default_childspec/2
        ,start_link/4 %% With options
        ,start/2 %% Stand-alone
        ,start/3
        ,start/4
        ,stop/1
        ,stop/2
        ,stop/3
        ,terminate_and_delete_child/2
        ,default_childspec/0
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
        ,get_restart_count/3
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

%% Do not use! just for tables like mnesia which wants to delete children of bad node
-export([self_start/1]).

%% -------------------------------------------------------------------------------------------------
%% Types:

-type childspec() :: #{'id' => id()
                      ,'start' => start()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()
                      ,'delete' => delete()
                      ,'state' => child_state()}.
-type  id() :: term().
-type  start() :: module() % Will be {module(), start_link, []}
                | {module(), function()} % Will be {module(), function(), []}
                | mfa().
-type  terminate_timeout() :: timeout().
-type  type() :: 'worker'
               | 'supervisor'
               | 'sup' % supervisor
               | 's' % supervisor
               | 'w'. % worker
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().
-type  delete() :: boolean().
-type  child_state() :: any().

-type default_childspec() :: #{'start' => start()
                              ,'terminate_timeout' => terminate_timeout()
                              ,'type' => type()
                              ,'modules' => modules()
                              ,'delete' => delete()
                              ,'state' => child_state()}.

-type start_return() :: {'ok', pid()} | {'ok', pid(), any()} | 'ignore' | {'error', term()}.

-type init_return() :: 'ok'
                     | {'ok', state()}
                     | {'ok', state(), [childspec()]|[]}
                     | {'ok', state(), [childspec()]|[], default_childspec()}
                     | {'ok', state(), [childspec()]|[], start_options()}
                     | {'ok', state(), [childspec()]|[], default_childspec(), start_options()}
                     | 'ignore'
                     | {'stop', Reason::any()}.
-type  state() :: any().

-type terminate_return() :: {'ok', callback_return_options()}
                          | {'new_error', NewReason::any(), callback_return_options()}
                          | any().
-type  callback_return_options() :: [callback_return_option()] | [].
-type   callback_return_option() :: {'log', boolean()}.

-type handle_start_return() :: {'ok', child_state(), state(), callback_return_options()}
                             | {'stop', reason(), child_state(), callback_return_options()}.

-type handle_exit_return() :: {action(), child_state(), state(), callback_return_options()}.
-type  action() :: 'restart'
                 | {'restart', pos_integer()}
                 | 'delete'
                 | 'wait'
                 | 'stop'
                 | {'stop', Reason::any()}.

-type handle_terminate_return() :: {'ok', child_state(), state(), callback_return_options()}
                                 | {'stop', reason(), child_state(), callback_return_options()}.

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type director() :: pid() | atom() | tuple().

-type start_options() :: [start_option()] | [].
-type  start_option() :: {'debug', [sys:dbg_opt()]|[]}
                       | {'spawn_opt', proc_lib:spawn_option()}
                       | {'timeout', timeout()}
                       | {'table_module', module()}
                       | {'table_init_argument', any()}
                       | {'delete_table', boolean()}.

-type metadata_arg() :: #{'pid' => pid() %% only for handle_start
                         ,'extra' => any() %% if process returns extra section and only in handle_start
                         ,'restart_count' => non_neg_integer()}.

-type init_arg() :: any().

-type reason() :: any().

-export_type([childspec/0
             ,id/0
             ,start/0
             ,terminate_timeout/0
             ,type/0
             ,modules/0
             ,append/0
             ,delete/0
             ,child_state/0
             ,default_childspec/0
             ,start_return/0
             ,init_return/0
             ,state/0
             ,terminate_return/0
             ,callback_return_options/0
             ,callback_return_option/0
             ,handle_start_return/0
             ,handle_exit_return/0
             ,action/0
             ,handle_terminate_return/0
             ,register_name/0
             ,director/0
             ,start_options/0
             ,start_option/0
             ,metadata_arg/0
             ,init_arg/0
             ,reason/0]).

%% -------------------------------------------------------------------------------------------------
%% Behaviour information:

-callback
init(init_arg()) ->
    init_return().


-callback
handle_start(id(), child_state(), state(), metadata_arg()) ->
    handle_start_return().


-callback
handle_terminate(id(), child_state(), reason(), state(), metadata_arg()) ->
    handle_terminate_return().


-callback
handle_exit(id(), child_state(), reason(), state(), metadata_arg()) ->
    handle_exit_return().


-callback
terminate(reason(), state()) ->
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
                ,delete_table
                ,timers
                ,msg}).

%% -------------------------------------------------------------------------------------------------
%% supervisor-like API:

-spec
start_link(module(), init_arg()) ->
    start_return().
%% @doc
%%      Starts and links a director.
%% @end
start_link(Mod, InitArg) when erlang:is_atom(Mod) ->
    gen:start(?MODULE, link, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start_link(register_name()|module(), module()|init_arg(), init_arg()|start_options()) ->
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
    {'ok', pid()}                                               |
    {'ok', pid(), Extra::term()}                                |
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
    |{'workers', non_neg_integer()}]   |
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
    case director_child:check_childspec(ChildSpec, ?DEF_DEF_CHILDSPEC) of
        {ok, _FixedChildSpec} ->
            ok;
        {error, _Reason}=Error ->
            Error
    end.

%% -------------------------------------------------------------------------------------------------
%% Specific API:

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
start_link(register_name(), module(), init_arg(), start_options()) ->
    start_return().
%% @doc
%%      Starts and links a director with start options.
%% @end
start_link(Name, Mod, InitArg, Opts) when erlang:is_tuple(Name) andalso
                                          erlang:is_atom(Mod) andalso
                                          erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Name, Mod, InitArg, Opts).


-spec
start(module(), init_arg()) ->
    start_return().
%% @doc
%%      Starts stand-alone director.
%% @end
start(Mod, InitArg) when erlang:is_atom(Mod) ->
    gen:start(?MODULE, nolink, Mod, InitArg, ?DEF_START_OPTIONS).


-spec
start(register_name()|module(), module()|init_arg(), init_arg()|start_options()) ->
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
start(register_name(), module(), init_arg(), start_options()) ->
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
stop(director(), reason()) ->
    'ok'.
%% @doc
%%      Stops director process with 'infinity' timeout.
%% @end
stop(Director, Reason) when ?is_director(Director) ->
    proc_lib:stop(Director, Reason, ?DEF_STOP_TIMEOUT).


-spec
stop(director(), reason(), timeout()) ->
    'ok'.
%% @doc
%%      Stops director process.
%% @end
stop(Director, Reason, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    proc_lib:stop(Director, Reason, Timeout).


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
become_supervisor(director(), pid(), childspec()) ->
    'ok' | {'error', 'noproc' | term()}.
%% @doc
%%      Tells director process become supervisor of this Pid with this Childspec.<br/>
%%      Pid should be alive Erlang process on same node.<br/>
%%      After this, new child process will have an Erlang message {'change_parent', NewParentPid} in<br/>
%%      Its mailbox.<br/>
%%      Error maybe occur for reading from or inserting to table.
%% @end
become_supervisor(Director, Pid, ChildSpec) when ?is_director(Director) andalso
                                                 erlang:is_map(ChildSpec) andalso
                                                 erlang:is_pid(Pid) ->
    gen_server:call(Director, {?BECOME_SUPERVISOR_TAG, Pid, ChildSpec}).


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
%% @hidden
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
%% @hidden
%% @doc
%%      Restarts a terminated or waited child using child id.<br/>
%%      Reason of error may be for starting process or table.
%% @end
restart_child(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?RESTART_CHILD_TAG, Id}, Timeout).


-spec
terminate_child(director(), id() | pid(), timeout()) ->
    'ok' | {'error', Reason::'not_found'|'not_parent'|term()}.
%% @hidden
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
%% @hidden
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
%% @hidden
%% @doc
%%      Returns count of children.<br/>
%%      Error is for reading from table.
%% @end
count_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?COUNT_CHILDREN_TAG, Timeout).


-spec
which_children(director(), timeout()) ->
    [{id(), type(), pid()|'restarting'|'undefined', modules()}] | [] | {'error', term()}.
%% @hidden
%% @doc
%%      Returns information about each child.<br/>
%%      Error is for reading from table.
%% @end
which_children(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?WHICH_CHILDREN_TAG, Timeout).


-spec
get_childspec(director(), id() | pid(), timeout()) ->
    {'ok', childspec()} | {'error', 'not_found'|term()}.
%% @hidden
%% @doc
%%      Returns childspec of child.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Director, Name, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_CHILDSPEC_TAG, Name}, Timeout).


-spec
get_restart_count(director(), id(), timeout()) ->
    {'ok', non_neg_integer()} | {'error', 'not_found'|term()}.
%% @hidden
%% @doc
%%      Returns restart count of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_RESTART_COUNT_TAG, Id}, Timeout).


-spec
get_pid(director(), id(), timeout()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @hidden
%% @doc
%%      Returns pid of a running child.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pid(Director, Id, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, {?GET_PID_TAG, Id}, Timeout).


-spec
get_pids(director(), timeout()) ->
    {'ok', [{id(), pid()}] | []} | {'error', term()}.
%% @hidden
%% @doc
%%      Returns list of {id, pid}s for all running children.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pids(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_PIDS_TAG, Timeout).


-spec
get_default_childspec(director(), timeout()) ->
    default_childspec().
%% @hidden
%% @doc
%%      Returns director process's default childspec.
%% @end
get_default_childspec(Director, Timeout) when ?is_director(Director) andalso ?is_timeout(Timeout) ->
    gen_server:call(Director, ?GET_DEF_CHILDSPEC, Timeout).


-spec
change_default_childspec(director(), default_childspec(), timeout()) ->
    {'ok', default_childspec()} | {'error', Reason::term()}.
%% @hidden
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
%% @hidden
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
become_supervisor(director(), pid(), childspec(), timeout()) ->
    ok | {'error', 'no_proc' | 'no_response' | 'proc_exited' | term()}.
%% @hidden
%% @doc
%%      Tells director process become supervisor of this Pid with this Childspec.<br/>
%%      Pid should be alive Erlang process on same node.<br/>
%%      After this, new child process will have an Erlang message {'change_parent', NewParentPid} in<br/>
%%      Its mailbox.<br/>
%%      Error maybe occur for reading from or inserting to table.
%% @end
become_supervisor(Director, Pid, ChildSpec, Timeout) when ?is_director(Director) andalso
                                                          erlang:is_map(ChildSpec) andalso
                                                          erlang:is_pid(Pid) andalso
                                                          ?is_timeout(Timeout) ->
    gen_server:call(Director, {?BECOME_SUPERVISOR_TAG, Pid, ChildSpec}, Timeout).


-spec
delete_running_child(director(), pid(), timeout()) ->
    'ok' | {'error', 'not_found' | term()}.
%% @hidden
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
    start_timer(0, Id).


%% -------------------------------------------------------------------------------------------------
%% 'gen' callback:

%% @hidden
init_it(Starter, self, Name, Mod, InitArg, Opts) ->
    init_it(Starter, erlang:self(), Name, Mod, InitArg, Opts);
init_it(Starter, Parent, Name0, Mod, InitArg, Opts) ->
    Name = name(Name0),
    erlang:process_flag(trap_exit, true),
    case init(Name, Mod, InitArg, Opts) of
        {ok, {Data, Children, DefChildSpec, Dbg, TabMod, TabInitArg, DelTab}} ->
            case director_table:create(TabMod, TabInitArg) of
                {ok, TabState} ->
                    case start_children(Name, Mod, Data, TabMod, TabState, Children) of
                        {ok, Data2, TabState2} ->
                            State = #?STATE{data = Data2
                                           ,name = Name
                                           ,module = Mod
                                           ,init_argument = InitArg
                                           ,table_state = TabState2
                                           ,default_childspec = DefChildSpec
                                           ,table_module = TabMod
                                           ,delete_table = DelTab
                                           ,msg = undefined},
                            proc_lib:init_ack(Starter, {ok, erlang:self()}),
%%                    exit(element(2, (catch loop(Parent, Dbg, State))));
                            loop(Parent, Dbg, State);
                        {error, Rsn} ->
                            unregister_name(Name0),
                            proc_lib:init_ack(Starter, {error, Rsn}),
                            erlang:exit(Rsn)
                    end;
                {hard_error, Rsn} ->
                    unregister_name(Name0),
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
    case init(Name, Mod, InitArg, []) of
        {ok, {_, Children, DefChildSpec, _, _, _, _}} ->
            case director_utils:has_duplicate([Child#?CHILD.id || Child <- Children]) of
                false ->
                    case change_old_children_pids(Children, TabMod, TabState) of
                        {ok, TabState2} ->
                            {ok, State#?STATE{table_state = TabState2
                                             ,default_childspec = DefChildSpec}};
                        {error, _}=Err ->
                            Err
                    end;
                {true, Id} ->
                    {error, {duplicate_child_id, Id}}
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
    Msg =
        receive
            Msg_ ->
                Msg_
        end,
    process_message(Parent, debug(Dbg, State#?STATE.name, {in, Msg}), State#?STATE{msg = Msg}, Msg).


process_message(Parent
               ,Dbg
               ,#?STATE{name = Name}=State
               ,{?GEN_CALL_TAG, From, Request}) ->
    {Dbg2, State2} = process_request(debug(Dbg, Name, {request, From, Request}), State, From, Request),
    loop(Parent, Dbg2, State2);
process_message(Parent, Dbg, #?STATE{name= Name}=State, {'EXIT', Pid, Reason}=Msg) ->
    process_exit(Parent, debug(Dbg, Name, Msg), State, Pid, Reason);
process_message(Parent, Dbg, #?STATE{name = Name}=State, {'DOWN', _, _, Pid, Id}) ->
    process_timeout(Parent, debug(Dbg, Name, {'DOWN', Pid, Id}), State, Pid, Id);
process_message(Parent, Dbg, #?STATE{name = Name}=State, {system, From, Msg}) ->
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, debug(Dbg, Name, {system, Msg}), State);
%% Catch clause:
process_message(Parent, Dbg, #?STATE{table_module = TabMod, table_state = TabState}=State, Msg) ->
    case director_table:handle_message(TabMod, TabState, Msg) of
        {ok, TabState2} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {soft_error, TabState2, unknown} ->
            error_logger:error_report("DIRECTOR ~tp received unknown message ~tp\n"
                                     ,[State#?STATE.name, Msg]),
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
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
        {ok, #?CHILD{pid = Pid}} ->
            {reply(Dbg, Name, From, {error, Pid}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?GET_PIDS_TAG) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            {reply(Dbg
                  ,Name
                  ,From
                  ,{ok, [{Id, Pid} || #?CHILD{pid = Pid, id = Id} <- Children, erlang:is_pid(Pid)]})
            ,State};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?COUNT_CHILDREN_TAG) ->
    case director_table:count_children(TabMod, TabState) of
        {error, Rsn}=Reply ->
            terminate(reply(Dbg, Name, From, Reply), State, Rsn);
        Reply ->
            {reply(Dbg, Name, From, Reply), State}
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?DELETE_CHILD_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {reply(Dbg, Name, From, {error, running}), State};
        {ok, #?CHILD{pid = restarting}} ->
            {reply(Dbg, Name, From, {error, restarting}), State};
        {ok, Child} ->
            case director_table:delete(TabMod, TabState, Child) of
                {ok, TabState2} ->
                    {reply(Dbg, Name, From, ok), State#?STATE{table_state = TabState2}};
                {hard_error, Rsn} ->
                    terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
            end;
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,{?GET_CHILDSPEC_TAG, Id}) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, Child} ->
            {reply(Dbg, Name, From, {ok, director_child:child_to_childspec(Child)}), State};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{module = Mod
                       ,data = Data
                       ,table_module = TabMod
                       ,table_state = TabState
                       ,name = Name}=State
               ,From
               ,{?RESTART_CHILD_TAG, Id}) ->
    case handle_restart(Name, Mod, Data, TabMod, TabState, Id) of
        {ok, Data2, TabState2, #?CHILD{pid = Pid, extra = undefined}} ->
            {reply(Dbg, Name, From, {ok, Pid}), State#?STATE{data = Data2, table_state = TabState2}};
        {ok, Data2, TabState2, #?CHILD{pid = Pid, extra = {value, Extra}}} ->
            {reply(Dbg, Name, From, {ok, Pid, Extra})
            ,State#?STATE{data = Data2, table_state = TabState2}};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod
                       ,table_state = TabState
                       ,name = Name
                       ,default_childspec = DefChildSpec
                       ,module = Mod
                       ,data = Data}=State
               ,From
               ,{?START_CHILD_TAG, ChildSpec}) ->
    case director_child:check_childspec(ChildSpec, DefChildSpec) of
        {ok, #?CHILD{id = Id}=Child} ->
            case director_table:lookup_id(TabMod, TabState, Id) of
                {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
                    {reply(Dbg, Name, From, {error, {already_started, Pid}}), State};
                {ok, _} ->
                    {reply(Dbg, Name, From, {error, already_present}), State};
                {soft_error, TabState2, not_found} ->
                    case handle_start(Name, Mod, Data, TabMod, TabState2, Child) of
                        {ok, Data2, TabState3, #?CHILD{pid = Pid, extra = undefined}} ->
                            {reply(Dbg, Name, From, {ok, Pid})
                            ,State#?STATE{data = Data2, table_state = TabState3}};
                        {ok, Data2, TabState3, #?CHILD{pid = Pid, extra = {value, Extra}}} ->
                            {reply(Dbg, Name, From, {ok, Pid, Extra})
                            ,State#?STATE{data = Data2, table_state = TabState3}};
                        {soft_error, TabState3, Rsn} ->
                            {reply(Dbg, Name, From, {error, Rsn})
                            ,State#?STATE{table_state = TabState3}};
                        {hard_error, Rsn} ->
                            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
                    end;
                {hard_error, Rsn} ->
                    terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
            end;
        {error, _}=Err ->
            {reply(Dbg, Name, From, Err), State}
    end;

process_request(Dbg
               ,#?STATE{module = Mod
                       ,data = Data
                       ,table_module = TabMod
                       ,table_state = TabState
                       ,name = Name}=State
               ,From
               ,{?TERMINATE_CHILD_TAG, Term}) ->
    case do_terminate_child(Name, Mod, Data, TabMod, TabState, Term) of
        {ok, Data2, TabState2, _} ->
            {reply(Dbg, Name, From, ok), State#?STATE{data = Data2, table_state = TabState2}};
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
               ,From
               ,?WHICH_CHILDREN_TAG) ->
    case director_table:which_children(TabMod, TabState) of
        {error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn);
        Reply ->
            {reply(Dbg, Name, From, Reply), State}

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
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
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
    case director_child:check_default_childspec(ChildSpec) of
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

process_request(Dbg
               ,#?STATE{module = Mod
                       ,data = Data
                       ,table_module = TabMod
                       ,table_state = TabState
                       ,name = Name}=State
               ,From
               ,{?TERMINATE_AND_DELETE_CHILD_TAG, Term}) ->
    case do_terminate_child(Name, Mod, Data, TabMod, TabState, Term) of
        {ok, Data2, TabState2, Child2} ->
            case director_table:delete(TabMod, TabState2, Child2) of
                {ok, TabState3} ->
                    {reply(Dbg, Name, From, ok), State#?STATE{data = Data2, table_state = TabState3}};
                {hard_error, Rsn}=Err ->
                    Dbg2 = reply(Dbg, Name, From, Err),
                    terminate(Dbg2, State, Rsn)
            end;
        {soft_error, TabState2, Rsn} ->
            {reply(Dbg, Name, From, {error, Rsn}), State#?STATE{table_state = TabState2}};
        {hard_error, Rsn} ->
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;

process_request(Dbg
               ,#?STATE{table_module = TabMod, table_state = TabState, name = Name, default_childspec = DefChildSpec}=State
               ,From
               ,{?BECOME_SUPERVISOR_TAG, Pid, ChildSpec}) ->
    try erlang:is_process_alive(Pid) of % Proc should be on same node
        true ->
            case director_child:check_childspec(ChildSpec, DefChildSpec) of
                {ok, #?CHILD{id = Id, start = Start}=Child} ->
                    case director_table:lookup_id(TabMod, TabState, Id) of
                        {soft_error, TabState2, not_found} ->
                            try erlang:link(Pid) of
                                _ ->
                                    case director_table:change_parent(TabMod
                                                                     ,TabState2
                                                                     ,Child#?CHILD{pid = Pid}) of
                                        {ok, TabState3} ->
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
    catch
        _:_ -> %% Process from other node
            {reply(Dbg, Name, From, {error, other_node}), State}
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
            terminate(reply(Dbg, Name, From, {error, Rsn}), State, Rsn)
    end;
%% Catch clause:
process_request(Dbg, #?STATE{name = Name}=State, From, Other) ->
    {reply(Dbg, Name, From, {error, {call, Other}}), State}.


process_exit(Parent, Dbg, #?STATE{name = Name}=State, Parent, Reason) ->
    terminate(debug(Dbg, Name, {parent, Parent, Reason}), State, Reason);
process_exit(Parent
            ,Dbg
            ,#?STATE{table_module = TabMod, table_state = TabState, name = Name}=State
            ,Pid
            ,Reason) ->
    case director_table:lookup_pid(TabMod, TabState, Pid) of
        {ok, #?CHILD{supervisor = Sup, id = Id}=Child} when erlang:self() =:= Sup ->
            Dbg2 = debug(Dbg, Name, {'EXIT', Id}),
            Child2 = Child#?CHILD{pid = undefined, extra = undefined},
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    handle_exit(Parent
                               ,Dbg2
                               ,State#?STATE{table_state = TabState2}
                               ,Child2
                               ,Reason);
                {hard_error, Rsn} ->
                    terminate(Dbg, State, Rsn)
            end;
        {ok, #?CHILD{id = Id}} ->
            loop(Parent, debug(Dbg, Name, {'EXIT', Id}), State);
        {soft_error, TabState2, _} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {hard_error, Rsn} ->
            terminate(Dbg, State, Rsn)
    end.


handle_exit(Parent
           ,Dbg
           ,#?STATE{module = Mod
                   ,data = Data
                   ,name = Name}=State
           ,#?CHILD{id = Id
                   ,restart_count = RestartCount
                   ,state = ChState}=Child
           ,Rsn) ->
    RestartCount2 = RestartCount + 1,
    MetaData = #{restart_count => RestartCount2},
    {HandleExitResult, Log} =
        try Mod:handle_exit(Id, ChState, Rsn, Data, MetaData) of
            {Action, ChState2, Data2, Opts} when erlang:is_list(Opts) ->
                Log2 =
                    case director_utils:value(log, Opts, ?DEF_LOG) of
                        true ->
                            true;
                        _ ->
                            false
                    end,
                {{Action, ChState2, Data2}, Log2};
            Other ->
                {{error, {return, [{value, Other}
                                  ,{module, Mod}
                                  ,{function, handle_exit}
                                  ,{arguments, [Data, Id, State, Rsn, MetaData]}]}}
                ,?DEF_LOG}
        catch
            _:Rsn2 ->
                {{error, {crash, [{reason, Rsn2}
                                 ,{stacktrace, erlang:get_stacktrace()}
                                 ,{module, Mod}
                                 ,{function, handle_exit}
                                 ,{arguments, [Data, Id, State, Rsn, MetaData]}]}}
                ,?DEF_LOG}
        end,
    if
        Log ->
            error_logger:error_report(supervisor_report
                                     ,[{supervisor, Name}
                                      ,{errorContext, child_terminated}
                                      ,{reason, Rsn}
                                      ,{offender, director_child:child_to_proplist(Child)}]);
        true ->
            ok
    end,
    case HandleExitResult of
        {Action2, ChState3, Data3} ->
            handle_exit_2(Parent
                         ,Dbg
                         ,State#?STATE{data = Data3}
                         ,Child#?CHILD{state = ChState3, restart_count = RestartCount2}
                         ,Rsn
                         ,Action2);
        {error, Rsn3} ->
            terminate(Dbg, State, Rsn3)
    end.


handle_exit_2(Parent
             ,Dbg
             ,#?STATE{name = Name, module = Mod, data = Data, table_module = TabMod, table_state = TabState}=State
             ,#?CHILD{id =Id}=Child
             ,Rsn
             ,restart) ->
    case handle_start(Name, Mod, Data, TabMod, TabState, Child) of
        {ok, Data2, TabState2, _} ->
            loop(Parent, Dbg, State#?STATE{data = Data2, table_state = TabState2});
        {hard_error, Rsn2} ->
            terminate(Dbg, State, [Rsn, Rsn2]);
        {soft_error, TabState2, _} ->
            handle_exit_3(Parent
                         ,debug(Dbg, Name, {timer, Id, 0})
                         ,State#?STATE{table_state = TabState2}
                         ,Child#?CHILD{timer = start_timer(0, Id), pid = restarting}
                         ,Rsn)
    end;

handle_exit_2(Parent
             ,Dbg
             ,#?STATE{name = Name}=State
             ,#?CHILD{id = Id}=Child
             ,Rsn
             ,{restart, Timeout}) when erlang:is_integer(Timeout) andalso Timeout > 0 ->
    handle_exit_3(Parent
                 ,debug(Dbg, Name, {timer, Id, Timeout})
                 ,State
                 ,Child#?CHILD{timer = start_timer(Timeout, Id), pid = restarting}
                 ,Rsn);

handle_exit_2(Parent, Dbg, State, Child, Rsn, wait) ->
    handle_exit_3(Parent
                 ,Dbg
                 ,State
                 ,Child#?CHILD{pid = undefined, extra = undefined}
                 ,Rsn);

handle_exit_2(Parent
             ,Dbg
             ,#?STATE{table_module = TabMod, table_state = TabState}=State
             ,Child
             ,Rsn
             ,delete) ->
    case director_table:delete(TabMod, TabState, Child) of
        {ok, TabState2} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {hard_error, Rsn2} ->
            terminate(Dbg, State, [Rsn, Rsn2])
    end;

handle_exit_2(_
             ,Dbg
             ,#?STATE{table_module = TabMod, table_state = TabState}=State
             ,Child
             ,Rsn
             ,stop) ->
    case director_table:insert(TabMod, TabState, Child) of
        {ok, TabState2} ->
            terminate(Dbg, State#?STATE{table_state = TabState2}, Rsn);
        {hard_error, Rsn2} ->
            terminate(Dbg, State, [Rsn, Rsn2])
    end;

handle_exit_2(_
             ,Dbg
             ,#?STATE{table_module = TabMod, table_state = TabState}=State
             ,Child
             ,_
             ,{stop, Rsn2}) ->
    case director_table:insert(TabMod, TabState, Child) of
        {ok, TabState2} ->
            terminate(Dbg, State#?STATE{table_state = TabState2}, Rsn2);
        {hard_error, Rsn3} ->
            terminate(Dbg, State, [Rsn2, Rsn3])
    end;
%% Catch Clause:
handle_exit_2(_
             ,Dbg
             ,#?STATE{module = Mod
                     ,data = Data
                     ,table_module = TabMod
                     ,table_state = TabState}=State
             ,#?CHILD{id = Id, restart_count = RestartCount}=Child
             ,Rsn
             ,Action) ->
    case director_table:insert(TabMod, TabState, Child) of
        {ok, TabState2} ->
            terminate(Dbg
                     ,State#?STATE{table_state = TabState2}
                     ,{action, [{action, Action}
                               ,{module, Mod}
                               ,{function, handle_exit}
                               ,{arguments, [Data
                                            ,Id
                                            ,State
                                            ,Rsn
                                            ,#{restart_count => RestartCount}]}]});
        {hard_error, Rsn2} ->
            terminate(Dbg, State, [Rsn, Rsn2])
    end.


handle_exit_3(Parent
             ,Dbg
             ,#?STATE{table_module = TabMod, table_state = TabState}=State
             ,Child
             ,Rsn) ->
    case director_table:insert(TabMod, TabState, Child) of
        {ok, TabState2} ->
            loop(Parent, Dbg, State#?STATE{table_state = TabState2});
        {hard_error, Rsn2} ->
            terminate(Dbg, State, [Rsn, Rsn2])
    end.


process_timeout(Parent
               ,Dbg
               ,#?STATE{name = Name, module = Mod, data = Data, table_module = TabMod, table_state = TabState}=State
               ,TimerPid
               ,Id) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{timer = TimerPid}=Child} ->
            case handle_start(Name, Mod, Data, TabMod, TabState, Child) of
                {ok, Data2, TabState2, _} ->
                    loop(Parent, Dbg, State#?STATE{data = Data2, table_state = TabState2});
                {soft_error, TabState2, Reason} ->
                    handle_exit(Parent
                               ,Dbg
                               ,State#?STATE{table_state = TabState2}
                               ,Child
                               ,Reason);
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


init(Name, Mod, InitArg, Opts) ->
    Rslt =
        try Mod:init(InitArg) of
            ok ->
                {ok, undefined, [], ?DEF_DEF_CHILDSPEC, ?DEF_START_OPTIONS};
            {ok, Data} ->
                {ok, Data, [], ?DEF_DEF_CHILDSPEC, ?DEF_START_OPTIONS};
            {ok, Data, ChildSpecs} ->
                case director_child:check_childspecs(ChildSpecs) of
                    {ok, ChildSpecs2} ->
                        {ok, Data, ChildSpecs2, ?DEF_DEF_CHILDSPEC, ?DEF_START_OPTIONS};
                    {error, _}=Error ->
                        Error
                end;
            {ok, Data, ChildSpecs, Opts2} when erlang:is_list(Opts2) ->
                case director_child:check_childspecs(ChildSpecs) of
                    {ok, ChildSpecs2} ->
                        {ok, Data, ChildSpecs2, ?DEF_DEF_CHILDSPEC, Opts2};
                    {error, _}=Error ->
                        Error
                end;
            {ok, Data, ChildSpecs, DefChildSpec} ->
                case director_child:check_default_childspec(DefChildSpec) of
                    {ok, DefChildSpec2} ->
                        case director_child:check_childspecs(ChildSpecs, DefChildSpec2) of
                            {ok, ChildSpecs2} ->
                                {ok, Data, ChildSpecs2, DefChildSpec2, ?DEF_START_OPTIONS};
                            {error, _}=Error ->
                                Error
                        end;
                    {error, _}=Err ->
                        Err
                end;
            {ok, Data, ChildSpecs, DefChildSpec, Opts2} when erlang:is_list(Opts2) ->
                case director_child:check_default_childspec(DefChildSpec) of
                    {ok, DefChildSpec2} ->
                        case director_child:check_childspecs(ChildSpecs, DefChildSpec2) of
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
                {error, {return, [{value, Other}
                                 ,{module, Mod}
                                 ,{function, init}
                                 ,{arguments, [InitArg]}]}}
        catch
            _:Rsn ->
                {error, {crash, [{reason, Rsn}
                                ,{stacktrace, erlang:get_stacktrace()}
                                ,{module, Mod}
                                ,{function, init}
                                ,{arguments, [InitArg]}]}}
        end,
    case Rslt of
        {ok, Data2, ChildSpecs3, DefChildSpec3, Opts3} ->
            Opts4 = director_utils:concat(Opts3, Opts),
            DbgFilter =
                fun(Val) ->
                    try
                        sys:debug_options(Val)
                    catch
                        _:_ ->
                            error_logger:format("~tp: ignoring erroneous debug options: ~tp\n"
                                               ,[Name, Val]),
                            ?DEF_DEBUG_OPTIONS
                    end
                end,
            Dbg = director_utils:option(Opts4, debug, DbgFilter, ?DEF_DEBUG_OPTIONS),
            TabFilter =
                fun
                    (Val) when erlang:is_list(Val) ->
                        Val2 = director_utils:proper(Val, []),
                        Mode =
                            case lists:keyfind(table, 1, Val2) of
                                {_, Mode2} when erlang:is_atom(Mode2) ->
                                    Mode2;
                                {_, Mode2} ->
                                    error_logger:format("~tp: ignoring erroneous db table: ~tp\n"
                                                       ,[Name, Mode2]),
                                    ?DEF_TABLE_MODE;
                                false ->
                                    ?DEF_TABLE_MODE;
                                Mode2 ->
                                    error_logger:format("~tp: ignoring erroneous db table: ~tp\n"
                                                       ,[Name, Mode2]),
                                    ?DEF_TABLE_MODE
                            end,
                        TabInitArg =
                            case lists:keyfind(init_arg, 1, Val2) of
                                {_, TabInitArg2} ->
                                    {value, TabInitArg2};
                                false ->
                                    ?DEF_TABLE_INIT_ARG;
                                TabInitArg2 ->
                                    error_logger:format("~tp: ignoring erroneous table init argumen"
                                                        "t: ~tp\n"
                                                       ,[Name, TabInitArg2]),
                                    ?DEF_TABLE_INIT_ARG
                            end,
                        DelTab =
                            case lists:keyfind(delete, 1, Val2) of
                                {_, DelTab2} when erlang:is_boolean(DelTab2) ->
                                    DelTab2;
                                false ->
                                    ?DEF_DELETE_TABLE;
                                DelTab2 ->
                                    error_logger:format("~tp: ignoring erroneous table delete flag:"
                                                        " ~tp\n"
                                                       ,[Name, DelTab2]),
                                    ?DEF_DELETE_TABLE
                            end,
                        {Mode, TabInitArg, DelTab};
                    (Val) ->
                        error_logger:format("~tp: ignoring erroneous table options: ~tp\n"
                                           ,[Name, Val]),
                        {?DEF_TABLE_MODE, ?DEF_TABLE_INIT_ARG, ?DEF_DELETE_TABLE}
                end,
            {TabMode, TabInitArg, DelTab} = director_utils:option(Opts4
                                                                 ,db
                                                                 ,TabFilter
                                                                 ,{?DEF_TABLE_MODE
                                                                  ,?DEF_TABLE_INIT_ARG
                                                                  ,?DEF_DELETE_TABLE}),
            TabMod = erlang:list_to_atom("director_table_" ++ erlang:atom_to_list(TabMode)),
            {ok, {Data2, ChildSpecs3, DefChildSpec3, Dbg, TabMod, TabInitArg, DelTab}};
        ignore ->
            ignore;
        {error, _}=Err2 ->
            Err2
    end.


start_children(Name, Mod, Data, TabMod, TabState, [#?CHILD{id=Id}=Child|Children]) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {soft_error, TabState2, not_found} ->
            case handle_start(Name, Mod, Data, TabMod, TabState2, Child) of
                {ok, Data2, TabState3, _} ->
                    start_children(Name, Mod, Data2, TabMod, TabState3, Children);
                {soft_error, TabState3, Rsn} ->
                    _ = terminate_and_delete_children(Name, Mod, Data, TabMod, TabState3),
                    {error, Rsn};
                {hard_error, Rsn} ->
                    _ = terminate_and_delete_children(Name, Mod, Data, TabMod, TabState2),
                    {error, Rsn}
            end;
        {ok, _} -> % So i'm using shared table and this child is started by other supervisor
            start_children(Name, Mod, Data, TabMod, TabState, Children);
        {hard_error, Rsn} ->
            {error, Rsn}
    end;
start_children(_, _, Data, _, TabState, []) ->
    {ok, Data, TabState}.


terminate(Dbg
         ,#?STATE{module = Mod
                 ,data = Data
                 ,table_module = TabMod
                 ,table_state = TabState1
                 ,name = Name
                 ,delete_table = DelTab
                 ,msg = LastMsg}
         ,Rsn) ->
    {Rsn2, TabState} =
        case terminate_and_delete_children(Name, Mod, Data, TabMod, TabState1) of
            {ok, _, TabState2} ->
                {[Rsn], TabState2};
            {error, TabState2, Rsn3} ->
                {[Rsn, Rsn3], TabState2}
        end,
    Rsn4 =
        if
            DelTab ->
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
        if
            erlang:length(Rsn4) =:= 1 ->
                erlang:hd(Rsn4);
            true ->
                Rsn4
        end,
    Rsn7 = call_terminate(Name, Mod, Data, Rsn6, LastMsg),
    sys:print_log(Dbg),
    erlang:exit(Rsn7).




call_terminate(Name, Mod, Data, Rsn, Msg) ->
    {Rsn2, Log} =
        try Mod:terminate(Rsn, Data) of
            {new_error, Rsn3, Opts} when erlang:is_list(Opts) ->
                Log2 =
                    case director_utils:value(log, Opts, ?DEF_LOG) of
                        true ->
                            true;
                        _ ->
                            false
                    end,
                {Rsn3, Log2};
            {ok, Opts} when erlang:is_list(Opts) ->
                Log2 =
                    case director_utils:value(log, Opts, ?DEF_LOG) of
                        true ->
                            true;
                        _ ->
                            fasle
                    end,
                {Rsn, Log2};
            _ ->
                {Rsn, ?DEF_LOG}
        catch
            _:Rsn3 ->
                {[Rsn, {crash, [{reason, Rsn3}
                               ,{stacktrace, erlang:get_stacktrace()}
                               ,{module, Mod}
                               ,{arguments, [Rsn, Data]}]}]
                ,?DEF_LOG}
        end,
    if
        Log ->
            error_logger:error_msg("** DIRECTOR ~tp terminating \n"
                                   "** Reason for termination ~tp \n"
                                   "** Last message was ~tp \n"
                                  ,[Name, Rsn2, Msg]);
        true ->
            ok
    end,
    Rsn2.



terminate_and_delete_children(Name, Mod, Data, TabMod, TabState) ->
    case director_table:tab2list(TabMod, TabState) of
        {ok, Children} ->
            terminate_and_delete_children(Name, Mod, Data, TabMod, TabState, Children);
        {hard_error, Rsn} ->
            {error, TabState, Rsn}
    end.


terminate_and_delete_children(Name, Mod, Data, TabMod, TabState, Children) ->
    TerminateAndDelete =
        fun
            (#?CHILD{id = Id, delete = Bool, supervisor = Sup}
            ,{Data_, TabState_, Rsns}) when erlang:self() == Sup ->
                case do_terminate_child(Name, Mod, Data_, TabMod, TabState_, Id) of
                    {ok, Data2, TabState3, Child} ->
                        if
                            Bool ->
                                case director_table:delete(TabMod, TabState3, Child) of
                                    {ok, TabState4} ->
                                        {Data2, TabState4, Rsns};
                                    {hard_error, Rsn} ->
                                        {Data2, TabState3, [Rsn|Rsns]}
                                end;
                            true ->
                                {Data2, TabState3, Rsns}
                        end;
                    {soft_error, TabState3, _} ->
                        {Data_, TabState3, Rsns};
                    {hard_error, Rsn} ->
                        {Data_, TabState_, [Rsn|Rsns]}
                end;
            (_, Acc) ->
                Acc
        end,
    case lists:foldl(TerminateAndDelete, {Data, TabState, []}, Children) of
        {Data2, TabState2, []} ->
            {ok, Data2, TabState2};
        {_Data2, TabState2, Errs} ->
            {error, TabState2, Errs}
    end.


do_terminate_child(Name, Mod, Data, TabMod, TabState, Id_or_Pid) ->
    SearchFunc =
        if
            erlang:is_pid(Id_or_Pid) ->
                lookup_pid;
            true ->
                lookup_id
        end,
    case director_table:SearchFunc(TabMod, TabState, Id_or_Pid) of
        {ok, #?CHILD{pid = Pid, supervisor = Sup}=Child} when Sup =:= self() andalso erlang:is_pid(Pid) ->

            do_terminate_child_2(Name, Mod, Data, TabMod, TabState, Child);
        {ok, _} ->
            {soft_error, TabState, not_parent};
        {soft_error, _, _}=SErr ->
            SErr;
        {hard_error, _}=HErr ->
            HErr
    end.


do_terminate_child_2(Name
                    ,Mod
                    ,Data
                    ,TabMod
                    ,TabState
                    ,#?CHILD{pid=Pid, terminate_timeout = TerminateTimeout}=Child) ->
    receive
        % Child unlinked me and wants to be deleted from my children :(
        {?GEN_CALL_TAG, From, {?DELETE_RUNNING_CHILD_TAG, Pid}} ->
            _ = reply(?DEF_DEBUG_OPTIONS, Name, From, ok),
            case director_table:delete(TabMod, TabState, Child) of
                {ok, TabState2} ->
                    {soft_error, TabState2, not_found};
                {hard_error, _}=HErr ->
                    HErr
            end
    after 0 ->
        case monitor_child(Pid) of
            ok ->
                Rsn =
                    if
                        TerminateTimeout =:= 0 ->
                            kill;
                        true ->
                            shutdown
                    end,
                erlang:exit(Pid, Rsn),
                receive
                    {'DOWN', _, process, Pid, Rsn2} ->
                        do_terminate_child_3(Name, Mod, Data, TabMod, TabState, Child, Rsn2)
                end;
            {error, Rsn2} ->
                do_terminate_child_3(Name, Mod, Data, TabMod, TabState, Child, Rsn2)
        end
    end.


do_terminate_child_3(Name
                    ,Mod
                    ,Data
                    ,TabMod
                    ,TabState
                    ,#?CHILD{state = State
                            ,id = Id
                            ,restart_count = RestartCount
                            ,timer = Timer}=Child
                    ,Rsn) ->
    MetaData = #{restart_count => RestartCount},
    try Mod:handle_terminate(Id, State, Rsn,  Data, MetaData) of
        % {ok, ChState2, Data2, Opts} | {stop, Rsn, ChState2, Opts}
        {El1, El2, El3, Opts} when (El1 =:= ok orelse El2 =:= stop) andalso
                                   erlang:is_list(Opts) ->
            ok = stop_timer(Timer),
            case director_utils:value(log, Opts, ?DEF_LOG) of
                true ->
                    error_logger:error_report(supervisor_report
                                             ,[{supervisor, Name}
                                              ,{errorContext, shutdown_error}
                                              ,{reason, Rsn}
                                              ,{offender, director_child:child_to_proplist(Child)}]);
                _ ->
                    ok
            end,
            Child2 =
                case El1 of
                    ok ->
                        ok = stop_timer(Timer),
                        Child#?CHILD{state = El2
                                    ,pid = undefined
                                    ,extra = undefined
                                    ,supervisor = undefined
                                    ,timer = undefined};
                    _ -> % stop
                        Child#?CHILD{state = El3
                                    ,pid = undefined
                                    ,extra = undefined
                                    ,supervisor = undefined
                                    ,timer = undefined}
                end,
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    case El1 of
                        ok ->
                            {ok, El3, TabState2, Child2};
                        _ -> % stop
                            {hard_error, El2}
                    end;
                {hard_error, _}=HErr ->
                    HErr
            end;
        Other ->
            {hard_error, {return, [{value, Other}
                             ,{module, Mod}
                             ,{function, handle_terminate}
                             ,{arguments, [Id, State, Rsn,  Data, MetaData]}]}}
    catch
        _:Rsn2 ->
            {hard_error, {crash, [{reason, Rsn2}
                                 ,{stacktrace, erlang:get_stacktrace()}
                                 ,{module, Mod}
                                 ,{function, handle_terminate}
                                 ,{arguments, [Id, State, Rsn,  Data, MetaData]}]}}

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


reply(Dbg, Name, {Pid, Tag}, Result) when erlang:is_pid(Pid) ->
    Pid ! {Tag, Result},
    debug(Dbg, Name, {out, Pid, Result});
reply(Dbg, Name, Pid, Result) when erlang:is_pid(Pid) ->
    Pid ! Result,
    debug(Dbg, Name, {out, Pid, Result});
reply(Dbg, Name, Unknown, Result) ->
    error_logger:error_msg("Director ~p could not send response ~p to unknown destination ~p"
                          ,[Name, Result, Unknown]),
    Dbg.


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


handle_restart(Name, Mod, Data, TabMod, TabState, Id) ->
    case director_table:lookup_id(TabMod, TabState, Id) of
        {ok, #?CHILD{pid = Pid}} when erlang:is_pid(Pid) ->
            {soft_error, TabState, {running, Pid}};
        {ok, #?CHILD{pid = restarting}} ->
            {soft_error, TabState, restarting};
        {ok, Child} ->
            handle_start(Name, Mod, Data, TabMod, TabState, Child);
        {soft_error, _, _}=SErr ->
            SErr;
        {hard_error, _}=HErr ->
            HErr
    end.

handle_start(Name, Mod, Data, TabMod, TabState, Child) ->
    Child2 = Child#?CHILD{pid = restarting, extra = undefined, supervisor = erlang:self()},
    case director_table:insert(TabMod, TabState, Child2) of
        {ok, TabState2} ->
            handle_start_2(Name, Mod, Data, TabMod, TabState2, Child2);
        {hard_error, _}=HErr ->
            HErr
    end.


handle_start_2(Name, Mod, Data, TabMod, TabState, #?CHILD{start = {ChMod, Func, Args}}=Child) ->
    try erlang:apply(ChMod, Func, Args) of
        {ok, Pid} when erlang:is_pid(Pid) ->
            handle_start_3(Name
                          ,Mod
                          ,Data
                          ,TabMod
                          ,TabState
                          ,Child#?CHILD{pid = Pid, extra = undefined});
        {ok, Pid, Extra} when erlang:is_pid(Pid) ->
            handle_start_3(Name
                          ,Mod
                          ,Data
                          ,TabMod
                          ,TabState
                          ,Child#?CHILD{pid = Pid, extra = {value, Extra}});
        ignore ->
            handle_start_3(Name
                          ,Mod
                          ,Data
                          ,TabMod
                          ,TabState
                          ,Child#?CHILD{pid = undefined, extra = undefined});
        {error, Rsn} ->
            {soft_error, TabState, Rsn};
        Other ->
            {soft_error, TabState, {return, [{value, Other}
                                            ,{module, Mod}
                                            ,{function, Func}
                                            ,{arguments, Args}]}}
    catch
        _:Rsn ->
            {soft_error, TabState, {crash, [{reason, Rsn}
                                           ,{stacktrace, erlang:get_stacktrace()}
                                           ,{module, Mod}
                                           ,{function, Func}
                                           ,{arguments, Args}]}}
    end.


handle_start_3(Name, Mod, Data, TabMod, TabState, Child) ->
    case director_table:insert(TabMod, TabState, Child) of
        {ok, TabState2} ->
            handle_start_4(Name, Mod, Data, TabMod, TabState2, Child);
        {hard_error, _}=HErr ->
            HErr
    end.


handle_start_4(Name
              ,Mod
              ,Data
              ,TabMod
              ,TabState
              ,#?CHILD{pid = Pid
                      ,extra = Extra
                      ,restart_count = RestartCount
                      ,id = Id
                      ,state = ChState}=Child) ->
    MetaData =
        case {Pid, Extra} of
            {Pid2, undefined} when erlang:is_pid(Pid2) ->
                #{pid => Pid, restart_count => RestartCount};
            {undefined, undefined} ->
                #{restart_count => RestartCount};
            {Pid2, {value, Extra2}} when erlang:is_pid(Pid2) ->
                #{pid => Pid, restart_count => RestartCount, extra => Extra2};
            {undefined, {value, Extra2}} ->
                #{restart_count => RestartCount, extra => Extra2}
        end,
    try Mod:handle_start(Id, ChState, Data, MetaData) of
        % {ok, ChState2, Data2, Opts} | {stop, Rsn, ChState2, Opts}
        {El1, El2, El3, Opts} when (El1 =:= ok orelse El2 =:= stop) andalso
                                   erlang:is_list(Opts) ->
            Child2 =
                case El1 of
                    ok ->
                        Child#?CHILD{state = El2};
                    _ -> % stop
                        Child#?CHILD{state = El3}
                end,
            case director_utils:value(log, Opts, ?DEF_LOG) of
                true ->
                    error_logger:info_report(progress
                                            ,[{supervisor, Name}
                                             ,{started
                                              ,director_child:child_to_proplist(Child2)}]);
                _ ->
                    ok
            end,
            case director_table:insert(TabMod, TabState, Child2) of
                {ok, TabState2} ->
                    case El1 of
                        ok ->
                            {ok, El3, TabState2, Child2};
                        _ -> % stop
                            {hard_error, El2}
                    end;
                {hard_error, _}=HErr ->
                    HErr
            end;
        Other ->
            {hard_error, {return, [{value, Other}
                                  ,{module, Mod}
                                  ,{function, handle_start}
                                  ,{arguments, [Id, ChState, Data, MetaData]}]}}
    catch
        _:Rsn ->
            {hard_error, {crash, [{reason, Rsn}
                            ,{stacktrace, erlang:get_stacktrace()}
                            ,{module, Mod}
                            ,{function, handle_start}
                            ,{arguments, [Id, ChState, Data, MetaData]}]}}

    end.


debug([], _Name, _MsgInfo) ->
    [];
debug(Dbg, Name, MsgInfo) ->
    sys:handle_debug(Dbg, fun print/3, Name, MsgInfo).


print(IODev, {in, Msg}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got message ~tp \n">>), [Name, Msg]);

print(IODev, {out, Dest, Msg}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp sent message ~tp to ~tp\n">>), [Name, Msg, Dest]);

print(IODev, {'EXIT', Pid, Rsn}, Name) ->
    io:format(IODev
             ,add_debug_header(<<"~tp got exit signal from ~tp with reason ~tp \n">>)
             ,[Name, Pid, Rsn]);

print(IODev, {'EXIT', Id}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp exit signal is from child id ~tp\n">>), [Name, Id]);

print(IODev, {request, From, {?GET_PID_TAG, Id}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting pid of child id ~tp\n">>), [Name, pid(From), Id]);

print(IODev, {request, From, ?GET_PIDS_TAG}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting pid of all children\n">>), [Name, pid(From)]);

print(IODev, {request, From, ?COUNT_CHILDREN_TAG}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting count of different types of its children\n">>), [Name, pid(From)]);

print(IODev, {request, From, {?DELETE_CHILD_TAG, Id}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for deleting child id ~tp\n">>), [Name, pid(From), Id]);

print(IODev, {request, From, {?GET_CHILDSPEC_TAG, Id}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting childspec of child id ~tp\n">>), [Name, pid(From), Id]);

print(IODev, {request, From, {?RESTART_CHILD_TAG, Id}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for restarting child id ~tp\n">>), [Name, pid(From), Id]);

print(IODev, {request, From, {?START_CHILD_TAG, ChildSpec}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for starting childspec ~tp\n">>), [Name, pid(From), ChildSpec]);

print(IODev, {request, From, {?TERMINATE_CHILD_TAG, Term}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for terminating child ~tp\n">>), [Name, pid(From), Term]);

print(IODev, {request, From, ?WHICH_CHILDREN_TAG}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting information about all children\n">>), [Name, pid(From)]);

print(IODev, {request, From, {?GET_RESTART_COUNT_TAG, Id}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting restart count of child id ~tp\n">>), [Name, pid(From), Id]);

print(IODev, {request, From, ?GET_DEF_CHILDSPEC}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for getting its default childspec\n">>), [Name, pid(From)]);

print(IODev, {request, From, {?CHANGE_DEF_CHILDSPEC, ChildSpec}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for changing its default childspec to ~tp\n">>), [Name, pid(From), ChildSpec]);

print(IODev, {request, From, {?TERMINATE_AND_DELETE_CHILD_TAG, Term}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for terminating and deleting child id/pid ~tp\n">>), [Name, pid(From), Term]);

print(IODev, {request, From, {?BECOME_SUPERVISOR_TAG, Pid, ChildSpec}}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got request from ~tp for becoming supervisor of ~tp with childspec ~tp\n">>), [Name, pid(From), Pid, ChildSpec]);

print(IODev, {request, From, Req}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got unknown request ~tp from ~tp\n">>), [Name, Req, pid(From)]);

print(IODev, {timer, Id, Time}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp starts timer with ~tp milli-seconds timeout for restarting child id ~tp\n">>), [Name, Time, Id]);

print(IODev, {'DOWN', Ref, Id}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got 'DOWN' signal from timer process ~tp for child id ~tp\n">>), [Name, Ref, Id]);

print(IODev, {system, Msg}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got system message ~tp\n">>), [Name, Msg]);

print(IODev, {parent, Parent, Rsn}, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got exit signal from its parent ~tp for reason ~tp\n">>), [Name, Parent, Rsn]);

print(IODev, Other, Name) ->
    io:format(IODev, add_debug_header(<<"~tp got debug ~tp \n">>), [Name, Other]).


add_debug_header(Txt) ->
    {{Y, Mo, D}, {H, Mi, S}} = erlang:localtime(),
    <<"*DBG* - "
     ,(erlang:integer_to_binary(Y))/binary
     ,"/"
     ,(erlang:integer_to_binary(Mo))/binary
     ,"/"
     ,(erlang:integer_to_binary(D))/binary
     ," "
     ,(erlang:integer_to_binary(H))/binary
     ,":"
     ,(erlang:integer_to_binary(Mi))/binary
     ,":"
     ,(erlang:integer_to_binary(S))/binary
     ," - DIRECTOR "
     ,Txt/binary>>.


pid({Pid, _}) when erlang:is_pid(Pid) ->
    Pid;
pid(Pid) ->
    Pid.


start_timer(Time, Msg) ->
    {Pid, _} = erlang:spawn_monitor(fun() -> exit_after(Time, Msg) end),
    Pid.

exit_after(Time, Rsn) ->
    wait(Time),
    erlang:exit(Rsn).


wait(Time) when Time > 4294967295 -> % 16#FFFFFFFF max timeout of 'receive' statement
    Time2 = erlang:trunc(Time / 2),
    wait(Time2),
    wait(Time2);
wait(Time) ->
    receive
    after Time ->
        ok
    end.


stop_timer(Pid) when erlang:is_pid(Pid) ->
    erlang:exit(Pid, kill),
    ok;
stop_timer(_) ->
    ok.