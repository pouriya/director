
%%% ------------------------------------------------------------------------------------------------
%%% Director is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2016-2017, Pouriya Jahanbakhsh
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
%% @version  17.11.30
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(director_utils).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([concat/2
        ,debug/3
        ,get_debug_options/2
        ,progress_report/2
        ,error_report/4
        ,run_log_validator/5
        ,check_childspecs/1
        ,check_childspecs/2
        ,check_childspec/2
        ,check_default_childspec/1
        ,filter_plan/1
        ,is_whole_integer/1
        ,get_log_validator/2
        ,get_table_module/2
        ,get_table_init_argument/2
        ,combine_child/2
        ,separate_child/2
        ,c2cs/1
        ,c_r2p/2
        ,cs2c/1
        ,get_delete_table_before_terminate/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/director_child.hrl").
-include("internal/director_defaults.hrl").

%% -------------------------------------------------------------------------------------------------
%% Functions:

concat(List1, List2) ->
    concat2(lists:reverse(make_properlist(List1)), make_properlist(List2)).


get_debug_options(Name, Opts) ->
    case lists:keyfind(debug, 1, Opts) of
        {_, DbgOpts} ->
            try
                sys:debug_options(DbgOpts)
            catch
                _:_ ->
                    error_logger:format("~p: ignoring erroneous debug options: ~p~n"
                                       ,[Name, DbgOpts]),
                    ?DEF_DEBUG_OPTIONS
            end;
        false ->
            ?DEF_DEBUG_OPTIONS;
        Other ->
            error_logger:format("~p: ignoring erroneous debug options: ~p~n", [Name, Other]),
            ?DEF_DEBUG_OPTIONS
    end.


get_delete_table_before_terminate(Name, Opts) ->
    case lists:keyfind(delete_table_before_terminate, 1, Opts) of
        false ->
            ?DEF_DELETE_TABLE_BEFORE_TERMINATE;
        {_, Bool} when erlang:is_boolean(Bool) ->
            Bool;
        {_, Other} ->
            error_logger:format("~p: ignoring erroneous flag for deleting table before "
                                "termination: ~p~n", [Name, Other]),
            ?DEF_LOG_VALIDATOR;
        Other ->
            error_logger:format("~p: ignoring erroneous flag for deleting table before "
                                "termination: ~p~n", [Name, Other]),
            ?DEF_LOG_VALIDATOR
    end.


progress_report(Name, #?CHILD{id = Id, log_validator = LogValidator, state = State}=Child) ->
    case run_log_validator(LogValidator, Id, info, start, State) of
        {none, NewState} ->
            NewState;
        {LogMode, NewState} ->
            error_logger:info_report(progress, [{supervisor, Name}
                                               ,{started, c_r2p(Child, LogMode)}]),
            NewState
    end.


error_report(Name, ErrorContext, Reason, #?CHILD{id = Id, log_validator = LogValidator, state = State}=Child) ->
    case run_log_validator(LogValidator, Id, error, Reason, State) of
        {none, NewState} ->
            NewState;
        {LogMode, NewState} ->
            error_logger:error_report(supervisor_report
                                     ,[{supervisor, Name}
                                      ,{errorContext, ErrorContext}
                                      ,{reason, Reason}
                                      ,{offender, c_r2p(Child, LogMode)}]),
            NewState
    end.


debug([], _Name, _MsgInfo) ->
    [];
debug(Dbg, Name, MsgInfo) ->
    sys:handle_debug(Dbg, fun print/3, Name, MsgInfo).


check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, ?DEF_DEF_CHILDSPEC).


check_default_childspec(ChildSpec) when erlang:is_map(ChildSpec) ->
    Keys = [{start, fun filter_start/1}
           ,{plan, fun filter_plan/1}
           ,{count, fun filter_count/1}
           ,{type, fun filter_type/1}
           ,{terminate_timeout, fun filter_terminate_timeout/1}
           ,{modules, fun filter_modules/1}
           ,{log_validator, fun filter_log_validator/1}
           ,{state, fun(St) -> {ok, St} end}
           ,{delete_before_terminate, fun filter_delete_before_terminate/1}],
    check_map2(ChildSpec, Keys, #{});
check_default_childspec(Other) ->
    {error, {default_childspec_type, [{childspec, Other}]}}.


get_log_validator(Name, Opts) ->
    case lists:keyfind(log_validator, 1, Opts) of
        false ->
            ?DEF_LOG_VALIDATOR;
        {_, Fun} when erlang:is_function(Fun, 4) ->
            Fun;
        {_, Other} ->
            error_logger:format("~p: ignoring erroneous log validator: ~p~n", [Name, Other]),
            ?DEF_LOG_VALIDATOR;
        Other ->
            error_logger:format("~p: ignoring erroneous log validator: ~p~n", [Name, Other]),
            ?DEF_LOG_VALIDATOR
    end.


get_table_module(Name, Opts) ->
    case lists:keyfind(table_module, 1, Opts) of
        false ->
            ?DEF_TABLE_MOD;
        {_, Mod} when erlang:is_atom(Mod) ->
            Mod;
        {_, Other} ->
            error_logger:format("~p: ignoring erroneous table module: ~p~n", [Name, Other]),
            ?DEF_TABLE_MOD;
        Other ->
            error_logger:format("~p: ignoring erroneous table module: ~p~n", [Name, Other]),
            ?DEF_TABLE_MOD
    end.


get_table_init_argument(Name, Opts) ->
    case lists:keyfind(table_init_argument, 1, Opts) of
        false ->
            ?DEF_TABLE_INIT_ARG;
        {_, InitArg}  ->
            {value, InitArg};
        Other ->
            error_logger:format("~p: ignoring erroneous table init argument: ~p~n", [Name, Other]),
            ?DEF_TABLE_INIT_ARG
    end.


run_log_validator(Validator, Id, Lvl, Extra, State) ->
    try Validator(Id, Lvl, Extra, State) of
        {Val, _}=Ok when Val == none orelse Val == short orelse Val == long ->
            Ok;
        Val when Val == none orelse Val == short orelse Val == long ->
            {Val, State};
        Other ->
            error_logger:format("~p: ignoring erroneous log mode: ~p~n", [erlang:self(), Other]),
            {?DEF_LOG_MODE, State}
    catch
        _:Rsn ->
            error_logger:format("Child id ~p: log validator crashed with reason ~p and stacktrace ~p~n"
                               ,[Id, Rsn, erlang:get_stacktrace()]),
            {?DEF_LOG_MODE, State}
    end.


check_childspecs([Elem|Elems], DefChildSpec, Children) ->
    case check_childspec(Elem, DefChildSpec) of
        {ok, ChildSpec} ->
            check_childspecs(Elems
                            ,DefChildSpec
                            ,[ChildSpec|Children]);
        {error, _Reason}=Error ->
            Error
    end;
check_childspecs([], _DefChildSpec, Children) ->
    {ok, lists:reverse(Children)};
check_childspecs(Other, _DefChildSpec, _Children) ->
    {error, {childspecs_type, [{childspecs, Other}]}}.


separate_child(ChildSpec, DefChildSpec) ->
    case maps:get(append, ChildSpec) of
        true ->
            maps:fold(fun separate_child/3, DefChildSpec, ChildSpec);
        false ->
            ChildSpec
    end.


cs2c(#{id := Id
     ,plan := Plan
     ,start := Start
     ,terminate_timeout := TerminateTimeout
     ,modules := Mods
     ,type := Type
     ,append := Append
     ,log_validator := LogValidator
     ,state := State
     ,delete_before_terminate := DelBeforeTerminate}) ->
    #?CHILD{id = Id
           ,pid = undefined
           ,plan = Plan
           ,restart_count = 0
           ,start = Start
           ,timer_reference = undefined
           ,terminate_timeout = TerminateTimeout
           ,extra = undefined
           ,modules = Mods
           ,type = Type
           ,append = Append
           ,log_validator = LogValidator
           ,supervisor = erlang:self()
           ,state = State
           ,delete_before_terminate = DelBeforeTerminate}.



c2cs(#?CHILD{id = Id
            ,start = Start
            ,plan = Plan
            ,terminate_timeout = TerminateTimeout
            ,modules = Modules
            ,type = Type
            ,append = Append
            ,log_validator = LogValidator
            ,state = State
            ,delete_before_terminate = DelBeforeTerminate}) ->
    #{id => Id
    ,start => Start
    ,plan => Plan
    ,terminate_timeout => TerminateTimeout
    ,modules => Modules
    ,type => Type
    ,append => Append
    ,log_validator => LogValidator
    ,state => State
    ,delete_before_terminate => DelBeforeTerminate}.


c_r2p(#?CHILD{pid = Pid
             ,id = Id
             ,plan = Plan
             ,restart_count = ResCount
             ,start = Start
             ,terminate_timeout = TerminateTimeout
             ,modules = Mods
             ,type = Type
             ,append = Append}
     ,short) ->
    [{id, Id}
    ,{pid, Pid}
    ,{plan, Plan}
    ,{restart_count, ResCount}
    ,{mfargs, Start}
    ,{restart_type, temporary}
    ,{shutdown, case TerminateTimeout of
                    0 ->
                        brutal_kill;
                    Timeout ->
                        Timeout
                end}
    ,{child_type, Type}
    ,{modules, Mods}
    ,{append, Append}];
c_r2p(#?CHILD{pid = Pid
             ,id = Id
             ,plan = Plan
             ,restart_count = ResCount
             ,start = Start
             ,timer_reference = TimerRef
             ,terminate_timeout = TerminateTimeout
             ,extra = Extra
             ,modules = Mods
             ,type = Type
             ,append = Append
             ,log_validator = LogValidator
             ,supervisor = Sup
             ,state = State
             ,delete_before_terminate = DelBeforeTerminate}
     ,long) ->
    [{id, Id}
    ,{pid, Pid}
    ,{plan, Plan}
    ,{restart_count, ResCount}
    ,{mfargs, Start}
    ,{timer_reference, TimerRef}
    ,{restart_type, temporary}
    ,{shutdown, case TerminateTimeout of
                    0 ->
                        brutal_kill;
                    Timeout ->
                        Timeout
                end}
    ,{child_type, Type}
    ,{extra, Extra}
    ,{modules, Mods}
    ,{append, Append}
    ,{log_validator, LogValidator}
    ,{supervisor, Sup}
    ,{state, State}
    ,{delete_before_terminate, DelBeforeTerminate}].


check_map(ChildSpec, [{Key, Filter, DEF}|Keys], ChildSpec2) ->
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
                    {error, {childspec_value, [{key, Key}, {reason, Reason}]}}
            end
    catch
        _:_ ->
            check_map(ChildSpec
                     ,Keys
                     ,maps:put(Key
                              ,DEF
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
                    {error, {childspec_value, [{key, Key}, {reason, Reason}]}}
            end
    catch
        _:_->
            {error, {key_not_found, [{key, Key}, {childspec, ChildSpec}]}}
    end;
check_map(ChildSpec, [Key|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            check_map(ChildSpec
                     ,Keys
                     ,maps:put(Key, Value, ChildSpec2))
    catch
        _:_ ->
            {error, {key_not_found, [{key, Key}, {childspec, ChildSpec}]}}
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
                    {error, {childspec_value, [{key, Key}, {reason, Reason}]}}
            end
    catch
        _:_ ->
            check_map2(ChildSpec, Keys, ChildSpec2)
    end;
check_map2(_ChidlSpec, [], ChildSpec2) ->
    {ok, ChildSpec2}.


filter_start({Mod, Func, Args}=Start)when erlang:is_atom(Mod) andalso
    erlang:is_atom(Func) andalso
    erlang:is_list(Args) ->
    {ok, Start};
filter_start({Mod, Func}) when erlang:is_atom(Mod) andalso erlang:is_atom(Func) ->
    {ok, {Mod, Func, []}};
filter_start(Mod) when erlang:is_atom(Mod) ->
    {ok, {Mod, start_link, []}};
filter_start(Other) ->
    {error, {childspec_value, [{start, Other}]}}.


combine_child(ChildSpec, DefChildSpec) ->
    case maps:get(append, ChildSpec) of
        true ->
            maps:fold(fun combine_child/3, DefChildSpec, ChildSpec);
        false ->
            ChildSpec
    end.


combine_child(start
             ,{Mod, Func, Args}
             ,#{start := {Mod2, Func2, Args2}}=Map) ->
    if
        Mod =:= Mod2 andalso Func =:= Func2 ->
            Map#{start => {Mod, Func, concat(Args2, Args)}};
        true ->
            Map
    end;
combine_child(plan, _Plan, #{plan := _Plan2}=Map) ->
    Map;
combine_child(terminate_timeout, TerminateTimeout, #{terminate_timeout := TerminateTimeout2}=Map) ->
    if
        erlang:is_integer(TerminateTimeout) andalso erlang:is_integer(TerminateTimeout2) ->
            Map#{terminate_timeout => TerminateTimeout2 + TerminateTimeout};
        true ->
            Map
    end;
combine_child(modules, Mods, #{modules := Mods2}=Map) ->
    if
        erlang:is_list(Mods) andalso erlang:is_list(Mods2) ->
            Map#{modules => concat(Mods2, Mods)};
        true ->
            Map
    end;
combine_child(type, _Type, #{type := _Type2}=Map) ->
    Map;
combine_child(log_validator, _LogValidator, #{log_validator := _LogValidator2}=Map) ->
    Map;
combine_child(state, _State, #{state := _State2}=Map) ->
    Map;
combine_child(delete_before_terminate
             ,_DelBeforeTerminate
             ,#{delete_before_terminate := _DelBeforeTerminate2}=Map) ->
    Map;
combine_child(Key, Val, Map) ->
    maps:put(Key, Val, Map).


separate_child(start
              ,{Mod, Func, Args}
              ,#{start := {_Mod2, _Func2, Args2}}=Map) ->
    Map#{start => {Mod, Func, Args -- Args2}};
separate_child(terminate_timeout, infinity, Map) ->
    Map#{terminate_timeout => infinity};
separate_child(terminate_timeout
              ,TerminateTimeout
              ,#{terminate_timeout := TerminateTimeout2}=Map) ->
    if
        TerminateTimeout2 =:= infinity ->
            Map#{terminate_timeout => TerminateTimeout};
        true ->
            Map#{terminate_timeout => TerminateTimeout
                - TerminateTimeout2}
    end;
separate_child(modules, dynamic, Map) ->
    Map#{modules => dynamic};
separate_child(modules, Mods, #{modules := Mods2}=Map) ->
    if
        Mods2 =:= dynamic ->
            Map#{modules => Mods};
        true ->
            Map#{modules => Mods -- Mods2}
    end;
separate_child(Key, Value, Map) ->
    maps:put(Key, Value, Map).


print(IODev, {in, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p got message \"~p\" ~n"
             ,[Name, Msg]);

print(IODev, {?GEN_CALL_TAG, {Pid, Tag}, Request}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p got request \"~p\" from \"~p\" wit"
              "h tag \"~p\" ~n"
             ,[Name, Request, Pid, Tag]);

print(IODev, {'EXIT', Pid, Reason}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p got exit signal for pid \"~p\" wit"
              "h reason \"~p\"~n"
             ,[Name, Pid, Reason]);

print(IODev, {timeout, Ref, Id}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p got restart event for child-id \"~"
              "p\" with timer reference \"~p\"~n"
             ,[Name, Id, Ref]);

print(IODev, {out, To, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p sent \"~p\" to \"~p\"~n"
             ,[Name, Msg, To]);

print(IODev, {plan, Id, {Strategy, State}}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p is running plan \"~p\" for id \"~p\" with state \"~p\"~n"
             ,[Name, Strategy, Id, State]);

print(IODev, Other, Name) ->
    io:format(IODev
             ,"*DBG* director ~p got debug \"~p\" ~n"
             ,[Name, Other]).


check_childspec(ChildSpec, DefChildSpec) when erlang:is_map(ChildSpec) ->
    Keys = [{append, fun filter_append/1, ?DEF_APPEND}],
    {ok, #{append := Append}} = {ok, ChildSpec2} = check_map(ChildSpec, Keys, #{}),
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
            ,{plan, fun filter_plan/1, ?DEF_PLAN}
            ,{type, fun filter_type/1, ?DEF_TYPE}
            ,{log_validator, fun filter_log_validator/1, ?DEF_LOG_VALIDATOR}
            ,{state, fun(St) -> {ok, St} end, ?DEF_CHILDSPEC_STATE}
            ,{delete_before_terminate
             ,fun filter_delete_before_terminate/1
             ,?DEF_DELETE_BEFORE_TERMINATE}],
    case check_map(ChildSpec, Keys2, ChildSpec2) of
        {ok, ChildSpec3} ->
            DefTerminateTimeout =
                case maps:get(type, ChildSpec3) of
                    worker ->
                        ?DEF_WORKER_TERMINATE_TIMEOUT;
                    supervisor ->
                        ?DEF_SUPERVISOR_TERMINATE_TIMEOUT
                end,
            DefMods = [erlang:element(1, maps:get(start, ChildSpec3))],
            Keys3 = [{terminate_timeout, fun filter_terminate_timeout/1, DefTerminateTimeout}
                    ,{modules, fun filter_modules/1, DefMods}],
            case check_map(ChildSpec, Keys3, ChildSpec3) of
                {ok, ChildSpec4} ->
                    {ok, cs2c((combine_child(ChildSpec4, DefChildSpec)))};
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;
check_childspec(Other, _DefChildSpec) ->
    {error, {childspec_value, [{childspec, Other}]}}.


filter_plan(F) when erlang:is_function(F) ->
    case erlang:fun_info(F, arity) of
        {arity, 4} ->
            {ok, F};
        {arity, Other} ->
            {error, {childspec_value, [{plan, F}, {arity, Other}]}}
    end;
filter_plan(F) ->
    {error, {childspec_value, [{plan, F}]}}.


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
            {error, {childspec_value, [{count, Count}]}}
    end.


filter_terminate_timeout(infinity) ->
    {ok, infinity};
filter_terminate_timeout(TerminateTimeout) ->
    case is_whole_integer(TerminateTimeout) of
        true ->
            {ok, TerminateTimeout};
        false ->
            {error, {childspec_value, [{terminate_timeout, TerminateTimeout}]}}
    end.


filter_type(worker) ->
    {ok, worker};
filter_type(supervisor) ->
    {ok, supervisor};
filter_type(sup) ->
    {ok, supervisor};
filter_type(w) ->
    {ok, worker};
filter_type(s) ->
    {ok, supervisor};
filter_type(Other) ->
    {error, {childspec_value, [{type, Other}]}}.


filter_modules(dynamic) ->
    {ok, dynamic};
filter_modules(Mod) when erlang:is_atom(Mod) ->
    {ok, [Mod]};
filter_modules(Mods) when erlang:is_list(Mods) ->
    {ok, Mods};
filter_modules(Other) ->
    {error, {childspec_value, [{modules, Other}]}}.


filter_append(Bool) when erlang:is_boolean(Bool) ->
    {ok, Bool};
filter_append(Other) ->
    {error, {childspec_value, [{append, Other}]}}.




filter_log_validator(F) when erlang:is_function(F) ->
    case erlang:fun_info(F, arity) of
        {arity, 4} ->
            {ok, F};
        {arity, Other} ->
            {error, {childspec_value, [{log_validator, F}, {arity, Other}]}}
    end;
filter_log_validator(F) ->
    {error, {childspec_value, [{log_validator, F}]}}.


filter_delete_before_terminate(Bool) when erlang:is_boolean(Bool) ->
    {ok, Bool};
filter_delete_before_terminate(Other) ->
    {error, {childspec_value, [{delete_before_terminate, Other}]}}.


check_childspecs([], _DefChildSpec) ->
    {ok, []};
check_childspecs(ChildSpecs, DefChildSpec) ->
    check_childspecs(ChildSpecs, DefChildSpec, []).




concat2([Item|List1], List2) ->
    concat2(List1, [Item|List2]);
concat2([], List) ->
    List.


make_properlist(L) ->
    make_properlist(L, []).

make_properlist([H|T], Ret) when erlang:is_list(T) ->
    make_properlist(T, [H | Ret]);
make_properlist([H|T], Ret) ->
    make_properlist([], [T, H | Ret]);
make_properlist([], Ret) ->
    lists:reverse(Ret).