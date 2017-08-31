
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
%% @version  17.9
%% -------------------------------------------------------------------------------------------------


-module(director_utils).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([debug/3
        ,get_debug_options/3
        ,progress_report/3
        ,error_report/5
        ,run_log_validate_fun/3
        ,check_childspecs/1
        ,check_childspecs/2
        ,check_childspec/2
        ,check_default_childspec/1
        ,filter_plan/1
        ,filter_plan_element/1
        ,is_whole_integer/1
        ,get_log_validate_fun/3
        ,get_table_type/3
        ,combine_child/2
        ,separate_child/2
        ,c2cs/1
        ,c_r2p/2
        ,cs2c/1]).





%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





-include("internal/director_child.hrl").
-include("internal/director_defaults.hrl").





%% -------------------------------------------------------------------------------------------------
%% Functions:





get_debug_options(Name, Opts, Def) ->
    case lists:keyfind(debug, 1, Opts) of
        {_, DbgOpts} ->
            try sys:debug_options(DbgOpts)
            catch _:_ ->
                error_logger:format("~p: ignoring erroneous debug options: ~p~n", [Name, DbgOpts]),
                Def
            end;
        false ->
            Def;
        Other ->
            error_logger:format("~p: ignoring erroneous debug options: ~p~n", [Name, Other]),
            Def
    end.







progress_report(Name, #?CHILD{id = Id}=Child, LogFun) ->
    case run_log_validate_fun(LogFun, Id, start) of
        off ->
            ok;
        LogMode ->
            error_logger:info_report(progress, [{supervisor, Name}
                                               ,{started, c_r2p(Child, LogMode)}])
    end.







error_report(Name, ErrorContext, Reason, #?CHILD{id = Id}=Child, LogFun) ->
    case run_log_validate_fun(LogFun, Id, Reason) of
        off ->
            ok;
        LogMode ->
            error_logger:error_report(supervisor_report
                                     ,[{supervisor, Name}
                                      ,{errorContext, ErrorContext}
                                      ,{reason, Reason}
                                      ,{offender, c_r2p(Child, LogMode)}])
    end.







debug([], _Name, _MsgInfo) ->
    [];
debug(Dbg, Name, MsgInfo) ->
    sys:handle_debug(Dbg, fun print/3, Name, MsgInfo).







check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, ?DEF_DEF_CHILDSPEC).







check_default_childspec(ChildSpec)
    when erlang:is_map(ChildSpec) ->
    Keys = [{start, fun filter_start/1}
           ,{plan, fun filter_plan/1}
           ,{count, fun filter_count/1}
           ,{type, fun filter_type/1}
           ,{terminate_timeout, fun filter_terminate_timeout/1}
           ,{modules, fun filter_modules/1}],
    check_map2(ChildSpec, Keys, #{});
check_default_childspec(Other) ->
    {error, {default_childspec_type, [{childspec, Other}]}}.







get_log_validate_fun(Name, Opts, Def) ->
    case lists:keyfind(log_validate_fun, 1, Opts) of
        false ->
            Def;
        {_, Fun} when erlang:is_function(Fun, 2) ->
            Fun;
        {_, Other} ->
            error_logger:format("~p: ignoring erroneous log validate fun: ~p~n", [Name, Other]),
            Def;
        Other ->
            error_logger:format("~p: ignoring erroneous log validate fun: ~p~n", [Name, Other]),
            Def
    end.







get_table_type(Name, Opts, Def) ->
    case lists:keyfind(table_type, 1, Opts) of
        false ->
            Def;
        {_, list} ->
            list;
        {_, ets} ->
            ets;
        {_, Mode} ->
            error_logger:format("~p: ignoring erroneous table type: ~p~n", [Name, Mode]),
            Def;
        Mode ->
            error_logger:format("~p: ignoring erroneous table type: ~p~n", [Name, Mode]),
            Def
    end.







run_log_validate_fun(ValidateLogFun, Id, Extra) ->
    case catch ValidateLogFun(Id, Extra) of
        off ->
            off;
        short ->
            short;
        long ->
            long;
        {'EXIT', Reason} ->
            error_logger:format("~p: validate log fun crashed: ~p~n", [erlang:self(), Reason]),
            ?DEF_LOG_MODE;
        Other ->
            error_logger:format("~p: ignoring erroneous log mode: ~p~n", [erlang:self(), Other]),
            ?DEF_LOG_MODE
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







c2cs(#?CHILD{id = Id
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







c_r2p(#?CHILD{pid = Pid
             ,id = Id
             ,start = Start
             ,type = Type
             ,terminate_timeout = TerminateTimeout}
     ,off) ->
    [{id, Id}
    ,{pid, Pid}
    ,{mfargs, Start}
    ,{restart_type, temporary}
    ,{shutdown, case TerminateTimeout of
                    0 ->
                        brutal_kill;
                    Timeout ->
                        Timeout
                end}
    ,{child_type, Type}];
c_r2p(#?CHILD{pid = Pid
             ,id = Id
             ,plan = Plan
             ,count = Count
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
    ,{count, Count}
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
             ,append = Append}
     ,long) ->
    [{id, Id}
    ,{pid, Pid}
    ,{plan, Plan}
    ,{count, Count}
    ,{count2, Count2}
    ,{restart_count, ResCount}
    ,{mfargs, Start}
    ,{plan_element_index, PlanElemIndex}
    ,{plan_length, PlanLen}
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
    ,{append, Append}].







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







filter_start({_Mod, _Func, _Args}=Start) ->
    {ok, Start};
filter_start({Mod, Func}) ->
    {ok, {Mod, Func, []}};
filter_start(Other) ->
    {error, {format, [{start, Other}]}}.







combine_child(ChildSpec, DefChildSpec) ->
    case maps:get(append, ChildSpec) of
        true ->
            maps:fold(fun combine_child/3, DefChildSpec, ChildSpec);
        false ->
            ChildSpec
    end.







combine_child(start
             ,{Mod, Func, Args}
             ,#{start := {_Mod2, _Func2, Args2}}=Map) ->
    Map#{start => {Mod, Func, Args2 ++ Args}};
combine_child(count, infinity, Map) ->
    Map#{count => infinity};
combine_child(count, Count, #{count := Count2}=Map) ->
    if
        Count2 =:= infinity ->
            Map#{count => Count};
        true ->
            Map#{count => Count + Count2}
    end;
combine_child(terminate_timeout, infinity, Map) ->
    Map#{terminate_timeout => infinity};
combine_child(terminate_timeout
             ,TerminateTimeout
             ,#{terminate_timeout := TerminateTimeout2}=Map) ->
    if
        TerminateTimeout2 =:= infinity ->
            Map#{terminate_timeout => TerminateTimeout};
        true ->
            Map#{terminate_timeout => TerminateTimeout
                + TerminateTimeout2}
    end;
combine_child(modules, dynamic, Map) ->
    Map#{modules => dynamic};
combine_child(modules, Mods, #{modules := Mods2}=Map) ->
    if
        Mods2 =:= dynamic ->
            Map#{modules => Mods};
        true ->
            Map#{modules => Mods2 ++ Mods}
    end;
combine_child(plan, Plan, #{plan := Plan2}=Map) ->
    Map#{plan => Plan2 ++ Plan};
combine_child(Key, Value, Map) ->
    Map#{Key => Value}.







separate_child(start
              ,{Mod, Func, Args}
              ,#{start := {_Mod2, _Func2, Args2}}=Map) ->
    Map#{start => {Mod, Func, Args -- Args2}};
separate_child(count, infinity, Map) ->
    Map#{count => infinity};
separate_child(count, Count, #{count := Count2}=Map) ->
    if
        Count2 =:= infinity ->
            Map#{count => Count};
        true ->
            Map#{count => Count - Count2}
    end;
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
separate_child(plan, Plan, #{plan := Plan2}=Map) ->
    Map#{plan => Plan -- Plan2};
separate_child(Key, Value, Map) ->
    Map#{Key => Value}.







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

print(IODev, {plan, Id, Strategy}, Name) ->
    io:format(IODev
             ,"*DBG* director ~p is running plan \"~p\" for id \"~p\"~n"
             ,[Name, Strategy, Id]);

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
            ,{count, fun filter_count/1, ?DEF_COUNT}
            ,{type, fun filter_type/1, ?DEF_TYPE}],
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
            case check_map(ChildSpec
                          ,Keys3
                          ,ChildSpec3) of
                {ok, ChildSpec4} ->
                    {ok, cs2c((combine_child(ChildSpec4, DefChildSpec)))};
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;
check_childspec(Other, _DefChildSpec) ->
    {error, {childspec_type, [{childspec, Other}]}}.







filter_plan(Plan) when erlang:is_list(Plan) ->
    filter_plan(Plan, []);
filter_plan(Other) ->
    {error, {plan_type, [{plan, Other}]}}.







filter_plan_element(restart) ->
    {ok, restart};
filter_plan_element({restart, WholeInt}=PlanElem) ->
    case is_whole_integer(WholeInt) of
        true ->
            {ok, PlanElem};
        false ->
            {error, {plan_restart_time_integer, [{plan_element, PlanElem}]}}
    end;
filter_plan_element(delete) ->
    {ok, delete};
filter_plan_element({stop, reason}) ->
    {ok, {stop, reason}};
filter_plan_element({stop, _Reason}=PlanElem) ->
    {ok, PlanElem};
filter_plan_element(stop) ->
    {ok, stop};
filter_plan_element(wait) ->
    {ok, wait};
filter_plan_element(Fun) when erlang:is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 3} ->
            {ok, Fun};
        {arity, Other} ->
            {error, {plan_fun_arity, [{'fun', Fun}, {arity, Other}]}}
    end;
filter_plan_element(Other) ->
    {error, {plan_element_type, [{plan_element, Other}]}}.







filter_plan([PlanElem|Plan], Plan2) ->
    case filter_plan_element(PlanElem) of
        {ok, PlanElem2} ->
            filter_plan(Plan, [PlanElem2|Plan2]);
        {error, _Reason}=Error ->
            Error
    end;
filter_plan([], Plan2) ->
    {ok, lists:reverse(Plan2)}.







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
            {error, {terminate_timeout_range_or_type, [{terminate_timeout, TerminateTimeout}]}}
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







check_childspecs([], _DefChildSpec) ->
    {ok, []};
check_childspecs(ChildSpecs, DefChildSpec) ->
    check_childspecs(ChildSpecs, DefChildSpec, []).