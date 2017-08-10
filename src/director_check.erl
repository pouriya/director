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
%%           Check functions for director.
%% @end
%% @hidden
%% ---------------------------------------------------------------------


-module(director_check).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([check_childspecs/1
        ,check_childspecs/2
        ,check_childspec/2
        ,check_default_childspec/1
        ,filter_plan/1
        ,filter_plan_element/1
        ,is_whole_integer/1
        ,get_debug_mode/3
        ,get_table_type/3]).






%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





%% Dependencies:
%%  ?DEFAULT_PLAN
%%  ?DEFAULT_COUNT
%%  ?DEFAULT_TYPE
%%  ?DEFAULT_WORKER_TERMINATE_TIMEOUT
%%  ?DEFAULT_SUPERVISOR_TERMINATE_TIMEOUT
%%  ?DEFAULT_DEFAULT_CHILDSPEC
-include("internal/director_defaults.hrl").





%% ---------------------------------------------------------------------
%% API:






check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, ?DEFAULT_DEFAULT_CHILDSPEC).







check_childspec(ChildSpec
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
                    {ok, director_wrapper:
                         cs2c((director_wrapper:
                               combine_child(ChildSpec4
                                            ,DefChildSpec)))};
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;
check_childspec(Other, _DefChildSpec) ->
    {error, {childspec_type, [{childspec, Other}]}}.







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
            {error
            ,{plan_restart_time_integer, [{plan_element, PlanElem}]}}
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
        {arity, 2} ->
            {ok, Fun};
        {arity, Other} ->
            {error, {plan_fun_arity, [{'fun', Fun}, {arity, Other}]}}
    end;
filter_plan_element(Other) ->
    {error, {plan_element_type, [{plan_element, Other}]}}.







get_debug_mode(Name, Opts, Def) ->
    case lists:keyfind(debug_mode, 1, Opts) of
        false ->
            Def;
        {_, long} ->
            long;
        {_, short} ->
            short;
        {_, off} ->
            off;
        {_, Mode} ->
            error_logger:format(
                "~p: ignoring erroneous debug mode - ~p~n",
                [Name, Mode]),
            Def;
        Mode ->
            error_logger:format(
                "~p: ignoring erroneous debug mode - ~p~n",
                [Name, Mode]),
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
            error_logger:format(
                "~p: ignoring erroneous table type - ~p~n",
                [Name, Mode]),
            Def;
        Mode ->
            error_logger:format(
                "~p: ignoring erroneous table type - ~p~n",
                [Name, Mode]),
            Def
    end.





%% ---------------------------------------------------------------------
%% Internal functions:





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







check_childspecs([], _DefChildSpec) ->
    {ok, []};
check_childspecs(ChildSpecs, DefChildSpec) ->
    check_childspecs(ChildSpecs, DefChildSpec, []).







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
