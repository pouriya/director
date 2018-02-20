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
%% @version  18.2.20
%% @hidden
%% -------------------------------------------------------------------------------------------------
-module(director_child).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([child_to_proplist/1
        ,child_to_childspec/1
        ,childspec_to_child/1
        ,check_childspecs/1
        ,check_childspecs/2
        ,check_childspec/2
        ,check_default_childspec/1
        ,combine_child/2
        ,separate_child/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/director_child.hrl").
-include("internal/director_defaults.hrl").

%% -------------------------------------------------------------------------------------------------
%% API Functions:


child_to_proplist(#?CHILD{pid = Pid
                         ,id = Id
                         ,restart_count = ResCount
                         ,start = Start
                         ,timer= Timer
                         ,terminate_timeout = TerminateTimeout
                         ,extra = Extra
                         ,modules = Mods
                         ,type = Type
                         ,append = Append
                         ,supervisor = Sup
                         ,state = State
                         ,delete = DelBeforeTerminate}) ->
    [{id, Id}
    ,{pid, Pid}
    ,{restart_count, ResCount}
    ,{mfargs, Start}
    ,{timer, Timer}
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
    ,{supervisor, Sup}
    ,{state, State}
    ,{delete, DelBeforeTerminate}].


child_to_childspec(#?CHILD{id = Id
                          ,start = Start
                          ,terminate_timeout = TerminateTimeout
                          ,modules = Modules
                          ,type = Type
                          ,append = Append
                          ,state = State
                          ,delete = DelBeforeTerminate}) ->
    #{id => Id
     ,start => Start
     ,terminate_timeout => TerminateTimeout
     ,modules => Modules
     ,type => Type
     ,append => Append
     ,state => State
     ,delete => DelBeforeTerminate}.


childspec_to_child(#{id := Id
                   ,start := Start
                   ,terminate_timeout := TerminateTimeout
                   ,modules := Mods
                   ,type := Type
                   ,append := Append
                   ,state := State
                   ,delete := DelBeforeTerminate}) ->
    #?CHILD{id = Id
           ,pid = undefined
           ,restart_count = 0
           ,start = Start
           ,timer = undefined
           ,terminate_timeout = TerminateTimeout
           ,extra = undefined
           ,modules = Mods
           ,type = Type
           ,append = Append
           ,supervisor = erlang:self()
           ,state = State
           ,delete = DelBeforeTerminate}.


check_childspecs(ChildSpecs) ->
    check_childspecs(ChildSpecs, ?DEF_DEF_CHILDSPEC).


check_childspecs([], _DefChildSpec) ->
    {ok, []};
check_childspecs(ChildSpecs, DefChildSpec) ->
    check_childspecs(ChildSpecs, DefChildSpec, []).


check_default_childspec(ChildSpec) when erlang:is_map(ChildSpec) ->
    Keys = [{start, fun filter_start/1}
           ,{count, fun filter_count/1}
           ,{type, fun filter_type/1}
           ,{terminate_timeout, fun filter_terminate_timeout/1}
           ,{modules, fun filter_modules/1}
           ,{state, fun(St) -> {ok, St} end}
           ,{delete, fun filter_delete/1}],
    check_map2(ChildSpec, Keys, #{});
check_default_childspec(Other) ->
    {error, {default_childspec_type, [{childspec, Other}]}}.



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
            ,{type, fun filter_type/1, ?DEF_TYPE}
            ,{state, fun(St) -> {ok, St} end, ?DEF_CHILDSPEC_STATE}
            ,{delete, fun filter_delete/1, ?DEF_DELETE_BEFORE_TERMINATE}],
    case check_map(ChildSpec, Keys2, ChildSpec2) of
        {ok, ChildSpec3} ->
            DefTerminateTimeout =
                case maps:get(type, ChildSpec3) of
                    worker ->
                        ?DEF_WORKER_TERMINATE_TIMEOUT;
                    _ -> % supervisor
                        ?DEF_SUPERVISOR_TERMINATE_TIMEOUT
                end,
            DefMods = [erlang:element(1, maps:get(start, ChildSpec3))],
            Keys3 = [{terminate_timeout, fun filter_terminate_timeout/1, DefTerminateTimeout}
                    ,{modules, fun filter_modules/1, DefMods}],
            case check_map(ChildSpec, Keys3, ChildSpec3) of
                {ok, ChildSpec4} ->
                    {ok, childspec_to_child((combine_child(ChildSpec4, DefChildSpec)))};
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;
check_childspec(Other, _DefChildSpec) ->
    {error, {childspec_value, [{childspec, Other}]}}.


separate_child(ChildSpec, DefChildSpec) ->
    case maps:get(append, ChildSpec) of
        true ->
            maps:fold(fun separate_child/3, DefChildSpec, ChildSpec);
        _ -> % false
            ChildSpec
    end.


combine_child(ChildSpec, DefChildSpec) ->
    case maps:get(append, ChildSpec) of
        true ->
            maps:fold(fun combine_child/3, DefChildSpec, ChildSpec);
        _ -> % false
            ChildSpec
    end.
%% -------------------------------------------------------------------------------------------------
%% Internal functions:

check_childspecs([Elem|Elems], DefChildSpec, Children) ->
    case check_childspec(Elem, DefChildSpec) of
        {ok, ChildSpec} ->
            check_childspecs(Elems, DefChildSpec, [ChildSpec|Children]);
        {error, _}=Err ->
            Err
    end;
check_childspecs([], _DefChildSpec, Children) ->
    {ok, lists:reverse(Children)};
check_childspecs(Other, _DefChildSpec, _Children) ->
    {error, {childspecs_type, [{childspecs, Other}]}}.


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


filter_delete(Bool) when erlang:is_boolean(Bool) ->
    {ok, Bool};
filter_delete(Other) ->
    {error, {childspec_value, [{delete, Other}]}}.


check_map(ChildSpec, [{Key, Filter, DEF}|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            case Filter(Value) of
                {ok, Value2} ->
                    check_map(ChildSpec, Keys, maps:put(Key, Value2, ChildSpec2));
                {error, Rsn} ->
                    {error, {childspec_value, [{key, Key}, {reason, Rsn}]}}
            end
    catch
        _:_ ->
            check_map(ChildSpec, Keys, maps:put(Key, DEF, ChildSpec2))

    end;
check_map(ChildSpec, [{Key, Filter}|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            case Filter(Value) of
                {ok, Value2} ->
                    check_map(ChildSpec, Keys, maps:put(Key, Value2, ChildSpec2));
                {error, Rsn} ->
                    {error, {childspec_value, [{key, Key}, {reason, Rsn}]}}
            end
    catch
        _:_->
            {error, {key_not_found, [{key, Key}, {childspec, ChildSpec}]}}
    end;
check_map(ChildSpec, [Key|Keys], ChildSpec2) ->
    try maps:get(Key, ChildSpec) of
        Value ->
            check_map(ChildSpec, Keys, maps:put(Key, Value, ChildSpec2))
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
                    check_map2(ChildSpec, Keys, maps:put(Key, Value2, ChildSpec2));
                {error, Rsn} ->
                    {error, {childspec_value, [{key, Key}, {reason, Rsn}]}}
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


combine_child(start, {Mod, Func, Args}, #{start := {Mod2, Func2, Args2}}=Map) ->
    if
        Mod =:= Mod2 andalso Func =:= Func2 ->
            Map#{start => {Mod, Func, director_utils:concat(Args2, Args)}};
        true ->
            Map
    end;
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
            Map#{modules => director_utils:concat(Mods2, Mods)};
        true ->
            Map
    end;
combine_child(type, _Type, #{type := _Type2}=Map) ->
    Map;
combine_child(state, _State, #{state := _State2}=Map) ->
    Map;
combine_child(delete, _DelBeforeTerminate, #{delete := _DelBeforeTerminate2}=Map) ->
    Map;
combine_child(Key, Val, Map) ->
    maps:put(Key, Val, Map).


separate_child(start, {Mod, Func, Args}, #{start := {_Mod2, _Func2, Args2}}=Map) ->
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
            Map#{terminate_timeout => TerminateTimeout - TerminateTimeout2}
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