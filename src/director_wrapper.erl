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
%% @version  17.6.4
%% @doc
%%           Wrapper functions.
%% @end
%% @hidden
%% ---------------------------------------------------------------------


-module(director_wrapper).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([combine_child/2
        ,separate_child/2
        ,c2cs/1
        ,c_r2p/1
        ,cs2c/1]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





%% Dependencies:
%%  ?CHILD{} record
-include("internal/director_child.hrl").





%% ---------------------------------------------------------------------
%% API functions:





combine_child(ChildSpec, DefChildSpec) ->
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
    maps:fold(Fun, DefChildSpec, ChildSpec).







separate_child(ChildSpec, DefChildSpec) ->
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