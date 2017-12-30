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
%% @version  17.12.30
%% -------------------------------------------------------------------------------------------------
-module(director_table_).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

-export(['1'/2
        ,'2'/2
        ,'3'/2
        ,'4'/2
        ,'5'/2
        ,'6'/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(M, 'director_table').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("internal/director_child.hrl").

%% -------------------------------------------------------------------------------------------------

'1'(Mod, InitArg) ->
    Create_Delete =
        fun(_) ->
            TabState = create(Mod, InitArg),
            count(Mod, TabState, 0),
            delete_table(Mod, TabState)
        end,
    lists:foreach(Create_Delete, lists:seq(1, 10)).


'2'(Mod, InitArg) ->
    TabState = create(Mod, InitArg),
    Id = id,
    Child = #?CHILD{id = Id},
    TabState2 = insert(Mod, TabState, Child),
    lookup_id(Mod, TabState2, Id, Child),
    count(Mod, TabState2, 1),

    TabState3 = delete(Mod, TabState2, Child),
    count(Mod, TabState3, 0).


'3'(Mod, InitArg) ->
    TabState = create(Mod, InitArg),
    Count = 100,
    Children = [#?CHILD{id = Int, pid = Int} || Int <- lists:seq(1, Count)],
    Fold =
        fun(Child, TabState2) ->
            insert(Mod, TabState2, Child)
        end,
    TabState2 = lists:foldl(Fold, TabState, Children),
    count(Mod, TabState2, Count),

    Fold2 =
        fun(#?CHILD{id = Id2}=Child, TabState3) ->
            lookup_id(Mod, TabState3, Id2, Child),
            delete(Mod, TabState3, Child)
        end,
    TabState3 = lists:foldl(Fold2, TabState2, Children),
    count(Mod, TabState3, 0).


'4'(Mod, InitArg) ->
    TabState = create(Mod, InitArg),
    Count = 100,
    Children = [#?CHILD{id = Int, pid = Int} || Int <- lists:seq(1, Count)],
    Fold =
        fun(Child, TabState2) ->
            insert(Mod, TabState2, Child)
        end,
    TabState2 = lists:foldl(Fold, TabState, Children),
    count(Mod, TabState2, Count),

    Fold2 =
        fun(#?CHILD{pid = Pid2}=Child, TabState3) ->
            lookup_pid(Mod, TabState3, Pid2, Child),
            delete(Mod, TabState3, Child)
        end,
    TabState3 = lists:foldl(Fold2, TabState2, Children),
    count(Mod, TabState3, 0).



'5'(Mod, InitArg) ->
    [Child1, Child2, Child3] = Children = [#?CHILD{id = 1}, #?CHILD{id = 2}, #?CHILD{id = 3}],
    TabState = create(Mod, InitArg),
    Fold =
        fun(Child, TabState2) ->
            insert(Mod, TabState2, Child)
        end,
    TabState2 = lists:foldl(Fold, TabState, Children),
    count(Mod, TabState2, 3),
    Rslt = ?M:tab2list(Mod, TabState2),
    ?assertMatch({ok, _}, Rslt),
    {ok, List} = Rslt,
    ?assert(erlang:is_list(List)),
    ?assert(erlang:length(List) =:= 3),
    ?assertEqual(Child1, lists:keyfind(1, 2, List)),
    ?assertEqual(Child2, lists:keyfind(2, 2, List)),
    ?assertEqual(Child3, lists:keyfind(3, 2, List)).


'6'(Mod, InitArg) ->
    TabState = create(Mod, InitArg),
    Count = 100,
    Children = [#?CHILD{id = id, append = false, supervisor = erlang:self()} | [#?CHILD{id = Int, append = true, modules = [], supervisor = erlang:self()} || Int <- lists:seq(1, Count)]],
    Fold =
        fun(Child, TabState2) ->
            insert(Mod, TabState2, Child)
        end,
    TabState2 = lists:foldl(Fold, TabState, Children),

    Val = [?MODULE],
    TabState3 = combine(Mod, TabState2, #{modules => Val}),

    Foreach =
        fun(Id) ->
            lookup_id(Mod, TabState3, Id, #?CHILD{id = Id, modules = Val, append = true, restart_count = 0, supervisor = erlang:self()})
        end,
    lists:foreach(Foreach, lists:seq(1, Count)),
    lookup_id(Mod, TabState3, id, #?CHILD{id = id, append = false, supervisor = erlang:self()}),

    TabState4 = separate(Mod, TabState3, #{modules => Val}),

    Foreach2 =
        fun(Id) ->
            lookup_id(Mod, TabState4, Id, #?CHILD{id = Id, append = true, modules = [], restart_count = 0, supervisor = erlang:self()})
        end,
    lists:foreach(Foreach2, lists:seq(1, Count)),
    lookup_id(Mod, TabState4, id, #?CHILD{id = id, append = false, supervisor = erlang:self()}).

%% -------------------------------------------------------------------------------------------------

create(Mod, InitArg) ->
    Res = ?M:create(Mod, InitArg),
    ?assertMatch({ok, _}, Res),
    erlang:element(2, Res).


count(Mod, TabState, Count) ->
    Rslt = ?M:count(Mod, TabState),
    ?assertEqual({ok, Count}, Rslt),
    erlang:element(2, Rslt).


delete_table(Mod, TabState) ->
    ?assertEqual(ok, ?M:delete_table(Mod, TabState)).


insert(Mod, TabState, Child) ->
    Res = ?M:insert(Mod, TabState, Child),
    ?assertMatch({ok, _}, Res),
    erlang:element(2, Res).


lookup_id(Mod, TabState, Id, Rslt) ->
    Res = ?M:lookup_id(Mod, TabState, Id),
    ?assertEqual({ok, Rslt}, Res),
    Rslt.


lookup_pid(Mod, TabState, Pid, Rslt) ->
    Res = ?M:lookup_pid(Mod, TabState, Pid),
    ?assertEqual({ok, Rslt}, Res),
    Rslt.


delete(Mod, TabState, Child) ->
    Res = ?M:delete(Mod, TabState, Child),
    ?assertMatch({ok, _}, Res),
    erlang:element(2, Res).


combine(Mod, TabState, DefChildSpec) ->
    Res = ?M:combine_children(Mod, TabState, DefChildSpec),
    ?assertMatch({ok, _}, Res),
    erlang:element(2, Res).


separate(Mod, TabState, DefChildSpec) ->
    Res = ?M:separate_children(Mod, TabState, DefChildSpec),
    ?assertMatch({ok, _}, Res),
    erlang:element(2, Res).