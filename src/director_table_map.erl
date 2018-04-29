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


-module(director_table_map).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(director_table).


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% Director's API:
-export([create/1
        ,insert/2
        ,delete/2
        ,lookup_id/2
        ,lookup_pid/2
        ,lookup_appended/1
        ,count/1
        ,delete_table/1
        ,tab2list/1
        ,handle_message/2
        ,change_parent/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

%% -------------------------------------------------------------------------------------------------
%% API functions:

create(_Arg) ->
    {ok, {#{}, #{}}}.


delete_table(_Tab) ->
    ok.


lookup_id({Tab, _}, Id) ->
    try maps:get(Id, Tab) of
        Child ->
            {ok, Child}
    catch
        _:_ ->
            {soft_error, not_found}
    end.


count({Tab, _}) ->
    {ok, erlang:map_size(Tab)}.


lookup_pid({Tab, Pids}, Pid) ->
    try maps:get(Pid, Pids) of
        Id ->
            {ok, maps:get(Id, Tab)}
    catch
        _:_ ->
            {soft_error, not_found}
    end.


lookup_appended({Tab, _}) ->
    LookupFun =
        fun(_, #?CHILD{append = Append}=Child, Acc) ->
            if
                Append ->
                    [Child|Acc];
                true ->
                    Acc
            end
        end,
    {ok, maps:fold(LookupFun, [], Tab)}.


insert({Tab, Pids}, #?CHILD{id = Id, pid = Pid}=Child) ->
    Tab2 = maps:put(Id, Child, Tab),
    if
        erlang:is_pid(Pid) ->
            {ok, {Tab2, maps:put(Pid, Id, Pids)}};
        true ->
            {ok, {Tab2, maps:remove(Pid, Pids)}}
    end.


delete({Tab, Pids}=Tab2, Id) ->
    case lookup_id(Tab2, Id) of
        {ok, #?CHILD{pid = Pid}} ->
            {ok, {maps:remove(Id, Tab), maps:remove(Pid, Pids)}};
        _ ->
            {ok, {maps:remove(Id, Tab), Pids}}
    end.


tab2list({Tab, _}) ->
    ToListFun =
        fun(_, Child, Acc) ->
            [Child|Acc]
        end,
    {ok, maps:fold(ToListFun, [], Tab)}.


handle_message(_, _) ->
    {soft_error, unknown}.


change_parent({Tab, Pids}, #?CHILD{id = Id, pid = Pid}=Child) ->
    Tab2 = maps:put(Id, Child, Tab),
    if
        erlang:is_pid(Pid) ->
            {ok, {Tab2, maps:put(Pid, Id, Pids)}};
        true ->
            {ok, {Tab2, maps:remove(Pid, Pids)}}
    end.