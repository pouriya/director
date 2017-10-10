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
%% @version  17.9.16
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(director_table_ets).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(director_table).


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([count_children/1
        ,which_children/1
        ,get_childspec/2
        ,get_pid/2
        ,get_pids/1
        ,get_plan/2
        ,get_restart_count/2]).

%% director's API:
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
        ,parent_insert/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(ETS_TABLE_OPTIONS, [public, named_table, set, {keypos, 2}]).

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

-define(is_valid_type(Type), (Type =:= set orelse Type =:= ordered_set)).

%% -------------------------------------------------------------------------------------------------
%% API:

count_children(Tab) ->
    director_table:count_children(?MODULE, Tab).


which_children(Tab) ->
    director_table:which_children(?MODULE, Tab).


get_childspec(Tab, Id) ->
    director_table:get_childspec(?MODULE, Tab, Id).


get_pid(Tab, Id) ->
    director_table:get_pid(?MODULE, Tab, Id).


get_pids(Tab) ->
    director_table:get_pids(?MODULE, Tab).


get_plan(Tab, Id) ->
    director_table:get_plan(?MODULE, Tab, Id).


get_restart_count(Tab, Id) ->
    director_table:get_restart_count(?MODULE, Tab, Id).

%% -------------------------------------------------------------------------------------------------
%% Director's API functions:

create({value, TabName}) when erlang:is_atom(TabName) ->
    case is_table(TabName) of
        true ->
            Self = erlang:self(),
            case {ets:info(TabName, protection)
                 ,ets:info(TabName, owner)
                 ,ets:info(TabName, type)} of
                {public, _, Type} when ?is_valid_type(Type) ->
                    {ok, TabName};
                {public, _, Type} ->
                    {error, {table_type, [{type, Type}, {init_argument, TabName}]}};
                {_, Self, Type} when ?is_valid_type(Type) ->
                    {ok, TabName};
                {_, Self, Type} ->
                    {error, {table_type, [{type, Type}, {init_argument, TabName}]}};
                {Protection, Pid, _Type} ->
                    {error, {table_protection_and_owner, [{protection, Protection}
                                                         ,{owner, Pid}
                                                         ,{self, Self}
                                                         ,{init_argument, TabName}]}}
            end;
        false ->
            try
                {ok, ets:new(TabName, ?ETS_TABLE_OPTIONS)}
            catch
                _:Reason ->
                    {error, {table_create, [{reason, Reason}, {init_argument, TabName}]}}
            end
    end;
create(undefined) ->
    {error, {table_init_argument, []}}.


delete_table(Tab) when erlang:is_atom(Tab) ->
    try ets:delete(Tab) of
        _ ->
            ok
    catch
        _:_ ->
            table_error(Tab)
    end.


lookup_id(Tab, Id) when erlang:is_atom(Tab) ->
    try ets:lookup(Tab, Id) of
        [Child] ->
            {ok, Child};
        [] ->
            {ok, not_found}
    catch
        _:_ ->
            table_error(Tab)
    end.


count(Tab) when erlang:is_atom(Tab) ->
    case ets:info(Tab, size) of
        undefined ->
            {error, {table_existence, [{table, Tab}]}};
        Size ->
            {ok, Size}
    end.


lookup_pid(Tab, Pid) when erlang:is_atom(Tab) ->
    try ets:match_object(Tab, #?CHILD{pid = Pid, _ = '_'}) of
        [Child] ->
            {ok, Child};
        [] ->
            {ok, not_found}
    catch
        _:_ ->
            table_error(Tab)
    end.


lookup_appended(Tab) when erlang:is_atom(Tab) ->
    try ets:match_object(Tab, #?CHILD{append = true, _ = '_'}) of
        Children ->
            {ok, Children}
    catch
        _:_ ->
            table_error(Tab)
    end.


insert(Tab, Child) when erlang:is_atom(Tab) ->
    try ets:insert(Tab, Child) of
        _ ->
            {ok, Tab}
    catch
        _:_ ->
            table_error(Tab)
    end.


delete(Tab, #?CHILD{id=Id}) when erlang:is_atom(Tab) ->
    try ets:delete(Tab, Id) of
        _ ->
            {ok, Tab}
    catch
        _:_ ->
            table_error(Tab)
    end.


tab2list(Tab) when erlang:is_atom(Tab) ->
    try
        {ok, ets:tab2list(Tab)}
    catch
        _:_ ->
            table_error(Tab)
    end.


handle_message(_, _) ->
    unknown.


parent_insert(Tab, #?CHILD{id = Id}=Child) when erlang:is_atom(Tab) ->
    try ets:lookup(Tab, Id) of
        [#?CHILD{supervisor = Pid}] when erlang:self() =/= Pid ->
            {error, Tab, not_owner};
        _ ->
            _ = ets:insert(Tab, Child),
            {ok, Tab}
    catch
        _:_ ->
            table_error(Tab)
    end.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

table_error(Tab) ->
    case is_table(Tab) of
        true ->
            {error, {table_protection_and_owner, [{protection, ets:info(Tab, protection)}
                                                 ,{owner, ets:info(Tab, owner)}
                                                 ,{table, Tab}]}};
        false ->
            {error, {table_existence, [{table, Tab}]}}
    end.


is_table(Tab) ->
    lists:member(Tab, ets:all()).