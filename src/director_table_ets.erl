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
%% @version  18.1.1
%% @doc
%%           API functions for interacting with ETS table as backend children table.
%% @end
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
        ,get_restart_count/2
        ,options/0]).

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
        ,change_parent/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(TABLE_OPTIONS, [public, named_table, set, {keypos, 2}]).

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

-define(is_valid_type(Type), (Type =:= set orelse Type =:= ordered_set)).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
count_children(atom()) ->
    [{'specs', non_neg_integer()}
    |{'active', non_neg_integer()}
    |{'supervisors', non_neg_integer()}
    |{'workers', non_neg_integer()}]    |
    {'error', term()}.
%% @doc
%%      Returns count of children.<br/>
%%      Error is for reading from table.
%% @end
count_children(Tab) ->
    director_table:count_children(?MODULE, Tab).

-spec
which_children(atom()) ->
    [{director:id(), director:type(), pid()|'restarting'|'undefined', director:modules()}] |
    [] |
    {'error', term()}.
%% @doc
%%      Returns information about each child.<br/>
%%      Error maybe occur for reading from table.
%% @end
which_children(Tab) ->
    director_table:which_children(?MODULE, Tab).

-spec
get_childspec(atom(), director:id() | pid()) ->
    {'ok', director:childspec()} | {'error', 'not_found' | term()}.
%% @doc
%%      Returns childspec of child id or child pid.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_childspec(Tab, Id) ->
    director_table:get_childspec(?MODULE, Tab, Id).

-spec
get_pid(atom(), director:id()) ->
    {'ok', pid()} | {'error', 'not_found'|'restarting'|'undefined'|term()}.
%% @doc
%%      Returns pid of child id if child is running.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_pid(Tab, Id) ->
    director_table:get_pid(?MODULE, Tab, Id).

-spec
get_pids(atom()) ->
    {'ok', [{director:id(), pid()}] | []} | {'error', term()}.
%% @doc
%%      Returns list of {id, pid}s for all running children.<br/>
%%      Error is for reading from table.
%% @end
get_pids(Tab) ->
    director_table:get_pids(?MODULE, Tab).

-spec
get_restart_count(atom(), director:id()) ->
    {'ok', non_neg_integer()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns restart count of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_restart_count(Tab, Id) ->
    director_table:get_restart_count(?MODULE, Tab, Id).

-spec
options() ->
    list().
%% @doc
%%      Returns mandatory options of ETS table for creating table for a director process.
%% @end
options() ->
    ?TABLE_OPTIONS.

%% -------------------------------------------------------------------------------------------------
%% Director's API functions:

%% @hidden
create({value, TabName}) ->
    case is_table(TabName) of
        true ->
            Self = erlang:self(),
            case {ets:info(TabName, protection)
                 ,ets:info(TabName, owner)
                 ,ets:info(TabName, type)
                 ,ets:info(TabName, keypos)} of
                {public, _, Type, 2} when ?is_valid_type(Type) ->
                    {ok, TabName};
                {public, _, Type, Keypos} when ?is_valid_type(Type) ->
                    {hard_error, {table_info, [{keypos, Keypos}]}};
                {public, _, Type, _} ->
                    {hard_error, {table_info, [{type, Type}]}};
                {_, Self, Type, 2} when ?is_valid_type(Type) ->
                    {ok, TabName};
                {_, Self, Type, KeyPos} when ?is_valid_type(Type) ->
                    {hard_error, {table_info, [{keypos, KeyPos}]}};
                {_, Self, Type, _} ->
                    {hard_error, {table_info, [{type, Type}]}};
                {Protection, Pid, _Type, _Keypos} ->
                    {hard_error, {table_info, [{protection, Protection}, {owner, Pid}]}}
            end;
        false ->
            try
                {ok, ets:new(TabName, ?TABLE_OPTIONS)}
            catch
                _:Reason ->
                    {hard_error, {table_create, [{reason, Reason}]}}
            end
    end.


%% @hidden
delete_table(Tab) ->
    try ets:delete(Tab) of
        _ ->
            ok
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
lookup_id(Tab, Id) ->
    try ets:lookup(Tab, Id) of
        [Child] ->
            {ok, Child};
        [] ->
            {soft_error, not_found}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
count(Tab) ->
    case ets:info(Tab, size) of
        undefined ->
            {hard_error, {table_existence, []}};
        Size ->
            {ok, Size}
    end.


%% @hidden
lookup_pid(Tab, Pid) ->
    try ets:match_object(Tab, #?CHILD{pid = Pid, _ = '_'}) of
        [Child] ->
            {ok, Child};
        [] ->
            {soft_error, not_found}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
lookup_appended(Tab) ->
    try
        {ok, ets:match_object(Tab, #?CHILD{append = true, _ = '_'})}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
insert(Tab, Child) ->
    try
        _ = ets:insert(Tab, Child),
        {ok, Tab}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
delete(Tab, Id) ->
    try
        _ = ets:delete(Tab, Id),
        {ok, Tab}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
tab2list(Tab) ->
    try
        {ok, ets:tab2list(Tab)}
    catch
        _:_ ->
            table_error(Tab)
    end.


%% @hidden
handle_message(Tab, _) ->
    {soft_error, Tab, unknown}.


%% @hidden
change_parent(Tab, #?CHILD{id = Id}=Child) ->
    try ets:lookup(Tab, Id) of
        [#?CHILD{supervisor = Pid}] when erlang:self() =/= Pid andalso Pid =/= undefined ->
            {soft_error, not_parent};
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
            {hard_error, {table_info, [{protection, ets:info(Tab, protection)}
                                      ,{owner, ets:info(Tab, owner)}]}};
        false ->
            {hard_error, {table_existence, []}}
    end.


is_table(Tab) ->
    lists:member(Tab, ets:all()).