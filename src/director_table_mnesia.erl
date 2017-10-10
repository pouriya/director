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


-module(director_table_mnesia).
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
        ,handle_message/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

-define(TABLE_OPTIONS, [{attributes, record_info(fields, ?CHILD)}
                       ,{type, set}
                       ,{record_name, ?CHILD}]).

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
            case {mnesia:table_info(TabName, access_mode)
                 ,mnesia:table_info(TabName, arity)
                 ,mnesia:table_info(TabName, type)} of
                {read_write, Size, Type} when ?is_valid_type(Type) andalso
                                              erlang:tuple_size(#?CHILD{}) =:= Size ->
                    _ = mnesia:subscribe(system),
                    {ok, TabName};
                {read_only, _, _} ->
                    {error, {table_access_mode, [{access_mode, read_only}
                                                ,{init_argument, TabName}]}};
                {_, Size, _} when erlang:tuple_size(#?CHILD{}) =:= Size ->
                    {error, {table_record_size, [{record_size, Size}
                                                ,{init_argument, TabName}]}};
                {_, _, Type} ->
                    {error, {table_type, [{type, Type}, {init_argument, TabName}]}}
            end;
        false ->
            try mnesia:create_table(TabName, ?TABLE_OPTIONS) of
                {atomic, ok} ->
                    _ = mnesia:subscribe(system),
                    {ok, TabName};
                {aborted, Rsn} ->
                    {error, {table_create, [{reason, Rsn}, {init_argument, TabName}]}}
            catch
                _:Reason ->
                    {error, {table_create, [{reason, Reason}, {init_argument, TabName}]}}
            end;
        error ->
            {error, {table_create, [{reason, mnesia_not_started}]}}
    end;
create(undefined) ->
    {error, {table_init_argument, []}}.


delete_table(Tab) ->
    try mnesia:delete_table(Tab) of
        {atomic, ok} ->
            _ = mnesia:unsubscribe(system),
            ok;
        {aborted, Rsn} ->
            {error, {delete_table, [{reason, Rsn}, {table, Tab}]}}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.


lookup_id(Tab, Id) ->
    TA =
        fun() ->
            case mnesia:read(Tab, Id, read) of
                [Child] ->
                    {ok, Child};
                [] ->
                    {ok, not_found}
            end
        end,
    transaction(Tab, TA).


count(Tab) ->
    try
        {ok, mnesia:table_info(Tab, size)}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.


lookup_pid(Tab, Pid) ->
    TA =
        fun() ->
            case mnesia:select(Tab, [{#?CHILD{pid = '$1', _='_'}, [{'=:=', '$1', Pid}], ['$_']}]) of
                [Child] ->
                    {ok, Child};
                [] ->
                    {ok, not_found}
            end
        end,
    transaction(Tab, TA).


lookup_appended(Tab) ->
    TA =
        fun() ->
            {ok, mnesia:select(Tab
                              ,[{#?CHILD{append = '$1', _='_'}, [{'=:=', '$1', true}], ['$_']}])}
        end,
    transaction(Tab, TA).


insert(Tab, Child) ->
    TA =
        fun() ->
            mnesia:write(Tab, Child, write),
            {ok, Tab}
        end,
    transaction(Tab, TA).


delete(Tab, #?CHILD{id=Id}) ->
    TA =
        fun() ->
            case mnesia:read(Tab, Id, read) of
                [Child] ->
                    mnesia:delete_object(Tab, Child, write),
                    {ok, Tab};
                [] ->
                    {ok, not_found}
            end
        end,
    transaction(Tab, TA).


tab2list(Tab) ->
    TA =
        fun() ->
            Fold =
                fun(Child, Acc) ->
                    [Child|Acc]
                end,
            {ok, mnesia:foldl(Fold, [], Tab)}
        end,
    transaction(Tab, TA).


handle_message(Tab, {mnesia_system_event, {mnesia_down, Node}}) ->
    TA =
        fun() ->
            Fold =
                fun
                    (#?CHILD{supervisor = Sup}=Child, Acc) when erlang:node(Sup) =:= Node ->
                        [Child|Acc];
                    (_, Acc) ->
                        Acc
                end,
            Children = mnesia:foldl(Fold, [], Tab),
            Delete =
                fun(Child) ->
                    ok = mnesia:delete_object(Tab, Child, write)
                end,
            ok = lists:foreach(Delete, Children),
            {ok, Tab}
        end,
    transaction(Tab, TA);
handle_message(Tab, {mnesia_system_event, _}) ->
    {ok, Tab};
handle_message(_, _) ->
    unknown.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

table_error(Tab, Rsn) ->
    case is_table(Tab) of
        true ->
            {error, {table_error, [{access_mode, mnesia:table_info(Tab, access_mode)}
                                  ,{arity, mnesia:table_info(Tab, arity)}
                                  ,{type, mnesia:table_info(Tab, type)}]}};
        false ->
            {error, {table_existence, [{table, Tab}]}};
        error ->
            {error, {table_error, [{reason, Rsn}]}}
    end.


is_table(Tab) ->
    try
        lists:member(Tab, mnesia:system_info(tables))
    catch
        _:_ ->
            error
    end.


transaction(Tab, TA) ->
    try mnesia:transaction(TA) of
        {atomic, Rslt} ->
            Rslt;
        {aborted, Rsn} ->
            {error, {transaction_error, [{reason, Rsn}]}}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.