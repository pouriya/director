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
%% @version  17.11.30
%% @doc
%%           API functions for interacting with Mnesia table as backend children table.
%% @end
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

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

-define(TABLE_OPTIONS, [{attributes, record_info(fields, ?CHILD)}
                       ,{type, set}
                       ,{record_name, ?CHILD}]).

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
get_plan(atom(), director:id()) ->
    {'ok', director:plan()} | {'error', 'not_found'|term()}.
%% @doc
%%      Returns plan of child id.<br/>
%%      Error maybe occur for reading from table.
%% @end
get_plan(Tab, Id) ->
    director_table:get_plan(?MODULE, Tab, Id).

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
%%      Returns mandatory options of Mnesia table for creating table for a director process.
%% @end
options() ->
    ?TABLE_OPTIONS.

%% -------------------------------------------------------------------------------------------------
%% Director's API functions:

%% @hidden
create({value, TabName}) ->
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
                    {hard_error, {table_access_mode, [{access_mode, read_only}]}};
                {_, Size, _} when erlang:tuple_size(#?CHILD{}) =:= Size ->
                    {hard_error, {table_record_size, [{record_size, Size}]}};
                {_, _, Type} ->
                    {hard_error, {table_type, [{type, Type}]}}
            end;
        false ->
            try mnesia:create_table(TabName, ?TABLE_OPTIONS) of
                {atomic, ok} ->
                    _ = mnesia:subscribe(system),
                    {ok, TabName};
                {aborted, Rsn} ->
                    {hard_error, {table_create, [{reason, Rsn}]}}
            catch
                _:Reason ->
                    {hard_error, {table_create, [{reason, Reason}]}}
            end;
        error ->
            {hard_error, {table_create, [{reason, mnesia_not_started}]}}
    end.


%% @hidden
delete_table(Tab) ->
    try mnesia:delete_table(Tab) of
        {atomic, ok} ->
            _ = mnesia:unsubscribe(system),
            ok;
        {aborted, Rsn} ->
            {hard_error, {delete_table, [{reason, Rsn}]}}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.


%% @hidden
lookup_id(Tab, Id) ->
    TA =
        fun() ->
            case mnesia:read(Tab, Id, read) of
                [Child] ->
                    {ok, Child};
                [] ->
                    {soft_error, not_found}
            end
        end,
    transaction(Tab, TA).


%% @hidden
count(Tab) ->
    try
        {ok, mnesia:table_info(Tab, size)}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.


%% @hidden
lookup_pid(Tab, Pid) ->
    TA =
        fun() ->
            case mnesia:select(Tab, [{#?CHILD{pid = '$1', _='_'}, [{'=:=', '$1', Pid}], ['$_']}]) of
                [Child] ->
                    {ok, Child};
                [] ->
                    {soft_error, not_found}
            end
        end,
    transaction(Tab, TA).


%% @hidden
lookup_appended(Tab) ->
    TA =
        fun() ->
            {ok
            ,mnesia:select(Tab, [{#?CHILD{append = '$1', _='_'}, [{'=:=', '$1', true}], ['$_']}])}
        end,
    transaction(Tab, TA).


%% @hidden
insert(Tab, Child) ->
    TA =
        fun() ->
            mnesia:write(Tab, Child, write),
            {ok, Tab}
        end,
    transaction(Tab, TA).


%% @hidden
delete(Tab, #?CHILD{id=Id}) ->
    TA =
        fun() ->
            case mnesia:read(Tab, Id, read) of
                [Child] ->
                    mnesia:delete_object(Tab, Child, write),
                    {ok, Tab};
                [] ->
                    {soft_error, not_found}
            end
        end,
    transaction(Tab, TA).


%% @hidden
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


%% @hidden
handle_message(Tab, {mnesia_system_event, {mnesia_down, Node}}) ->
    TA =
        fun() ->
            Fold =
                fun
                    (#?CHILD{supervisor = Sup
                            ,delete_before_terminate = false
                            ,pid = Pid}=Child
                    ,{ShouldRestart, ShouldDelete}) when erlang:node(Sup) =:= Node andalso
                                                         erlang:is_pid(Pid) ->
                        {[Child|ShouldRestart], ShouldDelete};
                    (#?CHILD{supervisor = Sup, delete_before_terminate = true}=Child
                    ,{ShouldRestart, ShouldDelete}) when erlang:node(Sup) =:= Node ->
                        {ShouldRestart, [Child|ShouldDelete]};
                    (_, Acc) ->
                        Acc
                end,
            mnesia:foldl(Fold, {[], []}, Tab)
        end,
    case transaction(Tab, TA) of
        {hard_error, _}=HErr ->
            HErr;
        {[], []} ->
            {ok, Tab};
        {ShouldRestart, ShouldDelete} ->
            DeleteTA =
                fun(Child) ->
                    fun () ->
                        ok = mnesia:delete_object(Tab, Child, write)
                    end
                end,
            DeleteTAs = [DeleteTA(Child) || Child <- ShouldDelete],
            case transactions(Tab, DeleteTAs) of
                {ok, _} ->
                    ShouldRestartTA =
                        fun(#?CHILD{id = Id}=Child) ->
                            Ref = director:self_start(Id),
                            fun() ->
                                ok = mnesia:write(Tab
                                                 ,Child#?CHILD{supervisor = undefined
                                                              ,timer_reference = Ref
                                                              ,pid = undefined
                                                              ,extra = undefined}
                                                 ,write)
                            end
                        end,
                    ShouldRestartTAs = [ShouldRestartTA(Child) || Child <- ShouldRestart],
                    transactions(Tab, ShouldRestartTAs);
                {hard_error, _}=HErr ->
                    HErr
            end
    end;
handle_message(Tab, {mnesia_system_event, _}) ->
    {ok, Tab};
handle_message(_, _) ->
    {soft_error, unknown}.


%% @hidden
change_parent(Tab, #?CHILD{id = Id}=Child) ->
    Self = erlang:self(),
    TA =
        fun() ->
            case mnesia:read(Tab, Id, write) of
                [#?CHILD{supervisor = Pid}] when Pid =/= Self andalso Pid =/= undefined ->
                    {soft_error, Tab, not_parent};
                _ ->
                    _ = mnesia:write(Tab, Child, write),
                    {ok, Tab}
            end
        end,
    transaction(Tab, TA).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

table_error(Tab, Rsn) ->
    case is_table(Tab) of
        true ->
            {hard_error, {table_error, [{access_mode, mnesia:table_info(Tab, access_mode)}
                                       ,{arity, mnesia:table_info(Tab, arity)}
                                       ,{type, mnesia:table_info(Tab, type)}]}};
        false ->
            {hard_error, {table_existence, []}};
        error ->
            {hard_error, {table_error, [{reason, Rsn}]}}
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
            {hard_error, {transaction_error, [{reason, Rsn}]}}
    catch
        _:Rsn ->
            table_error(Tab, Rsn)
    end.

transactions(Tab, [TA|TAs]) ->
    case transaction(Tab, TA) of
        {hard_error, _}=HErr ->
            HErr;
        _ ->
            transactions(Tab, TAs)
    end;
transactions(Tab, []) ->
    {ok, Tab}.