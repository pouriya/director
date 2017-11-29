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
%% @version  17.10.25
%% @hidden
%% @doc
%%           API functions for keeping, updating and fetching
%%           childspecs data.<br/>
%%           director supports tho type of tables: list, ets.
%% @end
%% -------------------------------------------------------------------------------------------------


-module(director_table).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% Director's API:
-export([create/2
        ,insert/3
        ,delete/3
        ,lookup_id/3
        ,lookup_pid/3
        ,lookup_appended/2
        ,combine_children/3
        ,separate_children/3
        ,count/2
        ,delete_table/2
        ,tab2list/2
        ,handle_message/3
        ,change_parent/3]).

%% Callback module API:
-export([count_children/2
        ,which_children/2
        ,get_childspec/3
        ,get_pid/3
        ,get_pids/2
        ,get_plan/3
        ,get_restart_count/3]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

%% -------------------------------------------------------------------------------------------------
%% Behavior information:

-callback
create({'value', InitArgument::any()} | 'undefined') ->
    {'ok', State::any()} | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
insert(State::any(), Child::#?CHILD{}) ->
    {'ok', NewState::any()} | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
delete(State::any(), Child::#?CHILD{}) ->
    {'ok', NewState::any()}                              |
    {'soft_error', Reason::'not_found'}                  |
    {'soft_error', NewState::any(), Reason::'not_found'} |
    {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
lookup_id(State::any(), Id::any()) ->
    {'ok', Child::#?CHILD{}}                             |
    {'soft_error', Reason::'not_found'}                  |
    {'soft_error', NewState::any(), Reason::'not_found'} |
    {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
lookup_pid(State::any(), Pid::pid()) ->
    {'ok', Child::#?CHILD{}}                             |
    {'soft_error', Reason::'not_found'}                  |
    {'soft_error', NewState::any(), Reason::'not_found'} |
    {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
lookup_appended(State::any()) ->
    {'ok', [Child::#?CHILD{}] | []} | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
count(State::any()) ->
    {'ok', Count::non_neg_integer()} | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
tab2list(State::any()) ->
    {'ok', [Child::#?CHILD{}] | []} | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
delete_table(State::any()) ->
    'ok' | {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
handle_message(State::any(), Msg::any()) ->
    {'ok', Child::#?CHILD{}}                             |
    {'soft_error', Reason::'unknown'}                    |
    {'soft_error', NewState::any(), Reason::'unknown'}   |
    {'hard_error', {Reason::atom(), ErrorParams::list()}}.


-callback
change_parent(State::any(), Child::#?CHILD{}) ->
    {'ok', NewState::any()}                              |
    {'soft_error', Reason::'not_parent'}                 |
    {'soft_error', NewState::any(), Reason::'not_parent'}|
    {'hard_error', {Reason::atom(), ErrorParams::list()}}.

%% -------------------------------------------------------------------------------------------------
%% Callback module API:

count_children(Mod, State) ->
    case tab2list(Mod, State) of
        {ok, Children} ->
            Fun =
                fun(#?CHILD{pid = Pid, type = Type}, {Specs, Actives, Sups, Workers}) ->
                    Actives2 =
                        if
                            erlang:is_pid(Pid) ->
                                Actives+1;
                            true ->
                                Actives
                        end,
                    {Sups2, Workers2} =
                        if
                            Type =:= supervisor ->
                                {Sups+1, Workers};
                            Type =:= worker ->
                                {Sups, Workers+1}
                        end,
                    {Specs+1, Actives2, Sups2, Workers2}
                end,
            {Specs, Actives, Sups, Workers} = lists:foldl(Fun, {0, 0, 0, 0}, Children),
            [{specs, Specs}, {active, Actives}, {supervisors, Sups}, {workers, Workers}];
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


which_children(Mod, State) ->
    case director_table:tab2list(Mod, State) of
        {ok, Children} ->
            [{Id, Pid, Type, Mods} || #?CHILD{id = Id
                                             ,pid = Pid
                                             ,type = Type
                                             ,modules = Mods} <- Children];
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


get_childspec(Mod, State, Id) ->
    case lookup_id(Mod, State, Id) of
        {ok, Child} ->
            {ok, director_utils:c2cs(Child)};
        {soft_error, _, Rsn} ->
            {error, Rsn};
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


get_pid(Mod, State, Id) ->
    case lookup_id(Mod, State, Id) of
        {ok, #?CHILD{pid = Pid}} ->
            {ok, Pid};
        {soft_error, _, Rsn} ->
            {error, Rsn};
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


get_pids(Mod, State) ->
    case director_table:tab2list(Mod, State) of
        {ok, Children} ->
            {ok, [{Id, Pid} || #?CHILD{id = Id, pid = Pid} <- Children, erlang:is_pid(Pid)]};
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


get_plan(Mod, State, Id) ->
    case lookup_id(Mod, State, Id) of
        {ok, #?CHILD{plan = Plan}} ->
            {ok, Plan};
        {soft_error, _, Rsn} ->
            {error, Rsn};
        {hard_error, Rsn} ->
            {error, Rsn}
    end.


get_restart_count(Mod, State, Id) ->
    case lookup_id(Mod, State, Id) of
        {ok, #?CHILD{restart_count = ResCount}} ->
            {ok, ResCount};
        {soft_error, _, Rsn} ->
            {error, Rsn};
        {hard_error, Rsn} ->
            {error, Rsn}
    end.

%% -------------------------------------------------------------------------------------------------
%% API functions:

create(Mod, InitArg) ->
    try Mod:create(InitArg) of
        {ok, _}=Ok ->
            Ok;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, create}
                                            ,{arguments, [InitArg]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, create}
                                            ,{arguments, [InitArg]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, create}
                                       ,{arguments, [InitArg]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


delete_table(Mod, State) ->
    try Mod:delete_table(State) of
        ok ->
            ok;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, delete_table}
                                            ,{arguments, [State]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, delete_table}
                                            ,{arguments, [State]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, delete_table}
                                       ,{arguments, [State]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


lookup_id(Mod, State, Id) ->
    try Mod:lookup_id(State, Id) of
        {ok, #?CHILD{}}=Ok ->
            Ok;
        {soft_error, not_found} ->
            {soft_error, State, not_found};
        {soft_error, _, not_found}=SErr ->
            SErr;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, lookup_id}
                                            ,{arguments, [State, Id]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, lookup_id}
                                            ,{arguments, [State, Id]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, lookup_id}
                                       ,{arguments, [State, Id]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


count(Mod, State) ->
    try Mod:count(State) of
        {ok, Count}=Ok when erlang:is_integer(Count) andalso Count > -1 ->
            Ok;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, count}
                                            ,{arguments, [State]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, count}
                                            ,{arguments, [State]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, count}
                                       ,{arguments, [State]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


lookup_pid(Mod, State, Pid) ->
    try Mod:lookup_pid(State, Pid) of
        {ok, #?CHILD{}}=Ok ->
            Ok;
        {soft_error, not_found} ->
            {soft_error, State, not_found};
        {soft_error, _, not_found}=SErr ->
            SErr;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, lookup_pid}
                                            ,{arguments, [State, Pid]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, lookup_pid}
                                            ,{arguments, [State, Pid]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, lookup_pid}
                                       ,{arguments, [State, Pid]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


lookup_appended(Mod, State) ->
    try Mod:lookup_appended(State) of
        {ok, List}=Ok when erlang:is_list(List) ->
            case validate_children(List) of
                true ->
                    Ok;
                false ->
                    {hard_error, {table_bad_return, [{returned_value, Ok}
                                                    ,{module, Mod}
                                                    ,{function, lookup_appended}
                                                    ,{arguments, [State]}]}}
            end;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, lookup_appended}
                                            ,{arguments, [State]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, lookup_appended}
                                            ,{arguments, [State]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, lookup_appended}
                                       ,{arguments, [State]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


insert(Mod, State, Child) ->
    try Mod:insert(State, Child) of
        {ok, _}=Ok ->
            Ok;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, insert}
                                            ,{arguments, [State, Child]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, insert}
                                            ,{arguments, [State, Child]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, insert}
                                       ,{arguments, [State, Child]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


delete(Mod, State, Child) ->
    try Mod:delete(State, Child) of
        {ok, _}=Ok ->
            Ok;
        {soft_error, not_found} ->
            {soft_error, State, not_found};
        {soft_error, _, not_found}=SErr ->
            SErr;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, delete}
                                            ,{arguments, [State, Child]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, delete}
                                            ,{arguments, [State, Child]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, delete}
                                       ,{arguments, [State, Child]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


tab2list(Mod, State) ->
    try Mod:tab2list(State) of
        {ok, List}=Ok when erlang:is_list(List) ->
            case validate_children(List) of
                true ->
                    Ok;
                false ->
                    {hard_error, {table_bad_return, [{returned_value, Ok}
                                                    ,{module, Mod}
                                                    ,{function, tab2list}
                                                    ,{arguments, [State]}]}}
            end;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, tab2list}
                                            ,{arguments, [State]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, tab2list}
                                            ,{arguments, [State]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, tab2list}
                                       ,{arguments, [State]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


combine_children(Mod, State, DefChildSpec) ->
    case lookup_appended(Mod, State) of
        {ok, Appended} ->
            case validate_parent(Appended) of
                true ->
                    Combine =
                        fun(Child) ->
                            ChildSpec = director_utils:c2cs(Child),
                            Combined = director_utils:combine_child(ChildSpec, DefChildSpec),
                            director_utils:cs2c(Combined)
                        end,
                    CombinedChildren = [Combine(Child) || Child <- Appended],
                    case insert_children(Mod, State, CombinedChildren) of
                        {ok, _}=Ok ->
                            Ok;
                        {hard_error, _}=HErr ->
                            HErr
                    end;
                false ->
                    {soft_error, State, not_parent}
            end;
        {hard_error, {Rsn, ErrParams}} ->
            {hard_error, {Rsn, lists:keyreplace(function
                                               ,1
                                               ,ErrParams
                                               ,{function, combine_children})}}
    end.


separate_children(Mod, State, DefChildSpec) ->
    case lookup_appended(Mod, State) of
        {ok, Appended} ->
            case validate_parent(Appended) of
                true ->
                    Separate =
                        fun(Child) ->
                            ChildSpec = director_utils:c2cs(Child),
                            Separated = director_utils:separate_child(ChildSpec, DefChildSpec),
                            director_utils:cs2c(Separated)
                        end,
                    SeparatedChildren = [Separate(Child) || Child <- Appended],
                    case insert_children(Mod, State, SeparatedChildren) of
                        {ok, _}=Ok ->
                            Ok;
                        {hard_error, _}=HErr ->
                            HErr
                    end;
                false ->
                    {soft_error, State, not_parent}
            end;
        {hard_error, {Rsn, ErrParams}} ->
            {hard_error, {Rsn, lists:keyreplace(function
                                               ,1
                                               ,ErrParams
                                               ,{function, separate_children})}}
    end.


handle_message(Mod, State, Msg) ->
    try Mod:handle_message(State, Msg) of
        {ok, _}=Ok ->
            Ok;
        {soft_error, unknown} ->
            {soft_error, State, unknown};
        {soft_error, _, unknown}=SErr ->
            SErr;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, handle_message}
                                            ,{arguments, [State, Msg]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, handle_message}
                                            ,{arguments, [State, Msg]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, handle_message}
                                       ,{arguments, [State, Msg]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.


change_parent(Mod, State, Child) ->
    try Mod:change_parent(State, Child) of
        {ok, _}=Ok ->
            Ok;
        {soft_error, not_parent} ->
            {soft_error, State, not_parent};
        {soft_error, _, not_parent}=SErr ->
            SErr;
        {hard_error, {Rsn, ErrParams}} when erlang:is_atom(Rsn) andalso erlang:is_list(ErrParams) ->
            {hard_error, {Rsn, ErrParams ++ [{module, Mod}
                                            ,{function, change_parent}
                                            ,{arguments, [State, Child]}]}};
        Other ->
            {hard_error, {table_bad_return, [{returned_value, Other}
                                            ,{module, Mod}
                                            ,{function, change_parent}
                                            ,{arguments, [State, Child]}]}}
    catch
        _:Rsn ->
            {hard_error, {table_crash, [{reason, Rsn}
                                       ,{module, Mod}
                                       ,{function, change_parent}
                                       ,{arguments, [State, Child]}
                                       ,{stacktrace, erlang:get_stacktrace()}]}}
    end.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

insert_children(Mod, State, [Child|Children]) ->
    case insert(Mod, State, Child) of
        {ok, State2} ->
            insert_children(Mod, State2, Children);
        {hard_error, _}=Err ->
            Err
    end;
insert_children(_, State, []) ->
    {ok, State}.


validate_children(Children) ->
    lists:all(fun(Child) -> erlang:is_record(Child, ?CHILD) end, Children).


validate_parent(Children) ->
    lists:all(fun(#?CHILD{supervisor = Sup}) -> Sup =:= erlang:self() end, Children).