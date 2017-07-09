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
%%           API functions for keeping, updating and fetching
%%           childspecs data.
%% @end
%% @hidden
%% ---------------------------------------------------------------------


-module(director_table).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([create/0
        ,insert/2
        ,delete/2
        ,lookup/2
        ,lookup_by_pid/2
        ,lookup_appended/1
        ,combine_children/2
        ,separate_children/2
        ,count/1
        ,delete_table/1
        ,tab2list/1]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-define(ETS_TABLE_OPTIONS, [{keypos,2}]).





%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").





%% ---------------------------------------------------------------------
%% API functions:





create() ->
    Name = erlang:list_to_atom(erlang:pid_to_list(erlang:self())),
    ets:new(Name, ?ETS_TABLE_OPTIONS).







delete_table(Table) ->
    ets:delete(Table).







lookup(Table, Id) ->
    case ets:lookup(Table, Id) of
        [Child] ->
            Child;
        [] ->
            not_found
    end.







count(Table) ->
    ets:info(Table, size).







lookup_by_pid(Table, Pid) ->
    case ets:match_object(Table
                         ,#?CHILD{id = '_'
                                 ,pid = Pid
                                 ,plan = '_'
                                 ,count = '_'
                                 ,count2 = '_'
                                 ,restart_count = '_'
                                 ,start = '_'
                                 ,plan_element_index = '_'
                                 ,plan_length = '_'
                                 ,timer_reference = '_'
                                 ,terminate_timeout = '_'
                                 ,extra = '_'
                                 ,modules = '_'
                                 ,type = '_'
                                 ,append = '_'}) of
        [Child] ->
            Child;
        [] ->
            not_found
    end.







lookup_appended(Table) ->
    ets:match_object(Table
                    ,#?CHILD{id = '_'
                            ,pid = '_'
                            ,plan = '_'
                            ,count = '_'
                            ,count2 = '_'
                            ,restart_count = '_'
                            ,start = '_'
                            ,plan_element_index = '_'
                            ,plan_length = '_'
                            ,timer_reference = '_'
                            ,terminate_timeout = '_'
                            ,extra = '_'
                            ,modules = '_'
                            ,type = '_'
                            ,append = true}).







insert(Table, Child) ->
    true = ets:insert(Table, Child),
    ok.







delete(Table, Id) ->
    true = ets:delete(Table, Id),
    ok.







tab2list(Table) ->
    ets:tab2list(Table).







combine_children(DefChildSpec, Table) ->
    AppendedChildren =
        [director_wrapper:combine_child(director_wrapper:c2cs(Child)
                                       ,DefChildSpec)
        || Child <- lookup_appended(Table)],
    [insert(Table, director_wrapper:cs2c(AppendedChild)) || AppendedChild <- AppendedChildren],
    ok.







separate_children(DefChildSpec, Table) ->
    AppendedChildren =
        [director_wrapper:separate_child(director_wrapper:c2cs(Child)
                                        ,DefChildSpec)
        || Child <- lookup_appended(Table)],
    [insert(Table, director_wrapper:cs2c(AppendedChild)) || AppendedChild <- AppendedChildren],
    ok.