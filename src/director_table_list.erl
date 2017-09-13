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
%% @version  17.9.13
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(director_table_list).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([create/1
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

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").

%% -------------------------------------------------------------------------------------------------
%% API functions:

create(list) ->
    {ok, []}.


delete_table(_Tab) ->
    ok.


lookup(Tab, Id) ->
    case lists:keyfind(Id, 2, Tab) of
        false ->
            not_found;
        Child ->
            Child
    end.


count(Tab) ->
    erlang:length(Tab).


lookup_by_pid(Tab, Pid) ->
    case lists:keyfind(Pid, 3, Tab) of
        false ->
            not_found;
        Child ->
            Child
    end.


lookup_appended(Tab) ->
    [Child || #?CHILD{append = Append}=Child <- Tab, Append == true].


insert(Tab, #?CHILD{id = Id}=Child) ->
    case lists:keyfind(Id, 2, Tab) of
        false ->
            [Child|Tab];
        _ ->
            {value, _, Tab2} = lists:keytake(Id, 2, Tab),
            [Child|Tab2]
    end.


delete(Tab, Id) ->
    case lists:keyfind(Id, 2, Tab) of
        false ->
            Tab;
        _ ->
            lists:keydelete(Id, 2, Tab)
    end.


tab2list(Tab) ->
    Tab.


combine_children(DefChildSpec, Tab) ->
    AppendedChildren = [director_utils:combine_child(director_utils:c2cs(Child), DefChildSpec)
                       || Child <- lookup_appended(Tab)],
    lists:foldl(fun(AppendedChild, Tab2) ->  insert(Tab2, director_utils:cs2c(AppendedChild)) end
               ,Tab
               ,AppendedChildren).


separate_children(DefChildSpec, Tab) ->
    AppendedChildren = [director_utils:separate_child(director_utils:c2cs(Child), DefChildSpec)
                       || Child <- lookup_appended(Tab)],
    lists:foldl(fun(AppendedChild, Tab2) ->  insert(Tab2, director_utils:cs2c(AppendedChild)) end
               ,Tab
               ,AppendedChildren).