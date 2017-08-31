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
%% @version  17.9
%% @doc
%%           PI functions for keeping, updating and fetching
%%           childspecs data.<br/>
%%           director supports tho type of tables: list, ets.
%% @end
%% -------------------------------------------------------------------------------------------------


-module(director_table).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([create/1
        ,insert/3
        ,delete/3
        ,lookup/3
        ,lookup_by_pid/3
        ,lookup_appended/2
        ,combine_children/3
        ,separate_children/3
        ,count/2
        ,delete_table/2
        ,tab2list/2]).





%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





%% Dependencies:
%%  #?CHILD{}
-include("internal/director_child.hrl").





%% -------------------------------------------------------------------------------------------------
%% API functions:





create(list) ->
    director_table_list:create();
create(ets) ->
    director_table_ets:create().







delete_table(Tab, list) ->
    director_table_list:delete_table(Tab);
delete_table(Tab, ets) ->
    director_table_ets:delete_table(Tab).







lookup(Tab, Id, list) ->
    director_table_list:lookup(Tab, Id);
lookup(Tab, Id, ets) ->
    director_table_ets:lookup(Tab, Id).








count(Tab, list) ->
    director_table_list:count(Tab);
count(Tab, ets) ->
    director_table_ets:count(Tab).








lookup_by_pid(Tab, Pid, list) ->
    director_table_list:lookup_by_pid(Tab, Pid);
lookup_by_pid(Tab, Pid, ets) ->
    director_table_ets:lookup_by_pid(Tab, Pid).







lookup_appended(Tab, list) ->
    director_table_list:lookup_appended(Tab);
lookup_appended(Tab, ets) ->
    director_table_ets:lookup_appended(Tab).








insert(Tab, Child, list) ->
    director_table_list:insert(Tab, Child);
insert(Tab, Child, ets) ->
    director_table_ets:insert(Tab, Child).







delete(Tab, Id, list) ->
    director_table_list:delete(Tab, Id);
delete(Tab, Id, ets) ->
    director_table_ets:delete(Tab, Id).







tab2list(Tab, list) ->
    director_table_list:tab2list(Tab);
tab2list(Tab, ets) ->
    director_table_ets:tab2list(Tab).







combine_children(DefChildSpec, Tab, list) ->
    director_table_list:combine_children(DefChildSpec, Tab);
combine_children(DefChildSpec, Tab, ets) ->
    director_table_ets:combine_children(DefChildSpec, Tab).







separate_children(DefChildSpec, Tab, list) ->
    director_table_list:separate_children(DefChildSpec, Tab);
separate_children(DefChildSpec, Tab, ets) ->
    director_table_ets:separate_children(DefChildSpec, Tab).