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
-module(director_utils).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([concat/2
        ,proper/2
        ,option/4
        ,value/3
        ,has_duplicate/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("internal/director_child.hrl").
-include("internal/director_defaults.hrl").

%% -------------------------------------------------------------------------------------------------
%% Functions:

concat(List1, List2) ->
    proper(proper(List1, []), lists:reverse(proper(List2, []))).


option(Key, Opts, Filter, Def) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} ->
            Filter(Val);
        false ->
            Def
    end.


value(Key, List, Def) ->
    case lists:keyfind(Key, 1, List) of
        {_, Val} ->
            Val;
        _ -> % {Key,..., ...} & false
            Def
    end.


has_duplicate(L) ->
    has_duplicate(L, []).


proper([H|T], Ret) when erlang:is_list(T) ->
    proper(T, [H | Ret]);
proper([H|T], Ret) ->
    proper([], [T, H | Ret]);
proper([], Ret) ->
    Ret.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

has_duplicate([Id|Ids], Ids2) ->
    case lists:member(Id, Ids2) of
        true ->
            {true, Id};
        false ->
            has_duplicate(Ids, [Id|Ids2])
    end;
has_duplicate(_, _) -> % ([], Ids2)
    false.