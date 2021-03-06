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
%% -------------------------------------------------------------------------------------------------
-module(director_table_ets_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1
        ,'7'/1
        ,'8'/1
        ,'9'/1
        ,'10'/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(TAB_MOD, 'director_table_ets').
-define(TAB_NAME, director_table_name).
-define(TAB_INIT_ARG, {value, ?TAB_NAME}).
-define(TESTER_MODULE, 'director_table_').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------------------------------------
%% ct callbacks:

all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-8)].


init_per_suite(Config) ->
    application:start(sasl),
    Config.


end_per_suite(Config) ->
    application:stop(sasl),
    Config.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    catch ets:delete(?TAB_NAME),
    ok.

%% -------------------------------------------------------------------------------------------------

'1'(_) ->
    ?TESTER_MODULE:'1'(?TAB_MOD, ?TAB_INIT_ARG).


'2'(_) ->
    ?TESTER_MODULE:'2'(?TAB_MOD, ?TAB_INIT_ARG).


'3'(_) ->
    ?TESTER_MODULE:'3'(?TAB_MOD, ?TAB_INIT_ARG).


'4'(_) ->
    ?TESTER_MODULE:'4'(?TAB_MOD, ?TAB_INIT_ARG).


'5'(_) ->
    ?TESTER_MODULE:'5'(?TAB_MOD, ?TAB_INIT_ARG).


'6'(_) ->
    ?TESTER_MODULE:'6'(?TAB_MOD, ?TAB_INIT_ARG).


'7'(_) ->
    ?TESTER_MODULE:'7'(?TAB_MOD, ?TAB_INIT_ARG).


'8'(_) ->
    ?TESTER_MODULE:'8'(?TAB_MOD, ?TAB_INIT_ARG).


'9'(_) ->
    ?assert(erlang:is_list(?TAB_MOD:options())).


'10'(_) ->
    ets:new(?TAB_NAME, ?TAB_MOD:options()),
    ?assertMatch({ok, _}, ?TAB_MOD:create({value, ?TAB_NAME})),
    ets:delete(?TAB_NAME),

    ets:new(?TAB_NAME, [public, named_table, {keypos, 2}]),
    ?assertMatch({ok, _}, ?TAB_MOD:create({value, ?TAB_NAME})),
    ets:delete(?TAB_NAME),

    ets:new(?TAB_NAME, [protected, named_table, {keypos, 2}]),
    ?assertMatch({ok, _}, ?TAB_MOD:create({value, ?TAB_NAME})),
    ets:delete(?TAB_NAME),

    timer:sleep(30),
    Creator = erlang:spawn_link(fun() -> ets:new(?TAB_NAME, [protected, named_table]), receive _ -> ets:delete(?TAB_NAME) end end),
    timer:sleep(30),
    ?assertMatch({hard_error, {table_info, [{protection, _}|_]}}, ?TAB_MOD:create({value, ?TAB_NAME})),
    Creator ! done,

    timer:sleep(30),
    Creator2 = erlang:spawn_link(fun() -> ets:new(?TAB_NAME, [public, named_table]), receive _ -> ets:delete(?TAB_NAME) end end),
    timer:sleep(30),
    ?assertMatch({hard_error, {table_info, [{keypos, _}|_]}}, ?TAB_MOD:create({value, ?TAB_NAME})),
    Creator2 ! done,

    timer:sleep(30),
    Creator3 = erlang:spawn_link(fun() -> ets:new(?TAB_NAME, [public, named_table, bag, {keypos,2}]), receive _ -> ets:delete(?TAB_NAME) end end),
    timer:sleep(30),
    ?assertMatch({hard_error, {table_info, [{type, _}|_]}}, ?TAB_MOD:create({value, ?TAB_NAME})),
    Creator3 ! done,
    timer:sleep(30),

    ets:new(?TAB_NAME, [protected, named_table]),
    ?assertMatch({hard_error, {table_info, [{keypos, _}|_]}}, ?TAB_MOD:create({value, ?TAB_NAME})),
    ets:delete(?TAB_NAME),

    ets:new(?TAB_NAME, [protected, named_table, {keypos, 2}, bag]),
    ?assertMatch({hard_error, {table_info, [{type, _}|_]}}, ?TAB_MOD:create({value, ?TAB_NAME})),
    ets:delete(?TAB_NAME),

    ?assertMatch({hard_error, {table_existence, _}}, ?TAB_MOD:delete_table(?TAB_NAME)),
    ets:new(?TAB_NAME, [public, named_table, {keypos, 2}]),
    timer:sleep(30),
    ?assertMatch(ok, ?TAB_MOD:delete_table(?TAB_NAME)),

    ?assertMatch({hard_error, {table_existence, _}}, ?TAB_MOD:lookup_id(?TAB_NAME, foo)),
    ?assertMatch({hard_error, {table_existence, _}}, ?TAB_MOD:lookup_pid(?TAB_NAME, foo)),

    Creator4 = erlang:spawn_link(fun() -> ets:new(?TAB_NAME, [private, named_table]), receive _ -> ets:delete(?TAB_NAME) end end),
    timer:sleep(30),
    ?assertMatch({hard_error, {table_info, [{protection, _}|_]}}, ?TAB_MOD:lookup_id(?TAB_NAME, foo)),
    Creator4 ! done,
    timer:sleep(30),



%%    ets:new(?TAB_NAME, [?TAB_MOD:options()]),
%%    ?assertMatch({ok, _}, ?TAB_MOD:create({value, ?TAB_NAME})),
%%    ets:delete(?TAB_NAME),
%%
%%    ets:new(?TAB_NAME, [?TAB_MOD:options()]),
%%    ?assertMatch({ok, _}, ?TAB_MOD:create({value, ?TAB_NAME})),
%%    ets:delete(?TAB_NAME),
ok.

