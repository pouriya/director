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
%% @version  18.4.29
%% -------------------------------------------------------------------------------------------------
-module(director_SUITE).
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
        ,'10'/1
        ,'11'/1
        ,'12'/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(DIRECTOR, director_name).
-define(CHILD, child_name).
-define(CALLBACK, director_callback).
-define(CHILD_MODULE, director_child_).
-define(TAB_NAME, director_table).
-define(TAB, list).
-define(DB_OPTS, [{table, ?TAB}, {init_arg, ?TAB_NAME}]).
-define(START_OPTIONS, [{debug, [trace]}
                       ,{db, ?DB_OPTS}]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------------------------------------
%% ct callbacks:


all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-8)].


init_per_suite(Config) ->
    application:start(sasl),
    application:start(mnesia),
    Config.


end_per_suite(Config) ->
    application:stop(sasl),
    application:stop(mnesia),
    Config.


init_per_testcase(_TestCase, Config) ->
    erlang:process_flag(trap_exit, true),
    Config.


end_per_testcase(_TestCase, _Config) ->
    catch erlang:exit(erlang:whereis(?DIRECTOR), kill),
    director_test_utils:flush_handle_returns(),
    catch  mnesia:delete_table(?TAB_NAME),
    ok.

%% -------------------------------------------------------------------------------------------------
%% Test cases:


'1'(_Config) ->
    InitArg = erlang:self(),
    Start =
        fun() ->
            erlang:process_flag(trap_exit, true),
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg)
        end,

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {stop, foo} end),
    ?assertEqual(ok, receive {error, foo} -> ok after 500 -> error end),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> ignore end),
    ?assertEqual(ok, receive ignore -> ok after 500 -> error end),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> bar end),
    ?assertEqual(ok, receive {error, {return, [{value, bar}|_]}} -> ok after 500 -> error end),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> fun() -> erlang:exit(baz) end end),
    ?assertEqual(ok, receive {error, {crash, [{reason, baz}|_]}} -> ok after 500 -> error end),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg} end),
    StartResult = receive {ok, _}=Ok -> Ok after 500 -> error end,
    ?assertEqual(StartResult, {ok, erlang:whereis(?DIRECTOR)}),
    Stop =
        fun(Rsn) ->
            ?assertEqual(ok, director:stop(?DIRECTOR, Rsn, 500))
        end,
    spawn_link(Stop),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, []} end),
    StartResult2 = receive {ok, _}=Ok2 -> Ok2 after 500 -> error end,
    ?assertEqual(StartResult2, {ok, erlang:whereis(?DIRECTOR)}),
    spawn_link(Stop),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [], #{}} end),
    StartResult3 = receive {ok, _}=Ok3 -> Ok3 after 500 -> error end,
    ?assertEqual(StartResult3, {ok, erlang:whereis(?DIRECTOR)}),
    spawn_link(Stop),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [], []} end),
    StartResult4 = receive {ok, _}=Ok4 -> Ok4 after 500 -> error end,
    ?assertEqual(StartResult4, {ok, erlang:whereis(?DIRECTOR)}),
    spawn_link(Stop),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [], #{}, []} end),
    StartResult5 = receive {ok, _}=Ok5 -> Ok5 after 500 -> error end,
    ?assertEqual(StartResult5, {ok, erlang:whereis(?DIRECTOR)}),
    spawn_link(Stop),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)).


'2'(_Config) ->
    InitArg = erlang:self(),
    Id = foo,
    ChState = child_state,
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,terminate_timeout => 0
                ,modules => Mods
                ,append => false
                ,type => worker
                ,state => ChState
                ,delete => true},

    ?assertEqual(ok, director:check_childspec(ChildSpec)),

    Start =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec], ?START_OPTIONS} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
                                                                                                           ChState2 =:= ChState andalso
                                                                                                           State2 =:= InitArg andalso
                                                                                                           erlang:is_pid(Pid) ->
                                          {ok, ChState2, State2, [{log, true}]}
                                      end),
    StartResult = receive {ok, _}=Ok -> Ok after 500 -> error end,
    ?assertEqual(StartResult, {ok, erlang:whereis(?DIRECTOR)}),
    ?assertEqual({ok, ChildSpec}, director:get_childspec(?DIRECTOR, Id)),

    ?assertMatch([{Id, _Pid, worker, Mods}], director:which_children(?DIRECTOR)),

    count_children(?DIRECTOR, 1,1,1,0),

    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),

    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),

    ?assertEqual({error, running}, director:delete_child(?DIRECTOR, Id)),

    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),

    Exit =
        fun(Rsn) ->
            {ok, PidX} = director:get_pid(?DIRECTOR, Id),
            erlang:exit(PidX, Rsn)
        end,
    Exit(kill),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, killed, State2, #{restart_count := 1}]) when Id2 =:= Id andalso
                                                                                                       ChState2 =:= ChState andalso
                                                                                                       State2 =:= InitArg ->
                                          {restart, ChState2, State2, [{log, true}]}
                                      end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 1}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),

    Exit(oops),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, oops, State2, #{restart_count := 2}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg ->
            {{restart, 500}, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertEqual([{Id, restarting, worker, [?CHILD_MODULE]}], director:which_children(?DIRECTOR)),
    timer:sleep(50),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 2}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),

    Exit(aah),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, aah, State2, #{restart_count := 3}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg ->
            {wait, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertEqual([{Id, undefined, worker, [?CHILD_MODULE]}], director:which_children(?DIRECTOR)),
    erlang:spawn_link(fun() -> ?assertMatch({ok, _}, director:restart_child(?DIRECTOR, Id)), receive after infinity -> ok end end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 3}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),

    Exit(aah),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, aah, State2, #{restart_count := 4}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg ->
            {delete, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),

    erlang:spawn_link(fun() -> ?assertMatch({ok, _}, director:start_child(?DIRECTOR, ChildSpec)), receive after infinity -> ok end end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),

    Exit(bye),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, bye, State2, #{restart_count := 1}]) when Id2 =:= Id andalso
            ChState2 =:= ChState andalso
            State2 =:= InitArg ->
            {stop, ChState2, State2, [{log, true}]}
                                      end),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([bye, Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(ok, receive {'EXIT', _, bye} -> ok after 500 -> error end),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    erlang:spawn_link(Start),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec], ?START_OPTIONS} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
                                                                                                           ChState2 =:= ChState andalso
                                                                                                           State2 =:= InitArg andalso
                                                                                                           erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    Exit(shutup),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, shutup, State2, #{restart_count := 1}]) when Id2 =:= Id andalso
                                                                                                    ChState2 =:= ChState andalso
                                                                                                    State2 =:= InitArg ->
            {{stop, '_'}, ChState2, State2, [{log, true}]}
                end),
    director_test_utils:handle_return(?CALLBACK, terminate, fun(['_', Arg]) when Arg =:= InitArg -> ok end),
    timer:sleep(50),
    ?assertEqual(ok, receive {'EXIT', _, '_'} -> ok after 500 -> error end),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)).




'3'(_Config) ->
    InitArg = erlang:self(),
    Start = {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]},
    Mods = [?CHILD_MODULE],
    DefChildSpec = #{start => Start
                    ,terminate_timeout => 1000
                    ,modules => Mods},
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [], DefChildSpec} end),
    ?assertEqual(DefChildSpec, director:get_default_childspec(?DIRECTOR)),
    Id = foo,
    ChildSpec = #{id => Id, append => true, modules => []},
    erlang:spawn(fun() -> ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR,  ChildSpec)) end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),

    {ok, ChildSpec2} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start, maps:get(start, ChildSpec2)),
    ?assertEqual(2000, maps:get(terminate_timeout, ChildSpec2)),
    ?assertEqual(Mods, maps:get(modules, ChildSpec2)),

    Start2 = {?CHILD_MODULE, start_link, [fun() -> {ok, undefined} end]},
    DefChildSpec2 = #{start => Start2, modules => [?CHILD_MODULE]},
    ?assertEqual(ok, director:change_default_childspec(?DIRECTOR, DefChildSpec2)),
    {ok, ChildSpec3} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start2, maps:get(start, ChildSpec3)),
    ?assertEqual(1000, maps:get(terminate_timeout, ChildSpec3)),
    ?assertEqual([?CHILD_MODULE], maps:get(modules, ChildSpec3)).


'4'(_config) ->
    InitArg = erlang:self(),
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec = #{id => Id, start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}},
    ChildSpec2 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}
                 ,terminate_timeout => 1000
                 ,modules => [?CHILD_MODULE]
                 ,append => false
                 ,type => worker
                 ,state => undefined
                 ,delete => true},
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec]} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    {ok, ChildSpec3} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(ChildSpec2, ChildSpec3),
    Id2 = bar,
    ChildSpec4 = #{id => Id2
                 ,start => {?CHILD_MODULE, start_link, [F]}
                 ,type => supervisor},
    erlang:spawn(fun() -> ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR, ChildSpec4)) end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id3, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id3 =:= Id2 andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    {ok, ChildSpec5} = director:get_childspec(?DIRECTOR, Id2),
    ?assertEqual(infinity, maps:get(terminate_timeout, ChildSpec5)).


'5'(Config) ->
    Src = "-module(director_callback2).\n"
          "-export([init/1\n"
          "        ,terminate/2\n"
          "        ,handle_start/4\n"
          "        ,handle_terminate/5]).\n\n"
          "init(_InitArg) ->\n"
          "    {ok\n"
          "    ,undefined\n"
          "    ,~s}.\n\n\n"
          "handle_terminate(_, ChState, _, State, _) ->\n"
          "    {ok, ChState, State, [{log, true}]}.\n\n\n"
          "handle_start(foo, undefined, undefined, #{pid := Pid, restart_count := 0}) when erlang:is_pid(Pid)  ->\n"
          "    {ok, undefined, undefined, [{log, true}]}~s\n\n\n"
          "terminate(_, _) ->\n"
          "    ok.\n",

    Callback = director_callback2,
    Id1 = foo,
    ChildMod1 = director_child_,
    Children1 = io_lib:format("[#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}]"
                             ,[Id1, ChildMod1]),
    Src1 = io_lib:format(Src, [Children1, "."]),
    ct:pal("Source 1:\n~s\n", [Src1]),

    Dir = filename:join(?config(data_dir, Config), "src"),
    code:add_patha(Dir),
    File = filename:join([Dir, erlang:atom_to_list(Callback) ++ ".erl"]),

    ?assertEqual(ok, file:write_file(File, Src1)),
    ?assertEqual({ok, Callback}, compile:file(File, [return_errors, {outdir, Dir}])),
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,Callback
                                                ,undefined
                                                ,?START_OPTIONS)),
    ?assertMatch({ok, [{foo, _}]}, director:get_pids(?DIRECTOR)),

    Id2 = bar,
    ChildMod2 = director_child_,
    Children2 = io_lib:format("[#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}\n"
                              ",#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}]"
                             ,[Id1, ChildMod1, Id2, ChildMod2]),
    HandleStartClause2 = ";\nhandle_start(bar, undefined, undefined, #{pid := Pid, restart_count := 0}) when erlang:is_pid(Pid)  ->\n"
                         "    {ok, undefined, undefined, [{log, true}]}.",
    Src2 = io_lib:format(Src, [Children2, HandleStartClause2]),
    ct:pal("Source 2:\n~s\n", [Src2]),

    ?assertEqual(ok, sys:suspend(?DIRECTOR)),

    ?assertEqual(ok, file:write_file(File, Src2)),
    ?assertEqual({ok, Callback}, compile:file(File, [return_errors, {outdir, Dir}])),
    ?assertEqual({module, Callback}, c:l(Callback)),

    ?assertEqual(ok, sys:change_code(?DIRECTOR, Callback, undefined, undefined)),
    ?assertEqual(ok, sys:resume(?DIRECTOR)),
    ct:pal("Childrens:\n~p\n", [director:which_children(?DIRECTOR)]),
    ?assertMatch({ok, _}, director:restart_child(?DIRECTOR, Id2)),
    director:stop(?DIRECTOR),
    file:delete(File).


'6'(_Config) ->
    InitArg = erlang:self(),
    Tab = mnesia,
    TabOpts = [{table, Tab}, {init_arg, ?TAB_NAME}],
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec = #{id => Id, start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}},
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec], [{db, TabOpts}|?START_OPTIONS]} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    ?assertEqual(ok, receive {ok, _} -> ok after 500 -> error end),
    StartDir2 =
        fun() ->
            InitArg ! director:start_link({local, director_name_2}, ?CALLBACK, InitArg, [{db, TabOpts}|?START_OPTIONS]),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir2),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec], [{db, TabOpts}|?START_OPTIONS]} end),
    ?assertEqual(ok, receive {ok, _} -> ok after 500 -> error end),
    Pid = erlang:whereis(?CHILD),
    ?assertEqual({ok, Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, Pid}, director:get_pid(director_name_2, Id)),
    erlang:spawn(fun() -> ?assertEqual(ok, director:terminate_and_delete_child(?DIRECTOR, Id)) end),
    director_test_utils:handle_return(?CALLBACK, handle_terminate, fun([foo, undefined, shutdown, _, _]) -> {ok, undefined, undefined, [{log, true}]} end),
    timer:sleep(50),
    ?assertEqual({error, not_found}, director:get_pid(director_name_2, Id)).


'7'(_Config) ->
    InitArg = erlang:self(),
    Tab = mnesia,
    TabOpts = [{table, Tab}, {init_arg, ?TAB_NAME}, {delete, false}],
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec1 = #{id => Id
                  ,start => {?CHILD_MODULE, start_link, [F]}
                  ,delete => false},
    Id2 = bar,
    ChildSpec2 = #{id => Id2
                  ,start => {?CHILD_MODULE, start_link, [F]}
                  ,delete => true},
    StartDir =
        fun() ->
            InitArg ! director:start_link(?CALLBACK
                                         ,InitArg
                                         ,[{db, TabOpts}|?START_OPTIONS]),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec1]} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id3, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id3 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    Res1 =
        receive
            Msg ->
                Msg
        end,
    ?assertMatch({ok, _}, Res1),
    Pid1 = erlang:element(2, Res1),

    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec1, ChildSpec2]} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id3, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id3 =:= Id2 andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),

    Res2 =
        receive
            Msg2 ->
                Msg2
        end,
    ?assertMatch({ok, _}, Res2),
    Pid2 = erlang:element(2, Res2),

    ?assertMatch({ok, _Pid}, director:get_pid(Pid1, Id)),
    ResGetPid1 = director:get_pid(Pid1, Id),
    ?assertMatch({ok, _Pid}, director:get_pid(Pid2, Id)),
    ResGetPid2 = director:get_pid(Pid2, Id),
    ?assertEqual(ResGetPid1, ResGetPid2),
    erlang:spawn_link(fun() -> ?assertEqual(ok, director:stop(Pid1)) end),
    director_test_utils:handle_return(?CALLBACK, handle_terminate, fun([foo, undefined, shutdown, _, _]) -> {ok, undefined, undefined, [{log, true}]} end),
    director_test_utils:handle_return(?CALLBACK, terminate, fun([normal, _]) -> {ok, [{log, true}]} end),
    ?assertEqual({error, undefined}, director:get_pid(Pid2, Id)),
    ?assertMatch({ok, _}, director:get_pid(Pid2, Id2)),
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec1]} end),Res3 =
        receive
            {ok, _}=Msg3 ->
                Msg3
        end,
    ?assertMatch({ok, _}, Res3),
    Pid3 = erlang:element(2, Res3),

    ?assertEqual({error, undefined}, director:get_pid(Pid3, Id)),
    ?assertMatch({ok, _}, director:get_pid(Pid3, Id2)),

    erlang:spawn_link(fun() -> ?assertMatch({ok, _Pid}, director:restart_child(Pid3, Id)) end),
        director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id3, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id3 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    Res4 = director:get_pid(Pid2, Id),
    ?assertMatch({ok, _Pid}, Res4),
    ?assert(erlang:is_pid(erlang:element(2, Res4))).


'8'(_Config) ->
    InitArg = erlang:self(),
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg, ?START_OPTIONS),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg} end),
    ?CALLBACK = supervisor:get_callback_module(?DIRECTOR).


'9'(_Cfg) ->
    InitArg = erlang:self(),
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg, ?START_OPTIONS),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg} end),

    F = fun() -> {ok, undefined} end,
    {ok, ChPid} = ?CHILD_MODULE:start_link(F),
    Id = foo,
    ChildSpec1 = #{id => Id, start => {?CHILD_MODULE, start_link, [F]}},
    ?assertEqual(ok, director:become_supervisor(?DIRECTOR, ChPid, ChildSpec1)),
    ?assertEqual({ok, ChPid}, director:get_pid(?DIRECTOR, Id)),
    erlang:exit(ChPid, kill),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_exit
                                     ,fun([Id2, ChState2, killed, State2, #{restart_count := 1}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg ->
            {restart, ChState2, State2, [{log, true}]}
                                      end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 1}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    timer:sleep(50),
    ?assertMatch({ok, _}, director:get_pid(?DIRECTOR, Id)),

    ?assertEqual({error, noproc}, director:become_supervisor(?DIRECTOR, ChPid, ChildSpec1)),
    {ok, ChPid2} = director:get_pid(?DIRECTOR, Id),
    ?assertEqual({error, {already_started, ChPid2}}, director:become_supervisor(?DIRECTOR, ChPid2, ChildSpec1)),
    erlang:spawn_link(fun() -> ?assertEqual(ok, director:terminate_child(?DIRECTOR, Id)) end),
    director_test_utils:handle_return(?CALLBACK, handle_terminate, fun([foo, undefined, shutdown, _, _]) -> {ok, undefined, undefined, [{log, true}]} end),
    {ok, ChPid3} = ?CHILD_MODULE:start_link(F),
    ?assertEqual({error, {already_present, Id}}, director:become_supervisor(?DIRECTOR, ChPid3, ChildSpec1)),
    ?assertEqual({error, {duplicate_child_name, Id}}, director:become_supervisor(?DIRECTOR, ChPid3, ChildSpec1#{start => ?CHILD_MODULE})),
    ok.


'10'(_Cfg) ->
    InitArg = erlang:self(),
    F = fun() -> {ok, undefined} end,
    Id = foo,
    ChildSpec1 = #{id => Id, start => {?CHILD_MODULE, start_link, [F]}},
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg, ?START_OPTIONS),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec1]} end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    {ok, ChPid} = director:get_pid(?DIRECTOR, Id),
    ?assertEqual(ok, director:delete_running_child(?DIRECTOR, ChPid)),
    timer:sleep(50),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)).


'11'(_Cfg) ->
    InitArg = erlang:self(),
    Id = foo,
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg, ?START_OPTIONS),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg} end),

    {ok, []} = director:get_pids(?DIRECTOR),
    ?assertEqual({error, not_found}, director:terminate_child(?DIRECTOR, Id)),
    ?assertEqual({error, not_found}, director:delete_running_child(?DIRECTOR, erlang:self())),
    ?assertEqual({error, not_found}, director:restart_child(?DIRECTOR, Id)).


'12'(_Cfg) ->
    InitArg = erlang:self(),
    F =
        fun() ->
            InitArg ! erlang:self(),
            receive
                M ->
                    M
            end
        end,
    Id = foo,
    ChildSpec1 = #{id => Id, start => {?CHILD_MODULE, start_link, [F]}},
    StartDir =
        fun() ->
            InitArg ! director:start_link({local, ?DIRECTOR}, ?CALLBACK, InitArg, ?START_OPTIONS),
            receive after infinity -> ok end
        end,
    erlang:spawn_link(StartDir),
    director_test_utils:handle_return(?CALLBACK, init, fun([Arg]) when Arg =:= InitArg -> {ok, InitArg, [ChildSpec1]} end),
    receive
        Pid ->
            Pid ! {ok, undefined}
    end,
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_start
                                     ,fun([Id2, ChState2, State2, #{pid := Pid2, restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg andalso
            erlang:is_pid(Pid2) ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    erlang:spawn_link(fun() -> director:terminate_child(?DIRECTOR, Id) end),
    director_test_utils:handle_return(?CALLBACK
                                     ,handle_terminate
                                     ,fun([Id2, ChState2, shutdown, State2, #{restart_count := 0}]) when Id2 =:= Id andalso
            ChState2 =:= undefined andalso
            State2 =:= InitArg ->
            {ok, ChState2, State2, [{log, true}]}
                                      end),
    erlang:spawn_link(fun() -> director:restart_child(?DIRECTOR, Id) end),
    receive
        Pid2 when erlang:is_pid(Pid2) ->
            io:format("Pid: ~p~n", [Pid2]),
            Pid2 ! {stop, oops}
    end,
    timer:sleep(10),
    ?assertEqual({error, undefined}, director:get_pid(?DIRECTOR, Id)),
    ok.


%% -------------------------------------------------------------------------------------------------
%% Internal functions:

count_children(Director, Specs, Actives, Workers, Sups) ->
    CountChildren = director:count_children(Director),

    ?assertEqual(Specs, ?config(specs, CountChildren)),
    ?assertEqual(Actives, ?config(active, CountChildren)),
    ?assertEqual(Workers, ?config(workers, CountChildren)),
    ?assertEqual(Sups, ?config(supervisors, CountChildren)).