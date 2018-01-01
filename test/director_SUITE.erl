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
%% @version  17.12.30
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
        ,'12'/1
        ,'13'/1
        ,'14'/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(DIRECTOR, director_name).
-define(CHILD, child_name).
-define(CALLBACK, director_callback).
-define(CHILD_MODULE, director_child).
-define(START_OPTIONS, [{debug, [trace]}
                       ,{log_validator, fun log_validator/4}
                       ,{table_module, director_table_mnesia}
                       ,{table_init_argument, director_table}]).

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
    ok.

%% -------------------------------------------------------------------------------------------------
%% Test cases:


'1'(_Config) ->
    ?assertEqual({error, foo}, director:start_link(?CALLBACK, fun()-> {stop, foo} end)),

    ?assertEqual(ignore, director:start_link(?CALLBACK, fun() -> ignore end)),

    ?assertMatch({error, {return, [{value, bar}|_]}}
                ,director:start_link(?CALLBACK, fun()-> bar end)),

    ?assertMatch({error, {crash, [{reason, baz}|_]}}
                ,director:start_link(?CALLBACK, fun()-> erlang:exit(baz) end)),

    ?assertEqual(director:start_link({local, ?DIRECTOR}
                                    ,?CALLBACK
                                    ,fun() -> {ok, undefined, []} end)
                ,{ok, erlang:whereis(?DIRECTOR)}),

    ?assertEqual(ok, director:stop(?DIRECTOR)),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)),

    ?assertEqual(director:start_link({local, ?DIRECTOR}
                                    ,?CALLBACK
                                    ,fun() -> {ok, undefined, [], #{}} end)
                ,{ok, erlang:whereis(?DIRECTOR)}),
    ?assertEqual(ok, director:stop(?DIRECTOR)),

    ?assertEqual(director:start_link({local, ?DIRECTOR}
                                    ,?CALLBACK
                                    ,fun() -> {ok, undefined, [], []} end)
                ,{ok, erlang:whereis(?DIRECTOR)}),
    ?assertEqual(ok, director:stop(?DIRECTOR)),

    ?assertEqual(director:start_link({local, ?DIRECTOR}
                                    ,?CALLBACK
                                    ,fun() -> {ok, undefined, [], #{}, []} end)
                ,{ok, erlang:whereis(?DIRECTOR)}),
    ?assertEqual(ok, director:stop(?DIRECTOR)).


'2'(_Config) ->
    Id = foo,
    State = state,
    Plan = fun(_, killed, _, St) -> {restart, St} end,
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE
                          ,start_link
                          ,[{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,terminate_timeout => 0
                ,modules => Mods
                ,append => false
                ,type => worker
                ,log_validator => fun(_, _, _, _) -> long end
                ,state => undefined
                ,delete => true},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, State, [ChildSpec]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual({ok, ChildSpec}, director:get_childspec(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid, worker, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,1,0),
    ?assertEqual({ok, Plan}, director:get_plan(?DIRECTOR, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),
    ?assertEqual({error, running}, director:delete_child(?DIRECTOR, Id)),
    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),

    [begin
         {ok, PidX} = director:get_pid(?DIRECTOR, Id),
         erlang:exit(PidX, kill)
     end || _ <- lists:seq(1, 10)],
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual(ok, director:terminate_child(?DIRECTOR, Id)),
    ?assertMatch(undefined, erlang:whereis(?CHILD)),
    ?assertMatch([{Id, undefined, worker, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,1,0),
    ?assertEqual({error, undefined}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),

    ?assertMatch({ok, _Pid}, director:restart_child(?DIRECTOR, Id)),
    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),
    ?assertMatch([{Id, _Pid, worker, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,1,0),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),

    ?assertEqual(ok, director:terminate_child(?DIRECTOR, erlang:whereis(?CHILD))),
    ?assertEqual(ok, director:delete_child(?DIRECTOR, Id)),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 0,0,0,0),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),
    ?assertEqual(ok, director:stop(?DIRECTOR)),

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual(ok, director:terminate_and_delete_child(?DIRECTOR, Id)),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),
    ?assertEqual(ok, director:stop(?DIRECTOR)),

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual(ok, director:terminate_child(?DIRECTOR, Id)),
    ?assertEqual(ok, director:terminate_and_delete_child(?DIRECTOR, Id)),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),
    ?assertEqual(ok, director:stop(?DIRECTOR)).


'3'(_Config) ->
    Id = foo,
    RestartTimeout = 100,
    Plan =
        fun
            (_, killed ,1, St) ->
                {restart, St};
            (_, killed ,2, St) ->
                {{restart, RestartTimeout}, St};
            (_, killed ,3, St) ->
                {wait, St};
            (_, killed ,4, St) ->
                {delete, St}
        end,
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE
                          ,start_link
                          ,[{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,terminate_timeout => infinity
                ,modules => Mods
                ,append => false
                ,type => supervisor
                ,log_validator => fun(_, _, _, _) -> long end
                ,state => undefined
                ,delete => true},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, undefined, []} end,

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR,  ChildSpec)),
    ?assertEqual({ok, ChildSpec}, director:get_childspec(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertEqual({ok, Plan}, director:get_plan(?DIRECTOR, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),
    ?assertEqual({error, running}, director:delete_child(?DIRECTOR, Id)),
    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),

    % plan: restart
    erlang:exit(erlang:whereis(?CHILD), kill),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),

    % plan: {restart, Timeout}
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(trunc(RestartTimeout/10)),
    ?assertMatch([{Id, restarting, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, restarting}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),
    timer:sleep(RestartTimeout),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),

    % plan: wait
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertMatch([{Id, undefined, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, undefined}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual({ok, []}, director:get_pids(?DIRECTOR)),

    % plan: delete
    ?assertMatch({ok, _Pid}, director:restart_child(?DIRECTOR,  Id)),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, [{Id, _Pid}]}, director:get_pids(?DIRECTOR)),

    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 0,0,0,0),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, []}, director:get_pids(?DIRECTOR)).


'4'(_Config) ->
    Id = foo,
    Plan = fun(_, killed, _, St) -> {stop, St} end,
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE
                          ,start_link
                          ,[{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, undefined, [ChildSpec]} end,

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    Pid = erlang:whereis(?DIRECTOR),
    Pid2 = erlang:whereis(?CHILD),
    erlang:exit(Pid2, kill),
    ?assertMatch(pass
                ,receive
                     {'EXIT', Pid, killed} ->
                         pass
                 end).


'5'(_Config) ->
    Reason = oops,
    Id = foo,
    Plan = fun(_, _, _, St) -> {{stop, Reason}, St} end,
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE
                          ,start_link
                          ,[{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, undefined, [ChildSpec]} end,

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    Pid = erlang:whereis(?DIRECTOR),
    Pid2 = erlang:whereis(?CHILD),
    erlang:exit(Pid2, Reason),
    ?assertMatch(pass
                ,receive
                     {'EXIT', Pid, Reason} ->
                         pass
                 end).


'6'(_Config) ->
    Start = {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]},
    Mods = [?CHILD_MODULE],
    Plan = fun(_, _ ,1, St) -> {restart, St} end,
    DefChildSpec = #{start => Start
                   ,plan => Plan
                   ,terminate_timeout => 1000
                   ,modules => Mods},
    F = fun() -> {ok, undefined, [], DefChildSpec} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual(DefChildSpec, director:get_default_childspec(?DIRECTOR)),
    Id = foo,
    Plan2 = fun(_, _, _, St) -> {stop, St} end,
    ChildSpec = #{id => Id, append => true, plan => Plan2, modules => []},
    ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR,  ChildSpec)),
    {ok, ChildSpec2} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start, maps:get(start, ChildSpec2)),
    ?assertEqual(Plan, maps:get(plan, ChildSpec2)),
    ?assertEqual(2000, maps:get(terminate_timeout, ChildSpec2)),
    ?assertEqual(Mods, maps:get(modules, ChildSpec2)),

    Start2 = {?CHILD_MODULE, start_link, [fun() -> {ok, undefined} end]},
    DefChildSpec2 = #{start => Start2
                     ,plan => Plan
                     ,modules => [?CHILD_MODULE]},
    ?assertEqual(ok, director:change_default_childspec(?DIRECTOR, DefChildSpec2)),
    {ok, ChildSpec3} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start2, maps:get(start, ChildSpec3)),
    ?assertEqual(Plan, maps:get(plan, ChildSpec2)),
    ?assertEqual(1000, maps:get(terminate_timeout, ChildSpec3)),
    ?assertEqual([?CHILD_MODULE], maps:get(modules, ChildSpec3)).


'7'(_config) ->
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}},
    ChildSpec2 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}
                 ,plan => fun director:plan/4
                 ,terminate_timeout => 1000
                 ,modules => [?CHILD_MODULE]
                 ,append => false
                 ,type => worker
                 ,log_validator => fun director:log_validator/4
                 ,state => undefined
                 ,delete => true},
    F2 = fun() -> {ok, undefined, [ChildSpec]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F2
                                                ,?START_OPTIONS)),
    {ok, ChildSpec3} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(ChildSpec2, ChildSpec3),

    Id2 = bar,
    ChildSpec4 = #{id => Id2
                 ,start => {?CHILD_MODULE, start_link, [F]}
                 ,type => supervisor},
    ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR, ChildSpec4)),
    {ok, ChildSpec5} = director:get_childspec(?DIRECTOR, Id2),
    ?assertEqual(infinity, maps:get(terminate_timeout, ChildSpec5)).


'8'(Config) ->
    Src = "-module(director_callback2).\n"
          "-export([init/1\n"
          "        ,terminate/2]).\n\n"
          "init(_InitArg) ->\n"
          "    {ok\n"
          "    ,undefined\n"
          "    ,~s}.\n\n\n"
          "terminate(_, _) ->\n"
          "    ok.\n",

    Callback = director_callback2,
    Id1 = foo,
    ChildMod1 = director_child,
    Children1 = io_lib:format("[#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}]"
                             ,[Id1, ChildMod1]),
    Src1 = io_lib:format(Src, [Children1]),
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
    ChildMod2 = director_child,
    Children2 = io_lib:format("[#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}\n"
                              ",#{id => ~s\n"
                              "  ,start => {~s, start_link, [fun() -> {ok, undefined} end]}}]"
                             ,[Id1, ChildMod1, Id2, ChildMod2]),
    Src2 = io_lib:format(Src, [Children2]),
    ct:pal("Source 2:\n~s\n", [Src2]),

    ?assertEqual(ok, sys:suspend(?DIRECTOR)),

    ?assertEqual(ok, file:write_file(File, Src2)),
    ?assertEqual({ok, Callback}, compile:file(File, [return_errors, {outdir, Dir}])),
    ?assertEqual({module, Callback}, c:l(Callback)),

    ?assertEqual(ok, sys:change_code(?DIRECTOR, Callback, undefined, undefined)),
    ?assertEqual(ok, sys:resume(?DIRECTOR)),
    ct:pal("Childrens:\n~p\n", [director:which_children(?DIRECTOR)]),
    ?assertMatch({ok, _}, director:restart_child(?DIRECTOR, Id2)),
    director:stop(?DIRECTOR).


'9'(_Config) ->
    TabMod = director_table_mnesia,
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec1 = #{id => Id
                  ,start => {?CHILD_MODULE, start_link, [F]}},
    F2 = fun() -> {ok, undefined, [ChildSpec1]} end,
    Res = director:start_link(?CALLBACK, F2, [{table_module, TabMod}|?START_OPTIONS]),
    ?assertMatch({ok, _Pid}, Res),
    {ok, Pid} = Res,

    Res2 = director:start_link(?CALLBACK, F2, [{table_module, TabMod}|?START_OPTIONS]),
    ?assertMatch({ok, _Pid}, Res2),
    {ok, Pid2} = Res2,

    ?assertMatch({ok, _Pid}, director:get_pid(Pid, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(Pid2, Id)),
    ?assertEqual(ok, director:terminate_and_delete_child(Pid, Id)),
    ?assertEqual({error, not_found}, director:get_pid(Pid2, Id)).


'10'(_Config) ->
    TabMod = director_table_mnesia,
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec1 = #{id => Id
                  ,start => {?CHILD_MODULE, start_link, [F]}
                  ,delete => false},
    Id2 = bar,
    ChildSpec2 = #{id => Id2
                  ,start => {?CHILD_MODULE, start_link, [F]}
                  ,delete => true},
    F2 = fun() -> {ok, undefined, [ChildSpec1, ChildSpec2]} end,
    Res = director:start_link(?CALLBACK, F2, [{delete_table, false}
                                             ,{table_module, TabMod}|?START_OPTIONS]),
    ?assertMatch({ok, _Pid}, Res),
    {ok, Pid} = Res,

    Res2 = director:start_link(?CALLBACK, F2, [{table_module, TabMod}|?START_OPTIONS]),
    ?assertMatch({ok, _Pid}, Res2),
    {ok, Pid2} = Res2,

    ?assertMatch({ok, _Pid}, director:get_pid(Pid, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(Pid2, Id)),
    ?assertEqual(ok, director:stop(Pid)),
    ?assertEqual({error, undefined}, director:get_pid(Pid2, Id)),
    ?assertEqual({error, not_found}, director:get_pid(Pid2, Id2)),

    Res3 = director:start_link(?CALLBACK
                              ,fun() -> {ok, undefined} end
                              ,[{delete_table, false}
                               ,{table_module, TabMod}|?START_OPTIONS]),
    ?assertMatch({ok, _Pid}, Res3),
    {ok, Pid3} = Res3,
    ?assertEqual({error, undefined}, director:get_pid(Pid3, Id)),
    ?assertEqual({error, not_found}, director:get_pid(Pid3, Id2)),

    ?assertMatch({ok, _Pid}, director:restart_child(Pid3, Id)),
    Res4 = director:get_pid(Pid2, Id),
    ?assertMatch({ok, _Pid}, Res4),
    ?assert(erlang:is_pid(erlang:element(2, Res4))).


'11'(_Config) ->
    F = fun() -> {ok, undefined} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?CALLBACK = supervisor:get_callback_module(?DIRECTOR).


'12'(_Cfg) ->
    F = fun() -> {ok, undefined} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    {ok, ChPid} = ?CHILD_MODULE:start_link(F),
    Id = foo,
    ChildSpec1 = #{id => Id
                  ,start => {?CHILD_MODULE, start_link, [F]}
                  ,plan => fun(_, _, _, St) -> {restart, St} end},
    ?assertEqual(ok, director:become_supervisor(?DIRECTOR, ChildSpec1, ChPid)),
    ?assertEqual({ok, ChPid}, director:get_pid(?DIRECTOR, Id)),
    erlang:exit(ChPid, kill),
    timer:sleep(15),
    ?assertMatch({ok, _}, director:get_pid(?DIRECTOR, Id)),

    ?assertEqual({error, noproc}, director:become_supervisor(?DIRECTOR, ChildSpec1, ChPid)),
    {ok, ChPid2} = director:get_pid(?DIRECTOR, Id),
    ?assertEqual({error, {already_started, ChPid2}}
                ,director:become_supervisor(?DIRECTOR, ChildSpec1, ChPid2)),
    ?assertEqual(ok, director:terminate_child(?DIRECTOR, Id)),
    {ok, ChPid3} = ?CHILD_MODULE:start_link(F),
    ?assertEqual({error, {already_present, Id}}
                ,director:become_supervisor(?DIRECTOR, ChildSpec1, ChPid3)),
    ?assertEqual({error, {duplicate_child_name, Id}}
                ,director:become_supervisor(?DIRECTOR, ChildSpec1#{start => ?CHILD_MODULE}, ChPid3)),
    ok.


'13'(_Cfg) ->
    F = fun() -> {ok, undefined} end,
    Id = foo,
    ChildSpec1 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [F]}
                 ,plan => fun(_, _, _, St) -> {restart, St} end},
    F2 = fun() -> {ok, undefined, [ChildSpec1]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F2
                                                ,?START_OPTIONS)),
    {ok, ChPid} = director:get_pid(?DIRECTOR, Id),
    erlang:exit(ChPid, kill),
    timer:sleep(15),
    {ok, ChPid2} = director:get_pid(?DIRECTOR, Id),
    ?assert(ChPid /= ChPid2),
    ?assertEqual(ok, director:delete_running_child(?DIRECTOR, ChPid2)),
    erlang:exit(ChPid, kill),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)).


'14'(_Cfg) ->
    F = fun() -> {ok, undefined} end,
    Id = foo,
    ChildSpec1 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [F]}
                 ,plan => fun(_, _, _, St) -> {restart, St} end},
    F2 = fun() -> {ok, undefined, [ChildSpec1]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F2
                                                ,?START_OPTIONS)),
    {ok, ChPid} = director:get_pid(?DIRECTOR, Id),
    erlang:exit(ChPid, kill),
    timer:sleep(15),
    {ok, ChPid2} = director:get_pid(?DIRECTOR, Id),
    ?assert(ChPid /= ChPid2),
    ?assertEqual(ok, director:delete_running_child(?DIRECTOR, ChPid2)),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch({ok, _Pid}, director:start_link({local, director_2}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual(ok, director:become_supervisor(director_2, ChildSpec1, ChPid2)),
    ?assertEqual({ok, ChPid2}, director:get_pid(director_2, Id)),
    erlang:exit(ChPid2, kill),
    timer:sleep(15),
    ?assertMatch({ok, _}, director:get_pid(director_2, Id)).


%% -------------------------------------------------------------------------------------------------
%% Internal functions:

count_children(Director, Specs, Actives, Workers, Sups) ->
    CountChildren = director:count_children(Director),

    ?assertEqual(Specs, ?config(specs, CountChildren)),
    ?assertEqual(Actives, ?config(active, CountChildren)),
    ?assertEqual(Workers, ?config(workers, CountChildren)),
    ?assertEqual(Sups, ?config(supervisors, CountChildren)).


log_validator(_, _, _, _) ->
    long.