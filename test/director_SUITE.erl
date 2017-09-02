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
%% @version  17.9.2
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
        ,'10'/1]).




%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:


-define(DIRECTOR, director_name).
-define(CHILD, child_name).
-define(CALLBACK, director_callback).
-define(CHILD_MODULE, director_child).
-define(START_OPTIONS, [{debug, [trace]}
                       ,{log_validator, fun log_validator/2}
                       ,{table_type, list}]).


-include("internal/director_defaults.hrl").
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
    erlang:process_flag(trap_exit, true),
    Config.





end_per_testcase(_TestCase, _Config) ->
        ok.





%% -------------------------------------------------------------------------------------------------
%%





'1'(_Config) ->
    ?assertEqual({error, foo}, director:start_link(?CALLBACK, fun()-> {stop, foo} end)),
    ?assertEqual(ignore, director:start_link(?CALLBACK, fun() -> ignore end)),
    ?assertMatch({error, {init_bad_return, [{returned_value, bar}|_]}}
                ,director:start_link(?CALLBACK, fun()-> bar end)),
    ?assertEqual({error, baz}, director:start_link(?CALLBACK, fun()-> erlang:exit(baz) end)),

    ?assertEqual(director:start_link({local, ?DIRECTOR}, ?CALLBACK, fun() -> {ok, []} end)
                ,{ok, erlang:whereis(?DIRECTOR)}),
    ?assertEqual(ok, director:stop(?DIRECTOR)),
    ?assertEqual(undefined, erlang:whereis(?DIRECTOR)).





'2'(_Config) ->
    Id = foo,
    Plan = [restart],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,append => false
                ,type => worker},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, [ChildSpec]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual({ok, ChildSpec}, director:get_childspec(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid, worker, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,1,0),
    ?assertEqual({ok, Plan}, director:get_plan(?DIRECTOR, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),
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
    ?assertEqual([], director:get_pids(?DIRECTOR)),

    ?assertMatch({ok, _Pid}, director:restart_child(?DIRECTOR, Id)),
    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),
    ?assertMatch([{Id, _Pid, worker, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,1,0),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    ?assertEqual(ok, director:terminate_child(?DIRECTOR, erlang:whereis(?CHILD))),
    ?assertEqual(ok, director:delete_child(?DIRECTOR, Id)),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 0,0,0,0),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)).







'3'(_Config) ->
    Id = foo,
    RestartTimeout = 100,
    Plan = [restart, {restart, RestartTimeout}, wait, delete],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => infinity
                ,modules => Mods
                ,append => false
                ,type => supervisor},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, []} end,

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
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),
    ?assertEqual({error, running}, director:delete_child(?DIRECTOR, Id)),
    ?assert(erlang:is_process_alive(erlang:whereis(?CHILD))),

    % plan: restart
    erlang:exit(erlang:whereis(?CHILD), kill),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    % plan: {restart, Timeout}
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertMatch([{Id, restarting, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, restarting}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)),
    timer:sleep(RestartTimeout),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    % plan: wait
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertMatch([{Id, undefined, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, undefined}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)),

    % plan: delete
    ?assertMatch({ok, _Pid}, director:restart_child(?DIRECTOR,  Id)),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 0,0,0,0),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([], director:get_pids(?DIRECTOR)).






'4'(_Config) ->
    Id = foo,
    Plan = [stop],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, [ChildSpec]} end,

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
    Plan = [{stop, Reason}],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, [ChildSpec]} end,

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
    RestartTimeout = 100,
    Id = foo,
    Fun =
        fun
            (foo, killed, 1) ->
                restart;
            (foo, killed, 2) ->
                {restart, RestartTimeout};
            (foo, killed, 3) ->
                wait;
            (foo, killed, 4) ->
                delete
        end,
    RestartTimeout = 100,
    Plan = [Fun],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => Plan
                ,count => 4
                ,terminate_timeout => infinity
                ,modules => Mods
                ,append => false
                ,type => supervisor},
    ?assertEqual(ok, director:check_childspec(ChildSpec)),
    F = fun() -> {ok, [ChildSpec]} end,

    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual({ok, ChildSpec}, director:get_childspec(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertEqual({ok, Plan}, director:get_plan(?DIRECTOR, Id)),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),
    ?assertEqual({error, running}, director:delete_child(?DIRECTOR, Id)),

    % plan: restart
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    % plan: {restart, Timeout}
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertMatch([{Id, restarting, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, restarting}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)),
    timer:sleep(RestartTimeout),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    % plan: wait
    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertEqual([{Id, undefined, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,0,0,1),
    ?assertEqual({error, undefined}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)),

    % plan: delete
    ?assertMatch({ok, _Pid}, director:restart_child(?DIRECTOR,  Id)),
    ?assertMatch([{Id, _Pid, supervisor, Mods}], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 1,1,0,1),
    ?assertMatch({ok, _Pid}, director:get_pid(?DIRECTOR, Id)),
    ?assertMatch([{Id, _Pid}], director:get_pids(?DIRECTOR)),

    erlang:exit(erlang:whereis(?CHILD), kill),
    timer:sleep(1),
    ?assertEqual([], director:which_children(?DIRECTOR)),
    count_children(?DIRECTOR, 0,0,0,0),
    ?assertEqual({error, not_found}, director:get_pid(?DIRECTOR, Id)),
    ?assertEqual([], director:get_pids(?DIRECTOR)).





'7'(_Config) ->
    Start = {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]},
    Mods = [?CHILD_MODULE],
    DefChildSpec = #{start => Start
                   ,plan => [restart]
                   ,count => 1
                   ,terminate_timeout => 1000
                   ,modules => Mods},
    F = fun() -> {ok, [], DefChildSpec} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    ?assertEqual(DefChildSpec, director:get_default_childspec(?DIRECTOR)),
    Id = foo,
    ChildSpec = #{id => Id, append => true, plan => [{restart, 1000}], modules => []},
    ?assertMatch({ok, _Pid}, director:start_child(?DIRECTOR,  ChildSpec)),
    {ok, ChildSpec2} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start, maps:get(start, ChildSpec2)),
    ?assertEqual([restart, {restart, 1000}], maps:get(plan, ChildSpec2)),
    ?assertEqual(?DEF_COUNT + 1, maps:get(count, ChildSpec2)),
    ?assertEqual(2000, maps:get(terminate_timeout, ChildSpec2)),
    ?assertEqual(Mods, maps:get(modules, ChildSpec2)),

    Start2 = {?CHILD_MODULE, start_link, [fun() -> {ok, undefined} end]},
    DefChildSpec2 = #{start => Start2
                   ,plan => [restart, wait]
                   ,modules => [?CHILD_MODULE]},
    ?assertEqual(ok, director:change_default_childspec(?DIRECTOR, DefChildSpec2)),
    {ok, ChildSpec3} = director:get_childspec(?DIRECTOR, Id),
    ?assertEqual(Start2, maps:get(start, ChildSpec3)),
    ?assertEqual([restart, wait, {restart, 1000}], maps:get(plan, ChildSpec3)),
    ?assertEqual(?DEF_COUNT, maps:get(count, ChildSpec3)),
    ?assertEqual(1000, maps:get(terminate_timeout, ChildSpec3)),
    ?assertEqual([?CHILD_MODULE], maps:get(modules, ChildSpec3)).








'8'(_config) ->
    ChildSpec = #{id => foo
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, fun() -> {ok, undefined} end]}
                ,plan => []
                ,count => 0
                ,terminate_timeout => 0
                ,modules => [?CHILD_MODULE]
                ,default_arguments => []
                ,type => worker},
    F = fun() -> {ok, [ChildSpec]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F
                                                ,?START_OPTIONS)),
    Pid = erlang:whereis(?DIRECTOR),
    erlang:exit(erlang:whereis(?CHILD), kill),
    ?assertEqual(pass
                ,receive
                     {'EXIT'
                     ,Pid
                     ,{empty_plan_child_terminated
                      ,[{reason, killed}|_]}} ->
                         pass
                 end),

    ChildSpec2 = ChildSpec#{plan => [restart]},
    F2 = fun() -> {ok, [ChildSpec2]} end,
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F2
                                                ,?START_OPTIONS)),
    Pid2 = erlang:whereis(?DIRECTOR),
    erlang:exit(erlang:whereis(?CHILD), kill),
    ?assertEqual(pass
                ,receive
                     {'EXIT'
                         ,Pid2
                         ,{reached_max_restart_plan
                          ,[{reason, killed}|_]}} ->
                         pass
                 end).








'9'(_config) ->
    Id = foo,
    F = fun() -> {ok, undefined} end,
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}},
    ChildSpec2 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}
                 ,plan => [fun director:plan_element_fun/3]
                 ,count => ?DEF_COUNT
                 ,terminate_timeout => 1000
                 ,modules => [?CHILD_MODULE]
                 ,append => false
                 ,type => worker},
    F2 = fun() -> {ok, [ChildSpec]} end,
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





'10'(Config) ->
    F = F = fun() -> {ok, undefined} end,
    Id = foo,
    ChildSpec = #{id => Id, start => {?CHILD_MODULE, start_link, [{local, ?CHILD}, F]}},
    F1 = fun() -> io:format("TEST~n"), {ok, [ChildSpec]} end, %% will see this twice in ct log
    ?assertMatch({ok, _Pid}, director:start_link({local, ?DIRECTOR}
                                                ,?CALLBACK
                                                ,F1
                                                ,?START_OPTIONS)),
    ?assertMatch([{foo, _}], director:get_pids(?DIRECTOR)),
    {_, _Pid} = director:get_pid(?DIRECTOR, Id),
    DataDir = ?config(data_dir, Config),
    ModFile = filename:join([DataDir, "src", erlang:atom_to_list(?CALLBACK) ++ ".erl"]),
    {ok, Src} = file:read_file(ModFile),
    {ok, Src2} =  file:read_file(filename:join([DataDir, "src", erlang:atom_to_list(?CALLBACK) ++ "2"])),
    ?assertEqual(ok, file:write_file(ModFile, Src2)),
    ?assertEqual({ok, ?CALLBACK}, compile:file(ModFile)),
    ?assertEqual(ok, sys:suspend(?DIRECTOR)),
    ?assertEqual(ok, sys:change_code(erlang:whereis(?DIRECTOR), ?CALLBACK, undefined, undefined)),
    ?assertEqual(ok, sys:resume(?DIRECTOR)),
    ?assertEqual(ok, file:write_file(ModFile, Src)).





%% -------------------------------------------------------------------------------------------------
%% Internal functions:





count_children(Director, Specs, Actives, Workers, Sups) ->
    CountChildren = director:count_children(Director),

    ?assertEqual(Specs, ?config(specs, CountChildren)),
    ?assertEqual(Actives, ?config(active, CountChildren)),
    ?assertEqual(Workers, ?config(workers, CountChildren)),
    ?assertEqual(Sups, ?config(supervisors, CountChildren)).





log_validator(_, _) ->
    long.
