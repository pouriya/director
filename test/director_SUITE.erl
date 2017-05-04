%% ---------------------------------------------------------------------


-module(director_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
-vsn("17.02.28").


%% ---------------------------------------------------------------------
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
        ,'11'/1]).




%% ---------------------------------------------------------------------
%% Records & Macros & Includes:


-define(NAME, test).
-define(REGISTER_NAME, {local, ?NAME}).
-define(CALLBACK, director_callback).
-define(INIT_ARGUMENT, undefined).
-define(START_OPTIONS, [{debug, [trace]}]).
-define(start_link, director:start_link({local, test}
                                       ,?CALLBACK
                                       ,?INIT_ARGUMENT
                                       ,?START_OPTIONS)).

-define(start_child(ChildSpec), director:start_child(?NAME, ChildSpec)).

-define(restart_child(Id), director:restart_child(?NAME, Id)).

-define(terminate_child(IdOrPid), director:terminate_child(?NAME, IdOrPid)).

-define(delete_child(Id), director:delete_child(?NAME, Id)).

-define(count_children, director:count_children(?NAME)).

-define(which_children, director:which_children(?NAME)).

-define(get_childspec(Id), director:get_childspec(?NAME, Id)).

-define(get_pid(Id), director:get_pid(?NAME, Id)).

-define(get_pids, director:get_pids(?NAME)).

-define(get_plan(Id), director:get_plan(?NAME, Id)).

-define(change_plan(Id, Plan), director:change_plan(?NAME, Id, Plan)).

-define(get_default_childspec, director:get_default_childspec(?NAME)).

-define(change_default_childspec(Childspec), director:change_default_childspec(?NAME, Childspec)).

-define(stop, director:stop(?NAME)).

-define(CHILD_MODULE, director_child).

-define(start_link2(InitArg), director:start_link({local, test}
                                                 ,?CALLBACK
                                                 ,InitArg
                                                 ,?START_OPTIONS)).

-include_lib("common_test/include/ct.hrl").





%% ---------------------------------------------------------------------
%% ct callbacks:





all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, 11)].




init_per_suite(Config) ->
    application:start(sasl),
    Config.



end_per_suite(Config) ->
    application:stop(sasl),
    Config.





init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = ?start_link,
    erlang:process_flag(trap_exit, true),
    start_profiling(Pid),
    Config.





end_per_testcase(_TestCase, _Config) ->
    catch ?stop,
    stop_profiling().





%% ---------------------------------------------------------------------
%%





'1'(_Config) ->
    [] = ?which_children,
    CountChildren = ?count_children,
    {ok, #{plan := []
         ,count := 0
         ,terminate_timeout := 0
         ,modules := []}} = ?get_default_childspec,
    0 = ?config(specs, CountChildren),
    0 = ?config(active, CountChildren),
    0 = ?config(workers, CountChildren),
    0 = ?config(supervisors, CountChildren),
    [] = ?get_pids.





'2'(_Config) ->
    Id = foo,
    Plan = [restart],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,append => false
                ,type => worker},
    ok = director:check_childspec(ChildSpec),
    {ok, Pid} = ?start_child(ChildSpec),
    {ok, ChildSpec} = ?get_childspec(Id),
    [{Id, Pid, worker, Mods}] = ?which_children,
    CountChildren = ?count_children,
    1 = ?config(specs, CountChildren),
    1 = ?config(active, CountChildren),
    1 = ?config(workers, CountChildren),
    0 = ?config(supervisors, CountChildren),
    {ok, Plan} = ?get_plan(Id),
    {ok, Pid} = ?get_pid(Id),
    [{Id, Pid}] = ?get_pids,
    {error, running} = ?delete_child(Id),
    true = erlang:is_process_alive(Pid),

    [begin
         {ok, PidX} = ?get_pid(Id),
         erlang:exit(PidX, kill),
         timer:sleep(10)
     end || _ <- lists:seq(1, 10)],
    {ok, Pid2} = ?get_pid(Id),
    ok = ?terminate_child(Id),
    false = erlang:is_process_alive(Pid2),
    [{Id, undefined, worker, Mods}] = ?which_children,
    CountChildren2 = ?count_children,
    1 = ?config(specs, CountChildren2),
    0 = ?config(active, CountChildren2),
    1 = ?config(workers, CountChildren2),
    0 = ?config(supervisors, CountChildren2),
    {error, undefined} = ?get_pid(Id),
    [] = ?get_pids,

    {ok, Pid3} = ?restart_child(Id),
    true = erlang:is_process_alive(Pid3),
    [{Id, Pid3, worker, Mods}] = ?which_children,
    CountChildren3 = ?count_children,
    1 = ?config(specs, CountChildren3),
    1 = ?config(active, CountChildren3),
    1 = ?config(workers, CountChildren3),
    0 = ?config(supervisors, CountChildren3),
    {ok, Pid3} = ?get_pid(Id),
    [{Id, Pid3}] = ?get_pids,

    ok = ?terminate_child(Pid3),
    ok = ?delete_child(Id),
    false = erlang:is_process_alive(Pid3),
    [] = ?which_children,
    CountChildren4 = ?count_children,
    0 = ?config(specs, CountChildren4),
    0 = ?config(active, CountChildren4),
    0 = ?config(workers, CountChildren4),
    0 = ?config(supervisors, CountChildren4),
    {error, not_found} = ?get_pid(Id),
    [] = ?get_pids.







'3'(_Config) ->
    Id = foo,
    RestartTimeout = 100,
    Plan = [restart, {restart, RestartTimeout}, wait, delete],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => infinity
                ,modules => Mods
                ,append => false
                ,type => supervisor},
    ok = director:check_childspec(ChildSpec),
    {ok, Pid} = ?start_child(ChildSpec),
    {ok, ChildSpec} = ?get_childspec(Id),
    [{Id, Pid, supervisor, Mods}] = ?which_children,
    CountChildren = ?count_children,
    1 = ?config(specs, CountChildren),
    1 = ?config(active, CountChildren),
    0 = ?config(workers, CountChildren),
    1 = ?config(supervisors, CountChildren),
    {ok, Plan} = ?get_plan(Id),
    {ok, Pid} = ?get_pid(Id),
    [{Id, Pid}] = ?get_pids,
    {error, running} = ?delete_child(Id),

    erlang:exit(Pid, kill),
    false = erlang:is_process_alive(Pid),
    [{Id, Pid2, supervisor, Mods}] = ?which_children,
    CountChildren2 = ?count_children,
    1 = ?config(specs, CountChildren2),
    1 = ?config(active, CountChildren2),
    0 = ?config(workers, CountChildren2),
    1 = ?config(supervisors, CountChildren2),
    {ok, Pid2} = ?get_pid(Id),
    [{Id, Pid2}] = ?get_pids,

    erlang:exit(Pid2, kill),
    false = erlang:is_process_alive(Pid2),
    [{Id, restarting, supervisor, Mods}] = ?which_children,
    CountChildren3 = ?count_children,
    1 = ?config(specs, CountChildren3),
    0 = ?config(active, CountChildren3),
    0 = ?config(workers, CountChildren3),
    1 = ?config(supervisors, CountChildren3),
    {error, restarting} = ?get_pid(Id),
    [] = ?get_pids,
    timer:sleep(RestartTimeout * 2),
    [{Id, Pid3, supervisor, Mods}] = ?which_children,
    CountChildren4 = ?count_children,
    1 = ?config(specs, CountChildren4),
    1 = ?config(active, CountChildren4),
    0 = ?config(workers, CountChildren4),
    1 = ?config(supervisors, CountChildren4),
    {ok, Pid3} = ?get_pid(Id),
    [{Id, Pid3}] = ?get_pids,

    erlang:exit(Pid3, kill),
    false = erlang:is_process_alive(Pid3),
    [{Id, undefined, supervisor, Mods}] = ?which_children,
    CountChildren5 = ?count_children,
    1 = ?config(specs, CountChildren5),
    0 = ?config(active, CountChildren5),
    0 = ?config(workers, CountChildren5),
    1 = ?config(supervisors, CountChildren5),
    {error, undefined} = ?get_pid(Id),
    [] = ?get_pids,

    {ok, Pid4} = ?restart_child(Id),
    true = erlang:is_process_alive(Pid4),
    [{Id, Pid4, supervisor, Mods}] = ?which_children,
    CountChildren6 = ?count_children,
    1 = ?config(specs, CountChildren6),
    1 = ?config(active, CountChildren6),
    0 = ?config(workers, CountChildren6),
    1 = ?config(supervisors, CountChildren6),
    {ok, Pid4} = ?get_pid(Id),
    [{Id, Pid4}] = ?get_pids,

    erlang:exit(Pid4, kill),
    false = erlang:is_process_alive(Pid4),
    [] = ?which_children,
    CountChildren7 = ?count_children,
    0 = ?config(specs, CountChildren7),
    0 = ?config(active, CountChildren7),
    0 = ?config(workers, CountChildren7),
    0 = ?config(supervisors, CountChildren7),
    {error, not_found} = ?get_pid(Id),
    [] = ?get_pids.






'4'(_Config) ->
    Id = foo,
    Plan = [stop],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    {ok, Pid} = ?start_child(ChildSpec),
    Pid2 = erlang:whereis(?NAME),
    erlang:exit(Pid, kill),
    pass =
        receive
            {'EXIT', Pid2, {stop
                           ,[{child, _Child}
                            ,{child_last_error_reason, killed}]}} ->
                pass
        end.






'5'(_Config) ->
    Reason = oops,
    Id = foo,
    Plan = [{stop, Reason}],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => Plan
                ,count => infinity
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    {ok, Pid} = ?start_child(ChildSpec),
    Pid2 = erlang:whereis(?NAME),
    erlang:exit(Pid, kill),
    pass =
        receive
            {'EXIT', Pid2, Reason} ->
                pass
        end.






'6'(_Config) ->
    RestartTimeout = 100,
    Fun =
        fun
            (killed, 1) ->
                restart;
            (killed, 2) ->
                {restart, RestartTimeout};
            (killed, 3) ->
                wait;
            (killed, 4) ->
                delete
        end,
    Id = foo,
    RestartTimeout = 100,
    Plan = [Fun],
    Mods = [?CHILD_MODULE],
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => Plan
                ,count => 4
                ,terminate_timeout => infinity
                ,modules => Mods
                ,append => false
                ,type => supervisor},
    ok = director:check_childspec(ChildSpec),
    {ok, Pid} = ?start_child(ChildSpec),
    {ok, ChildSpec} = ?get_childspec(Id),
    [{Id, Pid, supervisor, Mods}] = ?which_children,
    CountChildren = ?count_children,
    1 = ?config(specs, CountChildren),
    1 = ?config(active, CountChildren),
    0 = ?config(workers, CountChildren),
    1 = ?config(supervisors, CountChildren),
    {ok, Plan} = ?get_plan(Id),
    {ok, Pid} = ?get_pid(Id),
    [{Id, Pid}] = ?get_pids,
    {error, running} = ?delete_child(Id),

    erlang:exit(Pid, kill),
    false = erlang:is_process_alive(Pid),
    [{Id, Pid2, supervisor, Mods}] = ?which_children,
    CountChildren2 = ?count_children,
    1 = ?config(specs, CountChildren2),
    1 = ?config(active, CountChildren2),
    0 = ?config(workers, CountChildren2),
    1 = ?config(supervisors, CountChildren2),
    {ok, Pid2} = ?get_pid(Id),
    [{Id, Pid2}] = ?get_pids,

    erlang:exit(Pid2, kill),
    false = erlang:is_process_alive(Pid2),
    [{Id, restarting, supervisor, Mods}] = ?which_children,
    CountChildren3 = ?count_children,
    1 = ?config(specs, CountChildren3),
    0 = ?config(active, CountChildren3),
    0 = ?config(workers, CountChildren3),
    1 = ?config(supervisors, CountChildren3),
    {error, restarting} = ?get_pid(Id),
    [] = ?get_pids,
    timer:sleep(RestartTimeout * 2),
    [{Id, Pid3, supervisor, Mods}] = ?which_children,
    CountChildren4 = ?count_children,
    1 = ?config(specs, CountChildren4),
    1 = ?config(active, CountChildren4),
    0 = ?config(workers, CountChildren4),
    1 = ?config(supervisors, CountChildren4),
    {ok, Pid3} = ?get_pid(Id),
    [{Id, Pid3}] = ?get_pids,

    erlang:exit(Pid3, kill),
    false = erlang:is_process_alive(Pid3),
    [{Id, undefined, supervisor, Mods}] = ?which_children,
    CountChildren5 = ?count_children,
    1 = ?config(specs, CountChildren5),
    0 = ?config(active, CountChildren5),
    0 = ?config(workers, CountChildren5),
    1 = ?config(supervisors, CountChildren5),
    {error, undefined} = ?get_pid(Id),
    [] = ?get_pids,

    {ok, Pid4} = ?restart_child(Id),
    true = erlang:is_process_alive(Pid4),
    [{Id, Pid4, supervisor, Mods}] = ?which_children,
    CountChildren6 = ?count_children,
    1 = ?config(specs, CountChildren6),
    1 = ?config(active, CountChildren6),
    0 = ?config(workers, CountChildren6),
    1 = ?config(supervisors, CountChildren6),
    {ok, Pid4} = ?get_pid(Id),
    [{Id, Pid4}] = ?get_pids,

    erlang:exit(Pid4, kill),
    false = erlang:is_process_alive(Pid4),
    [] = ?which_children,
    CountChildren7 = ?count_children,
    0 = ?config(specs, CountChildren7),
    0 = ?config(active, CountChildren7),
    0 = ?config(workers, CountChildren7),
    0 = ?config(supervisors, CountChildren7),
    {error, not_found} = ?get_pid(Id),
    [] = ?get_pids.





'7'(_Config) ->
    ok = ?stop,
    Start = {?CHILD_MODULE, start_link, [1]},
    Mods = [?CHILD_MODULE],
    DefChildSpec = #{start => Start
                   ,plan => [restart]
                   ,count => 5
                   ,terminate_timeout => 1000
                   ,modules => Mods},

    {ok, _Pid} = ?start_link2(DefChildSpec),
    {ok, DefChildSpec} = ?get_default_childspec,
    Id = foo,
    ChildSpec = #{id => Id, append => true, plan => [{restart, 1000}], modules => []},
    {ok, _Pid2} = ?start_child(ChildSpec),
    {ok, ChildSpec2} = ?get_childspec(Id),
    Start = maps:get(start, ChildSpec2),
    [restart, {restart, 1000}] = maps:get(plan, ChildSpec2),
    6 = maps:get(count, ChildSpec2),
    2000 = maps:get(terminate_timeout, ChildSpec2),
    Mods = maps:get(modules, ChildSpec2),

    Start2 = {?CHILD_MODULE, start_link, []},
    DefChildSpec2 = #{start => Start2
                   ,plan => [restart, wait]
                   ,count => 10
                   ,terminate_timeout => 2000
                   ,modules => []},
    ok = ?change_default_childspec(DefChildSpec2),
    {ok, ChildSpec3} = ?get_childspec(Id),
    Start2 = maps:get(start, ChildSpec3),
    [restart, wait, {restart, 1000}] = maps:get(plan, ChildSpec3),
    11 = maps:get(count, ChildSpec3),
    3000 = maps:get(terminate_timeout, ChildSpec3),
    [] = maps:get(modules, ChildSpec3).








'8'(_config) ->
    ChildSpec = #{id => foo
                ,start => {?CHILD_MODULE, start_link, [1]}
                ,plan => []
                ,count => 0
                ,terminate_timeout => 0
                ,modules => [?CHILD_MODULE]
                ,default_arguments => []
                ,type => worker},
    {ok, Pid} = ?start_child(ChildSpec),
    Pid2 = erlang:whereis(?NAME),
    erlang:exit(Pid, kill),
    pass =
        receive
            {'EXIT'
            ,Pid2
            ,{reached_max_restart_plan
             ,[{child, _Child}, {child_last_error_reason, killed}]}} ->
                pass
        end.








'9'(_config) ->
    Id = foo,
    ChildSpec = #{id => Id
                ,start => {?CHILD_MODULE, start_link, [1]}},
    ChildSpec2 = #{id => Id
                 ,start => {?CHILD_MODULE, start_link, [1]}
                 ,plan => []
                 ,count => 1
                 ,terminate_timeout => 1000
                 ,modules => [?CHILD_MODULE]
                 ,append => false
                 ,type => worker},
    {ok, _Pid} = ?start_child(ChildSpec),
    {ok, ChildSpec3} = ?get_childspec(Id),
    ChildSpec4 = ChildSpec2#{plan => maps:get(plan, ChildSpec3)},
    ChildSpec4 = ChildSpec3,

    Id2 = bar,
    ChildSpec5 = #{id => Id2
                 ,start => {?CHILD_MODULE, start_link, [1]}
                 ,type => supervisor},
    {ok, _Pid2} = ?start_child(ChildSpec5),
    {ok, ChildSpec6} = ?get_childspec(Id2),
    #{terminate_timeout := infinity} = ChildSpec6.








'10'(Config) when erlang:is_list(Config) ->
    Id = foo,
    Mods = [?MODULE],
    RestartTimeout = 10,
    ChildSpec = #{id => Id
                ,start => {?MODULE, '10', [{restart, RestartTimeout}]}
                ,plan => []
                ,count => 0
                ,terminate_timeout => 0
                ,modules => Mods
                ,default_arguments => []
                ,type => worker},
    {ok, restarting} = ?start_child(ChildSpec),
    [{Id, restarting, worker, Mods}] = ?which_children,
    CountChildren = ?count_children,
    1 = ?config(specs, CountChildren),
    0 = ?config(active, CountChildren),
    1 = ?config(workers, CountChildren),
    0 = ?config(supervisors, CountChildren),
    {error, restarting} = ?get_pid(Id),
    [] = ?get_pids,

    timer:sleep(RestartTimeout),
    ok = ?delete_child(Id),
    [] = ?which_children,
    CountChildren2 = ?count_children,
    0 = ?config(specs, CountChildren2),
    0 = ?config(active, CountChildren2),
    0 = ?config(workers, CountChildren2),
    0 = ?config(supervisors, CountChildren2),
    {error, not_found} = ?get_pid(Id);
'10'(Arg) ->
    Arg.







'11'(_Config) ->
    State = sys:get_state(?NAME),
    State = sys:replace_state(?NAME, fun(State2) -> State2 end),
    State = sys:get_state(?NAME),

    ?CALLBACK = supervisor:get_callback_module(?NAME),
    ok = sys:suspend(?NAME),
    ok = sys:change_code(?NAME, ?CALLBACK, old, extra),
    ok = sys:resume(?NAME).





start_profiling(Pid) ->
    eprof:start(),
    eprof:start_profiling([Pid]).




stop_profiling() ->
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().
