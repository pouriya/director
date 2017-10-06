-module(director_table_list_SUITE).
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
        ,'6'/1]).




%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:


-define(TAB_MOD, 'director_table_list').
-define(TAB_INIT_ARG, 'undefined').
-define(TESTER_MODULE, 'director_table_').




%% -------------------------------------------------------------------------------------------------
%% ct callbacks:





all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
        || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-7)].




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
    ?TESTER_MODULE:'1'(?TAB_MOD, ?TAB_INIT_ARG).


'2'(_Config) ->
    ?TESTER_MODULE:'2'(?TAB_MOD, ?TAB_INIT_ARG).


'3'(_Config) ->
    ?TESTER_MODULE:'3'(?TAB_MOD, ?TAB_INIT_ARG).


'4'(_Config) ->
    ?TESTER_MODULE:'4'(?TAB_MOD, ?TAB_INIT_ARG).


'5'(_Config) ->
    ?TESTER_MODULE:'5'(?TAB_MOD, ?TAB_INIT_ARG).


'6'(_Config) ->
    ?TESTER_MODULE:'6'(?TAB_MOD, ?TAB_INIT_ARG).