-module(director_utils_SUITE).

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
        || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-8)].


init_per_suite(Config) ->
    application:start(sasl),
    Config.


end_per_suite(Config) ->
    application:stop(sasl),
    Config.


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, _) ->
    ok.


'1'(_) ->
    ?assertEqual([1,2,3,4,5], director_utils:concat([1,2,3], [4,5])),
    ?assertEqual([1,2,3,4,5], director_utils:concat([1,2|3], [4|5])),
    ?assertEqual([1,2,3,4,5], director_utils:concat([1,2|3], [4,5])),
    ?assertEqual([1,2,3,4,5], director_utils:concat([1,2,3], [4|5])),
    ?assertEqual([4,5], director_utils:concat([], [4,5])),
    ?assertEqual([1,2,3], director_utils:concat([1,2,3], [])),
    ?assertEqual([], director_utils:concat([], [])).


'2'(_) ->
    ?assertEqual(default, director_utils:option(key, [], fun(_) -> filter end, default)),
    ?assertEqual(filter, director_utils:option(key, [{key, value}], fun(value) -> filter end, default)).


'3'(_) ->
    ?assertEqual(default, director_utils:value(key, [], default)),
    ?assertEqual(value, director_utils:value(key, [{key, value}], default)).


'4'(_) ->
    ?assertEqual({true, 1}, director_utils:has_duplicate([1,2,3,4,5,6,7,8,9,1])),
    ?assertEqual({true, 1}, director_utils:has_duplicate([1,2,3,4,5,1,6,7,8,9])),
    ?assertEqual(false, director_utils:has_duplicate([1,2,3,4,5,6,7,8,9])),
    ?assertEqual(false, director_utils:has_duplicate([])).


