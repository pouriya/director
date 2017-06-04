-module(director_callback).
-export([start_link/1
        ,start_link/2
        ,init/1]).



start_link(InitArg) ->
    director:start(?MODULE, InitArg, [{debug, [trace]}]).


start_link(Name, InitArg) ->
    director:start(Name, ?MODULE, InitArg, [{debug, [trace]}]).




init(InitArg) ->
    InitArg().
