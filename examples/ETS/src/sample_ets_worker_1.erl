-module(sample_ets_worker_1).

-export([start_link/0]).
-export([init/0]).


start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    error_logger:info_msg("I'm done.~n", []),
    exit(normal).