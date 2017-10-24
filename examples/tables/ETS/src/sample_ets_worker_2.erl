-module(sample_ets_worker_2).


-export([start_link/0]).
-export([init/0]).


start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    RandomInt = element(3, os:timestamp()),
    Rsn =
        if
            RandomInt rem 2 =:= 0 ->
                error_logger:info_msg("I'm done.~n", []),
                normal;
            true ->
                oops
        end,
    exit(Rsn).