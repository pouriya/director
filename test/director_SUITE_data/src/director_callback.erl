-module(director_callback).
-export([init/1
        ,terminate/2
        ,code_change/3]).




init(InitArg) ->
    InitArg().



terminate(_, _) ->
    ok.



code_change(_, State, _) ->
    {ok, State}.