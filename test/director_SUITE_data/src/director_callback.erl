-module(director_callback).
-export([init/1
        ,terminate/2]).




init(InitArg) ->
    InitArg().



terminate(_, _) ->
    ok.