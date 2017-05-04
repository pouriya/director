-module(director_callback).
-export([start_link/0
        ,init/1]).




start_link() ->
    director:start({local, director}, ?MODULE, undefined, [{debug, [trace]}]).




init(undefined) ->
    {ok, []};
init(Arg) ->
    {ok, [], Arg}.
