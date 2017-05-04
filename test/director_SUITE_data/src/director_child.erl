-module(director_child).
-export([start_link/1
        ,start_link/2
        ,init/1
        ,terminate/2]).






start_link(Arg) ->
    gen_server:start_link(?MODULE, [Arg], []).

start_link(Arg1, Arg2) ->
    gen_server:start_link(?MODULE, [Arg1, Arg2], []).




init({exit, Reason}) ->
    {stop, Reason};
init(Arg) ->
    {ok, Arg}.








terminate(_Reason, _State) ->
    ok.