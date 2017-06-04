-module(director_child).
-export([start_link/1
        ,start_link/2
        ,init/1
        ,terminate/2]).






start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).


start_link(Name, Arg) ->
    gen_server:start_link(Name, ?MODULE, Arg, []).



init(Arg) ->
    Arg().








terminate(_Reason, _State) ->
    ok.