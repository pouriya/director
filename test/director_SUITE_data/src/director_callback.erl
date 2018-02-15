-module(director_callback).
-export([init/1
        ,handle_start/4
        ,handle_terminate/5
        ,handle_exit/5
        ,terminate/2]).




init(InitArg) ->
    director_test_utils:make_return(InitArg, ?MODULE, init, [InitArg]).

handle_start(Id, ChState, State, MetaData) ->
    director_test_utils:make_return(State, ?MODULE, handle_start, [Id, ChState, State, MetaData]).


handle_exit(Id, ChState, Rsn, State, MetaData) ->
    director_test_utils:make_return(State, ?MODULE, handle_exit, [Id, ChState, Rsn, State, MetaData]).

handle_terminate(Id, ChState, Rsn, State, MetaData) ->
    director_test_utils:make_return(State, ?MODULE, handle_terminate, [Id, ChState, Rsn, State, MetaData]).

terminate(Rsn, State) ->
    director_test_utils:make_return(State, ?MODULE, terminate, [Rsn, State]).