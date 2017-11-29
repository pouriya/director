-module(sample_sf_worker).

-export([start_link/1
        ,init/2
        ,loop/2]).

-record(state, {id, start}).


start_link(Id) ->
    proc_lib:start_link(?MODULE, init, [erlang:self(), Id]).


init(Parent, Id) ->
    proc_lib:init_ack(Parent, {ok, erlang:self()}),
    loop(Parent, #state{id = Id}).


loop(Parent, #state{id = Id}=State) ->
    receive
        {change_parent, From, NewParent} ->
            ok = director:delete_running_child(Parent, erlang:self()),
            ok = director:become_supervisor(NewParent
                                           ,#{id => Id, start => {?MODULE, start_link, [Id]}}
                                           ,erlang:self()),
            From ! ok,
            loop(NewParent, State);
        print_parent ->
            io:format("~p: parent == ~p~n", [erlang:self(), Parent]),
            loop(Parent, State)
    end.