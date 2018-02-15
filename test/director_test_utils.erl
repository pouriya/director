%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jan 2018 6:21 PM
%%%-------------------------------------------------------------------
-module(director_test_utils).
-author("pouriya").

%% API
-export([make_return/4
        ,handle_return/3
        ,flush_handle_returns/0]).



make_return(Pid, Mod, Func, Args) ->
    Ref = erlang:make_ref(),
    Pid ! {handle_return, erlang:self(), Ref, Mod, Func, Args},
    receive
        {Ref, Val} ->
            if
                erlang:is_function(Val) ->
                    Val();
                true ->
                    Val
            end
    end.


handle_return(Mod, Func, Filter) ->
    receive
        {handle_return, Pid, Ref, Mod, Func, Args} ->
            Pid ! {Ref, Filter(Args)}
    after 1000 ->
        flush_handle_returns(),
        c:flush(),
        exit({handle_return, Mod, Func})
    end.


flush_handle_returns() ->
    receive
        {handle_return, Pid, _, Mod, Func, Args} ->
            ct:pal("Handle return from ~tp for ~tp:~tp/~tp with arguments ~tp~n", [Pid, Mod, Func, erlang:length(Args), Args]),
            flush_handle_returns()
    after 0 ->
        ok
    end.