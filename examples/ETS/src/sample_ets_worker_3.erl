-module(sample_ets_worker_3).
%%-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
        ,handle_info/2
        ,terminate/2]).

-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, undefined, []).



init(undefined) ->
    {ok, #state{}}.





handle_info(Job, State) ->
    error_logger:info_msg("Doing job.~n", []),
    Job(),
    {noreply, State}.


terminate(_Rsn, _State) ->
    ok.