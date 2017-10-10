-module(sample_child).


-export([start_link/0]).
-export([init/1
        ,terminate/2]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).


init(undefined) ->
    {ok, #state{}}.


terminate(_Rsn, #state{}) ->
    ok.