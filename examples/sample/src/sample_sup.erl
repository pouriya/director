-module(sample_sup).

-behaviour(director).

%% API
-export([start_link/0]).

-record(state, {}).

%% Director callbacks
-export([init/1
        ,terminate/2]).

start_link() ->
    director:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Child = #{id => 1
             ,start => sample_child %% will be {sample_child, start_link, []}
             },
    {ok, #state{}, [Child]}.


terminate(_Rsn, #state{}) ->
    ok.