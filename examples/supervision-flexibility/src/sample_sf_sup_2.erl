-module(sample_sf_sup_2).
-behaviour(director).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1
        ,terminate/2]).


start_link() ->
    director:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, undefined, []}.


terminate(_Rsn, _State) ->
    ok.