-module(sample_sf_sup).
-behaviour(director).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1
        ,terminate/2]).


start_link() ->
    director:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, undefined, [#{id => sup_1, start => sample_sf_sup_1, type => sup}  % sup = supervisor
                    ,#{id => sup_2, start => sample_sf_sup_2, type => s}]}. % s = supervisor

terminate(_Rsn, _State) ->
    ok.