-module(polysupervisor).
-behaviour(supervisor).
 
-export([start_link/1]).
-export([init/1]).
 
start_link(Type) -> supervisor:start_link({local,?MODULE}, ?MODULE, Type).

init(lenient) -> init({one_for_one, 3, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
            [{workforme,
                {polyworker, start_link, [add, [{[x],1,[1]}], [{[x],1,[1]}]]},
                 transient, 1000, worker, [polyworker]}
    ]}}.
