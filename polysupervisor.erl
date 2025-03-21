-module(polysupervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Type) ->
    supervisor:start_link({global,?MODULE}, ?MODULE, Type).

init(lenient) ->
    init({one_for_one, 3, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
         [{server5,
           {polyworker, start_link, [super_alpha]},
           permanent, 2000, worker, [polyworker]},
           {server4,
           {polyworker, start_link, [titan_iota]},
           permanent, 2000, worker, [polyworker]}
         ]}}.