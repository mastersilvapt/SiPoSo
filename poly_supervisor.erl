-module(poly_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Type) ->
    supervisor:start_link({global,?MODULE}, ?MODULE, Type).

init(lenient) ->
    init({one_for_one, 3, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
         [{server1,
           {poly_worker, start_link, [super_alpha]},
           permanent, 2000, worker, [poly_worker]},
           {server2,
           {poly_worker, start_link, [titan_iota]},
           permanent, 2000, worker, [poly_worker]}
         ]}}.

% start another server
% poly_supervisor:start_child({global,poly_supervisor},{server3,{poly_worker, start_link, [mega_eta]},permanent, 2000, worker, [poly_worker]}).
