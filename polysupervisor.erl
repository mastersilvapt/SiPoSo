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

% start another server
% polysupervisor:start_child({global,polysupervisor},{server3,{polyworker, start_link, [mega_eta]},permanent, 2000, worker, [polyworker]}).

% start node
% erl -name supervisor@20.104.85.133 -setcookie supermegapolycookie -kernel inet_dist_listen_min 41161 inet_dist_listen_max 41161