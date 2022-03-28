-module(manager).
-export([get_available_nodes/1,names/0]).

% -import(manager,[get_available_nodes/1]). to make only this function visible

% lists global names of available servers
get_available_nodes(T) ->
    % L = supervisor:which_children({global, polysupervisor}),
    L = [{server2,"<9229.102.0>",worker,[polyworker]},{server1,"<9229.101.0>",worker,[polyworker]}],
    lists:foldl(fun(_, I) -> T = [lists:nth(I,names())], I - 1 end, length(L), L),
    io:format("~p ~n", T).

% tells supervisor to start another node

% tells supervisor to shutdown a node

% tells supervisor to stop

names() -> 
    [   
        "super_alpha",
        "titan_iota",
        "super_gamma",
        "attack_delta",
        "colossal_pi"
    ].