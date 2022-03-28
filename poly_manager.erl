-module(poly_manager).
-export([get_available_nodes/0,names/0]).

% lists global names of available servers
get_available_nodes() ->
    L = poly_supervisor:which_children({global, polysupervisor}),
    {T,_} = lists:split(length(L),lists:reverse(names())),
    T.

% tells supervisor to start another node

% tells supervisor to shutdown a node

% tells supervisor to stop

names() -> 
    [   
        "super_gamma",
        "attack_delta",
        "colossal_pi",
        "titan_iota",
        "super_alpha"
    ].
