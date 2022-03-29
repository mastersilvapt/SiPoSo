-module(polymanager).
-export([get_available_nodes/0, names/0]).

% lists global names of available servers
get_available_nodes() ->
    L = supervisor:which_children({global, polysupervisor}),
    {T, _} = lists:split(length(L), lists:reverse(names())),
    T.

% tells supervisor to start another node
start_another(LocalName, RestartType, ShutdownTime) ->
    % temos de derivar o nome e.g mega_eta do LocalName
    supervisor:start_child(
        {global, polysupervisor},
        {LocalName, {polyworker, start_link, [mega_eta]}, RestartType, ShutdownTime, worker, [
            polyworker
        ]}
    ).

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
