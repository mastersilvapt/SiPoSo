-module(polymanager).
-export([names/0, start/0, init/0, stop_child/1, restart_child/1, delete_child/1, stop_supervisor/0, start_another/3]).

start() -> spawn(?MODULE, init, []).

init() ->
    global:register_name(siposomanager, self()),
    loop().

loop() ->
    receive
        {who, From} ->
            io:format("query ~n"),
            From ! {ok, get_available_nodes()},
            loop()
    end.

% lists global names of available servers
get_available_nodes() ->
    L = supervisor:which_children({global, polysupervisor}),
    {T, _} = lists:split(length(L), lists:reverse(names())),
    T.

% tells supervisor to start another node
start_another(N, RestartType, ShutdownTime) ->
    if N >= 1 andalso N =< 5 ->
        supervisor:start_child(
            {global, polysupervisor},
            {list_to_atom("server"++integer_to_list(N)), {polyworker, start_link, [list_to_atom(lists:nth(N, names()))]}, RestartType, ShutdownTime, worker, [
                polyworker
            ]}
        );
    true ->
        io:format("First parameter must be within 1 and 5 ~n")
    end.


% tells supervisor to stop a child
stop_child(ChildName) ->
    supervisor:terminate_child({global, polysupervisor}, ChildName).

% tells supervisor to delete a child
delete_child(ChildName) ->
    supervisor:delete_child({global, polysupervisor}, ChildName).

% tells supervisor to delete a child
restart_child(ChildName) ->
    supervisor:restart_child({global, polysupervisor}, ChildName).

% tells supervisor to stop
stop_supervisor() ->
    exit(global:whereis_name(polysupervisor), shutdown).

names() ->
    [
        "super_gamma",
        "attack_delta",
        "colossal_pi",
        "titan_iota",
        "super_alpha"
    ].

% start node
% erl -name manager@20.126.76.228 -setcookie supermegapolycookie -kernel inet_dist_listen_min 41161 inet_dist_listen_max 41161