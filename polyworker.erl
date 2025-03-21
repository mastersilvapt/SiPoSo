-module(polyworker).
-behaviour(gen_server).

-export([start_link/1, stop/1, loop/1, mid_point/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

% res is deprecated, res no longer part of server record
-record(state, {name, res}).
-define(DELAY, 750).

start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [Name], []).

stop(Name) -> gen_server:call(Name, stop).

init([Name]) ->
    %% To know when the parent shuts down
    process_flag(trap_exit, true),
    io:format("Server ~s started ~n", [Name]),
    {ok, #state{name = Name}, ?DELAY}.  % give some delay before accepting to make sure everything is ok

handle_info(timeout, S = #state{name = N}) ->
    io:format("~s accepting requests!~n", [N]),
    polyworker:loop(S),
    {noreply, S};
handle_info(_Message, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
    {noreply, S, ?DELAY}.

handle_call(stop, _From, S = #state{}) ->
    {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
    {noreply, S, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Everything gets trapped here.
%% - Normal is to signal a normal exit
%% - Shutdown is when the supervisor is issuing a shutdown so the process will terminate
%%    (when the supervisor gets a valid exit aimed at him) valid exit are set to be any reason
%% - Any other reason is abnormal and the process MAY terminate (supervisor will restart it)
%% - kill will 100% sure terminate the process, but the supervisor will restart it
terminate(normal, S) ->
    io:format("~s is normally leaving", [S#state.name]);
terminate(shutdown, S) ->
    io:format(
        "Monitor is telling to shutdown node ~s!~n",
        [S#state.name]
    );
terminate(_Reason, S) ->
    io:format("Server ~s is leaving abnormally, ~p", [S#state.name, _Reason]).

% creates worker process and monitors it for results
restarter(Args, K) when K < 10 ->
    Pid = spawn_link(?MODULE, mid_point, [Args]),
    receive
        {'EXIT', Pid, {normal, Res}} ->
            io:format("Exited normally. ~n"),
            {ok, Res};
        {'EXIT', Pid, shutdown} ->
            ok;
        {'EXIT', Pid, _} ->
            restarter(Args, K + 1)
    end;
restarter(_, _) ->
    io:format("Unsuccessful computation! ~n"),
    {ok, error}.

% wait for requests
loop(S) ->
    receive
        {Args, From, T} ->
            timer:sleep(T),
            Res = restarter(Args, 0),
            io:format("Got, ~p ~n", [Res]),
            From ! Res,
            polyworker:loop(S)
        % after one match above try to match the following messages in the mailbox
    after 0 ->
        polyworker:loop(S)
    end.

mid_point(Args) ->
    X = polymath:handler(Args),
    exit({normal, X}).
