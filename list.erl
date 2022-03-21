-module(list).
-export([start/0]).

start() -> register(list,spawn(fun() -> loop(0),liveness() end)).

liveness() ->
    ok.

loop(N) ->
    receive
        {register, Name, Pid} -> put(Name, Pid), loop(N+1);
        {unregister, Name} -> loop(N-1)
    end.
