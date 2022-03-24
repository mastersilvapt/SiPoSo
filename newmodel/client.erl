-module(client).

-export([do_sum/3]).

do_sum(XS, YS, T) -> 
    global:send(super_alpha, {sum, XS, YS, self(), T}),
    receive
        {ok, Res} -> io:format("~p ~n", [Res])
    end.
