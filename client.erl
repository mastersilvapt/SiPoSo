-module(client).
-export([status/1]).

rpc(Server, Content) ->
    Ref = make_ref(),
    Server ! {self(), Ref, Content},
    receive
        {response, Ref, Replay} -> Replay
    end.

status(Server) ->
    io:format("Status: ~p~n",[rpc(Server,{status})]).
