-module(client).
-export([do/2]).

rpc(Server, Content) ->
    Ref = make_ref(),
    Server ! {self(), Ref, Content},
    receive
        {response, Ref, Replay} -> Replay
    end.

do(sum,Args) -> io:format("Sum is ~p~n", [rpc(list,{sum,Args})]);
do(sub,Args) -> io:format("Sum is ~p~n", [rpc(list,{sub,Args})]);
do(mult,Args) -> io:format("Sum is ~p~n", [rpc(list,{mult,Args})]);
do(status,_) -> io:format("Status: ~p~n",[rpc(list,{status})]);
do(_,_) -> ok.
