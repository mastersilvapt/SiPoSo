-module(client).
-export([sum/4,sub/4,mult/4,fact/4,is_prime/4, list/0]).

rpc(Server, T, Op, XS, YS) ->
    global:send(Server, {Op, XS, YS, self(), T}),
    receive
        {ok, Res} -> Res
    end.

sum(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,sum,XS,YS)]).

sub(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,sub,XS,YS)]).

mult(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,mult,XS,YS)]).

fact(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,fact,XS,YS)]).

is_prime(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,prime,XS,YS)]).

list() -> 
    net_adm:ping('manager@20.126.76.228'),
    global:send(siposomanager, {who, self()}),
    receive
        {ok, L} -> L
    end.
    

