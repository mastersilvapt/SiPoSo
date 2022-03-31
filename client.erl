-module(client).
-export([sum/4,sub/4,mult/4,fact/3,is_prime/3, list/0]).

rpc(Server, T, Args) ->
    global:send(Server, {Args, self(), T}),
    receive
        {ok, Res} -> Res
    end.

sum(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{sum,{XS,YS}})]).

sub(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{sub,{XS,YS}})]).

mult(XS,YS,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{mult,{XS,YS}})]).

fact(X,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{fact,{X}})]).

is_prime(X,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{is_prime,{X}})]).

fib(X,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{fib,{X}})]).

maclaurinSeries(Start,End,X,T,Server) -> io:format("The result is ~p~n",[rpc(Server,T,{macSeries,{Start,End,X}})]).

list() -> 
    net_adm:ping('manager@20.126.76.228'),
    timer:sleep(1000),
    global:send(siposomanager, {who, self()}),
    receive
        {ok, L} -> L
    end.
    

