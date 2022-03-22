-module(server).
-export([start/1, normalize/1, sum/2, sub/2, clean/1, nub/1, mult/2, calculateVars/2, calculateExp/4, join/2, second/1]).

% Start and register the server as server to other nodes access it.
start(Name) -> register(Name, spawn(fun() -> loop(0) end)), put(name,Name), list ! {register, Name, whereis(Name)}.

% Function that receives and redirect
loop(N) ->
    receive
        {From, Ref, {stop}} ->
            io:format("Server going offline.~nOrder by ~p with reference ~p~n.", [node(whereis(From)), Ref]),
            replay(From, Ref, stop()),
            list ! {unregister, get(name)};
        {From, Ref, {status}} -> replay(From, Ref, N),loop(N);
        {From, Ref, Content} -> replay(From, Ref, handler(Content)),loop(N+1)
    end.

% Stops the server and prints who did it.
stop() -> unregister(get(name)), ok.

%
replay(From, Ref, Response) ->
    From ! {response,Ref,Response}.


%normalize({VS,C,ES}) ->

handler({sum,{XS,YS}}) -> sum(XS,YS);
handler({sub,{XS,YS}}) -> sub(XS,YS);
handler({mult,{XS,YS}}) -> mult(XS,YS);
handler(_) -> ok.

%head([X|_]) -> X.

getSum([]) -> 0;
getSum([{_,C,_}|XS]) -> C + getSum(XS).

filter(t,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> (V == Vf) and (E == Ef) end,XS);
filter(f,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> (V /= Vf) or (E /= Ef) end,XS).

% clean(XS) -> [{V,C,E} || {V,C,E} <- XS, C /= 0].
clean(XS) -> lists:filter(fun({V,C,E}) -> C /= 0 end, XS).

normalize([]) -> [];
normalize([{V,C,E}|XS]) -> [{V, C+getSum(filter(t,V,E,XS)), E}] ++ normalize(filter(f,V,E,XS)).

sum(XS,YS) -> normalize(lists:append(XS,YS)).

negate([]) -> [];
negate([{V,C,E}|XS]) -> [{V,-C,E}] ++ negate(XS).

sub(XS,YS) -> sum(XS,negate(YS)).

%recNub([],_) -> [];
%recNub([R|XS],R) -> recNub(XS,R);
%recNub([X|XS],R) -> [X] ++ recNub(XS,R).

nub([]) -> [];
nub([X|XS]) -> [X] ++ nub(lists:filter(fun(Xi) -> Xi /= X end, XS)).
%nub([X|XS]) -> [X] ++ nub(recNub(XS,X)).

calculateVars(V1,V2) -> nub(lists:append(V1,V2)).

join(L1,[]) -> L1;
join([],L2) -> L2;
join([L1|LS1],[L2|LS2]) when L1 < L2 -> [L1] ++ join(LS1,[L2] ++ LS2);
join([L1|LS1],[L2|LS2]) when L1 > L2 -> [L2] ++ join([L1] ++ LS1,LS2);
join([{V,E1}|LS1],[{V,E2}|LS2]) -> [{V,E1+E2}] ++ join(LS1,LS2).

second([]) -> [];
second([{_,S}|XS]) -> [S] ++ second(XS).

calculateExp(V1,V2,E1,E2) -> second(join(lists:sort(fun({V10,_},{V11,_}) -> V10 < V11 end, lists:zip(V1,E1)), lists:sort(fun({V20,_},{V21,_}) -> V20 < V21 end, lists:zip(V2,E2)))).

mult(XS,YS) -> normalize([{calculateVars(V1,V2), C1*C2, calculateExp(V1,V2,E1,E2)} || {V1,C1,E1} <- XS, {V2,C2,E2} <- YS]).
%mult(XS,YS) -> normalize([ lists:append([ || {Vi1,Ei1} <- lists:sort(fun({V10,_},) -> end, lists:zip(V1,E1)), {Vi2,Ei2} <- lists:zip(V2,E2)]) || {V1,C1,E1} <- XS, {V2,C2,E2} <- YS]).
