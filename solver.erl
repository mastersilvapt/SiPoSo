-module(solver).
-export([handler/1]).

handler({sum,{XS,YS}}) -> clean(sum(XS,YS));
handler({sub,{XS,YS}}) -> clean(sub(XS,YS));
handler({mult,{XS,YS}}) -> clean(mult(XS,YS));
handler(_) -> ok.


filter(t,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> (V == Vf) and (E == Ef) end,XS);
filter(f,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> not ((V == Vf) and (E == Ef)) end,XS).

clean(XS) -> lists:filter(fun({V,C,E}) -> C /= 0 end, XS).

normalize([]) -> [];
normalize([{V,C,E}|XS]) -> [{V, lists:foldr(fun({_,C1,_},Acc) -> C1+Acc end, C, filter(t,V,E,XS)), E} | normalize(filter(f,V,E,XS))].

sum(XS,YS) -> normalize(lists:append(XS,YS)).

sub(XS,YS) -> sum(XS,[{V,-C,E} || {V,C,E} <- YS ]).

nub([]) -> [];
nub([X|XS]) -> [X | nub(lists:filter(fun(Xi) -> Xi /= X end, XS))].

calculateVars(V1,V2) -> nub(lists:append(V1,V2)).

calculateExp(J1,J2) -> lists:map(fun({_,S}) -> S end, lists:merge(lists:sort(J1), lists:sort(J2))).

mult(XS,YS) -> normalize([{calculateVars(V1,V2), C1*C2, calculateExp(lists:zip(V1,E1),lists:zip(V2,E2))} || {V1,C1,E1} <- XS, {V2,C2,E2} <- YS]).
