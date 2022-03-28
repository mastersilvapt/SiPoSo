-module(polySolver).
-export([handler/1,checkIntegrity/1]).
%-compile(export_all).

checkIntegrity([]) -> true;
checkIntegrity([{V,C,E}|XS]) -> if (is_integer(C) and (length(V) == length(E))) -> checkIntegrity(XS); true -> false end;
checkIntegrity(_) -> false.

handler({sum,{XS,YS}}) -> clean(sum(handler(XS),handler(YS)));
handler({sub,{XS,YS}}) -> clean(sub(handler(XS),handler(YS)));
handler({mult,{XS,YS}}) -> clean(mult(handler(XS),handler(YS)));
handler(XS) -> A = checkIntegrity(XS), if A -> XS; true -> {error,integrity} end.

filter(t,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> (V == Vf) and (E == Ef) end,XS);
filter(f,V,E,XS) -> lists:filter(fun({Vf,_,Ef}) -> not ((V == Vf) and (E == Ef)) end,XS).

clean(XS) -> [{cleanGetVars(lists:zip(V,E)), C, cleanGetExp(lists:zip(V,E))} || {V,C,E} <- XS, C /= 0].

cleanGetVars(VE) -> fst(lists:filter(fun({_,E}) -> E /= 0 end, VE)).

cleanGetExp(VE) -> snd(lists:filter(fun({_,E}) -> E /= 0 end, VE)).

normalize([]) -> [];
normalize([{V,C,E}|XS]) -> [{V, lists:foldr(fun({_,C1,_},Acc) -> C1+Acc end, C, filter(t,V,E,XS)), E} | normalize(filter(f,V,E,XS))].

sum(XS,YS) -> normalize(lists:append(XS,YS)).

sub(XS,YS) -> sum(XS,[{V,-C,E} || {V,C,E} <- YS]).

fst(XS) -> lists:map(fun({F,_}) -> F end,XS).
snd(XS) -> lists:map(fun({_,S}) -> S end,XS).

merge(L1,[]) -> L1;
merge([],L2) -> L2;
merge([{V1,E1}|LS1],[{V2,E2}|LS2]) when V1 < V2 -> [{V1,E1} | merge(LS1, [{V2,E2} | LS2])];
merge([{V1,E1}|LS1],[{V2,E2}|LS2]) when V1 > V2 -> [{V2,E2} | merge([{V1,E1} | LS1], LS2)];
merge([{V,E1}|LS1],[{V,E2}|LS2]) -> [{V,E1+E2} | merge(LS1,LS2)].

calculateVars(J1,J2) -> fst(merge(lists:sort(J1), lists:sort(J2))).
%calculateVars(J1,J2) -> fst(lists:merge(fun() -> end, lists:sort(J1), lists:sort(J2))).

calculateExp(J1,J2) -> snd(merge(lists:sort(J1), lists:sort(J2))).
%calculateExp(J1,J2) -> snd(lists:merge(fun({V1,E1},{V2,E2}) -> if (V1 < V2) -> {V1,E1}; true -> if (V1 > V2) -> {V2,E2}; true -> {V1,E1+E2} end end, lists:sort(J1), lists:sort(J2))).

mult(XS,YS) -> normalize([{calculateVars(lists:zip(V1,E1),lists:zip(V2,E2)), C1*C2, calculateExp(lists:zip(V1,E1),lists:zip(V2,E2))} || {V1,C1,E1} <- XS, {V2,C2,E2} <- YS]).
