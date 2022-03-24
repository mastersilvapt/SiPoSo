-module(mathfunc).

-compile([export_all]).

sum(XS, YS) -> normalize(lists:append(XS, YS)).

sub(XS, YS) -> sum(XS, negate(YS)).

mult(XS, YS) ->
    normalize([
        {calculateVars(V1, V2), C1 * C2, calculateExp(V1, V2, E1, E2)}
     || {V1, C1, E1} <- XS, {V2, C2, E2} <- YS
    ]).

normalize([]) ->
    [];
normalize([{V, C, E} | XS]) ->
    [{V, C + getSum(filter(t, V, E, XS)), E}] ++ normalize(filter(f, V, E, XS)).

getSum([]) -> 0;
getSum([{_, C, _} | XS]) -> C + getSum(XS).

filter(t, V, E, XS) -> lists:filter(fun({Vf, _, Ef}) -> (V == Vf) and (E == Ef) end, XS);
filter(f, V, E, XS) -> lists:filter(fun({Vf, _, Ef}) -> (V /= Vf) or (E /= Ef) end, XS).

% clean(XS) -> [{V,C,E} || {V,C,E} <- XS, C /= 0].

negate([]) -> [];
negate([{V, C, E} | XS]) -> [{V, -C, E}] ++ negate(XS).

recNub([], _) -> [];
recNub([R | XS], R) -> recNub(XS, R);
recNub([X | XS], R) -> [X] ++ recNub(XS, R).

nub([]) -> [];
nub([X | XS]) -> [X] ++ nub(recNub(XS, X)).

calculateVars(V1, V2) -> nub(lists:append(V1, V2)).

join(L1, []) -> L1;
join([], L2) -> L2;
join([L1 | LS1], [L2 | LS2]) when L1 < L2 -> [L1] ++ join(LS1, [L2] ++ LS2);
join([L1 | LS1], [L2 | LS2]) when L1 > L2 -> [L2] ++ join([L1] ++ LS1, LS2);
join([{V, E1} | LS1], [{V, E2} | LS2]) -> [{V, E1 + E2}] ++ join(LS1, LS2).

second([]) -> [];
second([{_, S} | XS]) -> [S] ++ second(XS).

calculateExp(V1, V2, E1, E2) ->
    second(
        join(
            lists:sort(fun({V10, _}, {V11, _}) -> V10 < V11 end, lists:zip(V1, E1)),
            lists:sort(fun({V20, _}, {V21, _}) -> V20 < V21 end, lists:zip(V2, E2))
        )
    ).