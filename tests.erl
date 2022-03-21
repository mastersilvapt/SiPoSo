-module(tests).
-export([normalize/1]).

getSum([]) -> 0;
getSum([{_,C,_}|XS]) -> C + getSum(XS).


normalize([]) -> [];
normalize([{V,C,E}|XS]) -> [{V,C + getSum(lists:filter(fun({Vf,_,Ef}) -> (V == Vf) and (E == Ef) end,XS)),E}] ++ normalize(lists:filter(fun({Vf,_,Ef}) -> (V /= Vf) or (E /= Ef) end,XS)).
