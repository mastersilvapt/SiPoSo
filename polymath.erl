-module(polymath).
-export([handler/1]).
%-compile(export_all).

% Verify if the polynomial structure is correct.
check_integrity([]) -> true;
check_integrity([{V,C,E}|XS]) -> if (is_integer(C) and (length(V) == length(E))) -> check_integrity(XS); true -> false end;
check_integrity(_) -> false.

% Entry point to math functions, calls the clean funtion after perform each operaction and is recursive (not needed)
handler({sum,{XS,YS}}) -> clean(sum(handler(XS),handler(YS)));
handler({sub,{XS,YS}}) -> clean(sub(handler(XS),handler(YS)));
handler({mult,{XS,YS}}) -> clean(mult(handler(XS),handler(YS)));
handler(XS) -> A = check_integrity(XS), if A -> XS; true -> {error,integrity} end.

% Get the vars and the functions and remove the var if expoent equals to 0 and clears the term if coefficient is 0
clean(XS) -> [{clean_get_vars(lists:zip(V,E)), C, clean_get_exp(lists:zip(V,E))} || {V,C,E} <- XS, C /= 0].

clean_get_vars(VE) -> fst(lists:filter(fun({_,E}) -> E /= 0 end, VE)).

clean_get_exp(VE) -> snd(lists:filter(fun({_,E}) -> E /= 0 end, VE)).

% Main function, takes the polynomial and simplifies every thing that is possivel (adding them if possible)
normalize([]) -> [];
normalize(XS) -> [{V,C,E} || {{V,E},C} <- maps:to_list(lists:foldr(fun({V,C,E},Map) -> maps:update_with({V,E},fun(Val) -> C + Val end, C, Map) end, maps:new(), XS))].

% Just need to append both lists and the normalize will do the rest.
sum(XS,YS) -> normalize(lists:append(XS,YS)).

% Negate the second member by negate each coefficient and sum them.
sub(XS,YS) -> sum(XS,[{V,-C,E} || {V,C,E} <- YS]).

% Auxiliar functions to get a list of every first or second elem of a list of tuples.
fst(XS) -> lists:map(fun({F,_}) -> F end,XS).
snd(XS) -> lists:map(fun({_,S}) -> S end,XS).

% Merge can be replaced with lists:merge with a lambda function, just adds 2 lists in order and sum the expoent is equals, used on multiplication, output a ordered list
merge(L1,[]) -> L1;
merge([],L2) -> L2;
merge([{V1,E1}|LS1],[{V2,E2}|LS2]) when V1 < V2 -> [{V1,E1} | merge(LS1, [{V2,E2} | LS2])];
merge([{V1,E1}|LS1],[{V2,E2}|LS2]) when V1 > V2 -> [{V2,E2} | merge([{V1,E1} | LS1], LS2)];
merge([{V,E1}|LS1],[{V,E2}|LS2]) -> [{V,E1+E2} | merge(LS1,LS2)].

% Auxiliar function in order to not bloat the code
calculate_vars(J1,J2) -> fst(merge(lists:sort(J1), lists:sort(J2))).

calculate_exp(J1,J2) -> snd(merge(lists:sort(J1), lists:sort(J2))).

% Using comprehension lists we can multiply each member of the first one for the second, after that we only need to normalize, each term vars and expoents are calculated with the merge function
mult(XS,YS) -> normalize([{calculate_vars(lists:zip(V1,E1),lists:zip(V2,E2)), C1*C2, calculate_exp(lists:zip(V1,E1),lists:zip(V2,E2))} || {V1,C1,E1} <- XS, {V2,C2,E2} <- YS]).
