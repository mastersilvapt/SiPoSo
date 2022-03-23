
 -module(polyworker).
 -behaviour(gen_server).

 -export([start_link/3, stop/1]).
 -export([
     init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     code_change/3,
     terminate/2
 ]).

 -record(state, {name = "", operation, pol1, pol2}).
 -define(DELAY, 750).

 start_link(Operation, XS, YS) ->
     %probably need to change register name (operations) to a later global one
     gen_server:start_link({local, Operation}, ?MODULE, [Operation, XS, YS], []).

 stop(Operation) -> gen_server:call(Operation, stop).

 init([Operation, XS, YS]) ->
     %% To know when the parent shuts down
     process_flag(trap_exit, true),

     % can be later removed
     DelayForComputation = 3000,
     Name = pick_name(),
     io:format(
         "Node ~s, will started doing ~p in ~p ms! ~n",
         [Name, Operation, DelayForComputation]
     ),
     {ok, #state{name = Name, operation = Operation, pol1 = XS, pol2 = YS}, DelayForComputation}.

 handle_info(timeout, S = #state{name = N, operation = add, pol1 = P1, pol2 = P2}) ->
     io:format("Started sum on ~s!~n", [N]),
     timer:sleep(1000),
     case rand:uniform(5) of
        1 ->
             io:format("~s exploded. Uh oh~n",[N]),
             {stop, unlucky, S};
        _ ->
             io:format("Result is ~p~n",[sum(normalize(P1), normalize(P2))]),
             {noreply, S}
     end;
     %{noreply, S};
 handle_info(timeout, S = #state{name = N, operation = mult, pol1 = P1, pol2 = P2}) ->
     io:format("Started mult on ~s!~n", [N]),
     io:format("Result is ~p~n",[mult(normalize(P1), normalize(P2))]),
     %stop(self(),S),
     {noreply, S};
 handle_info(timeout, S = #state{name = N, operation = sub, pol1 = P1, pol2 = P2}) ->
     io:format("Started sub on ~s!~n", [N]),
     io:format("Result is ~p~n",[sub(normalize(P1), normalize(P2))]),
     %stop(self(),S, infinity),
     {noreply, S};
 % don't know what to do, the caller MAY crash, not my problem
 handle_info(_Message, S) ->
     {noreply, S, ?DELAY}.

 % normal terminate process
 handle_call(stop, _From, S = #state{}) ->
     {stop, normal, ok, S};
 handle_call(_Message, _From, S) ->
     {noreply, S, ?DELAY}.

 handle_cast(_Message, S) ->
     {noreply, S, ?DELAY}.

 code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

 terminate(normal, S) ->
     io:format("~s is normally leaving", [S#state.name]);
 terminate(shutdown, S) ->
     io:format(
         "Monitor is telling to shutdown ~s!~n",
         [S#state.name]
     );
 terminate(_Reason, S) ->
     io:format("~s is leaving, ~p", [S#state.name, _Reason]).

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

 % funny random names hehe
 pick_name() ->
     lists:nth(rand:uniform(10), firstnames()) ++
         " " ++
         lists:nth(rand:uniform(10), lastnames()).

 firstnames() ->
     [
         "Alpha",
         "Beta",
         "Gamma",
         "Delta",
         "Epsilon",
         "Zeta",
         "Eta",
         "Theta",
         "Iota",
         "Kappa"
     ].

 lastnames() ->
     [
         "Beast",
         "Pink",
         "Murasaki",
         "Isogashii",
         "Deep",
         "Super",
         "Counting",
         "Psi",
         "Chi",
         "Xi"
     ].
