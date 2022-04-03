-module(polytests).

-include_lib("eunit/include/eunit.hrl").

s(XS) -> lists:sort(XS).

% Should return error if the manager and supevisors are on
server_test() ->
    ?assertEqual(pong, net_adm:ping('manager@20.126.76.228')),
    timer:sleep(1000),
    A = polymanager:start_another(3,transient,2000),
    if A == ok -> ?assertEqual(3, length(supervisor:which_children({global,polysupervisor}))), polymanager:stop_child(server3), polymanager:delete_child(server3) end.

sum_test() ->
    ?assertEqual(s([{[x],2,[1]}]), s(polymath:handler({sum,{polyexamples:ex1(), polyexamples:ex1()}}))),
    ?assertEqual(s([{[x],1,[2]},{[z],1,[2]},{[x],6,[1]},{[x,y,z],1,[1,1,1]},{[],-5,[]}]), s(polymath:handler({sum,{polyexamples:ex3(), polyexamples:ex6()}}))),
    ?assertEqual(s([{[y,x,z],10,[4,2,1]},{[x],5,[2]},{[x,y],30,[1,2]},{[y],-100,[1]},{[x],2,[1]},{[],3,[]}]), s(polymath:handler({sum,{polyexamples:ex7(), polyexamples:ex2()}}))),
    ?assertEqual(s([{[z,y],1,[3,1]},{[x,y],2,[1,2]},{[x],1,[1]},{[],3,[]}]), s(polymath:handler({sum,{polyexamples:ex5(), polyexamples:ex1()}}))),
    ?assertEqual(s([{[x,y],125,[2,1]},{[z],1,[2]},{[x],-32,[1]},{[x,y,z],60,[1,3,1]},{[z,y],-30,[1,1]},{[x,z],-52,[1,2]}]), s(polymath:handler({sum,{polyexamples:ex9(), polyexamples:ex10()}}))),
    ?assertEqual(s([{[x,y],77,[2,1]},{[y,z],63,[1,2]},{[x],-40,[1]},{[],7,[]},{[x,y],2,[1,2]}]), s(polymath:handler({sum,{polyexamples:ex11(), polyexamples:ex4()}}))),
    ?assertEqual(s([{[y],32,[1]},{[x,z],2,[2,1]},{[z],1,[2]},{[y,x,z],10,[4,2,1]},{[x,y],30,[1,2]}]), s(polymath:handler({sum,{polyexamples:ex8(), polyexamples:ex7()}}))).

sub_test() ->
    ?assertEqual(s([{[],-3,[]},{[x],-5,[2]},{[x],-1,[1]}]),s(polymath:handler({sub,{polyexamples:ex1(),polyexamples:ex2()}}))),
    ?assertEqual(s([]), s(polymath:handler({sub,{polyexamples:ex2(),polyexamples:ex2()}}))),
    ?assertEqual(s([{[x,y],80,[2,1]}]),s(polymath:handler({sub,{polyexamples:ex11(),polyexamples:ex12()}}))).

mult_test() ->
    ?assertEqual(s([{[x],1,[2]}]),s(polymath:handler({mult,{polyexamples:ex1(),polyexamples:ex1()}}))),
    ?assertEqual(s([{[x],6,[2]},{[x],-2,[1]}]),s(polymath:handler({mult,{polyexamples:ex1(),polyexamples:ex3()}}))),
    ?assertEqual(s([{[x,y],-8460,[2,2]}, {[x,y],2250,[3,3]}, {[x,y],3200,[1,1]}, {[x,y,z],-320,[3,4,1]}, {[x,y,z],10,[2,4,3]}, {[x,y,z],30,[1,2,2]}, {[x,y,z],750,[4,5,1]}, {[y,z],-100,[1,2]}]),s(polymath:handler({mult,{polyexamples:ex9(),polyexamples:ex7()}}))).

rec_test() ->
    ?assertEqual(s([{[x],3,[1]}]),s(polymath:handler({sum,{polyexamples:sumRec1(),polyexamples:ex1()}}))),
    ?assertEqual(s([{[x],4,[1]}]),s(polymath:handler({sum,{polyexamples:sumRec1(),polyexamples:sumRec1()}}))).
