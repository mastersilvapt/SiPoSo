-module(poly_tests).

-include_lib("eunit/include/eunit.hrl").

s(XS) -> lists:sort(XS).

sum_test() ->
    ?assertEqual(s([{[x],2,[1]}]), s(poly_solver:handler({sum,{poly_examples:ex1(), poly_examples:ex1()}}))),
    ?assertEqual(s([{[x],1,[2]},{[z],1,[2]},{[x],6,[1]},{[x,y,z],1,[1,1,1]},{[],-5,[]}]), s(poly_solver:handler({sum,{poly_examples:ex3(), poly_examples:ex6()}}))),
    ?assertEqual(s([{[y,x,z],10,[4,2,1]},{[x],5,[2]},{[x,y],30,[1,2]},{[y],-100,[1]},{[x],2,[1]},{[],3,[]}]), s(poly_solver:handler({sum,{poly_examples:ex7(), poly_examples:ex2()}}))),
    ?assertEqual(s([{[z,y],1,[3,1]},{[x,y],2,[1,2]},{[x],1,[1]},{[],3,[]}]), s(poly_solver:handler({sum,{poly_examples:ex5(), poly_examples:ex1()}}))),
    ?assertEqual(s([{[x,y],125,[2,1]},{[z],1,[2]},{[x],-32,[1]},{[x,y,z],60,[1,3,1]},{[z,y],-30,[1,1]},{[x,z],-52,[1,2]}]), s(poly_solver:handler({sum,{poly_examples:ex9(), poly_examples:ex10()}}))),
    ?assertEqual(s([{[x,y],77,[2,1]},{[y,z],63,[1,2]},{[x],-40,[1]},{[],7,[]},{[x,y],2,[1,2]}]), s(poly_solver:handler({sum,{poly_examples:ex11(), poly_examples:ex4()}}))),
    ?assertEqual(s([{[y],32,[1]},{[x,z],2,[2,1]},{[z],1,[2]},{[y,x,z],10,[4,2,1]},{[x,y],30,[1,2]}]), s(poly_solver:handler({sum,{poly_examples:ex8(), poly_examples:ex7()}}))).

sub_test() ->
    ?assertEqual(s([{[],-3,[]},{[x],-5,[2]},{[x],-1,[1]}]),s(poly_solver:handler({sub,{poly_examples:ex1(),poly_examples:ex2()}}))),
    ?assertEqual(s([]), s(poly_solver:handler({sub,{poly_examples:ex2(),poly_examples:ex2()}}))),
    ?assertEqual(s([{[x,y],80,[2,1]}]),s(poly_solver:handler({sub,{poly_examples:ex11(),poly_examples:ex12()}}))).

mult_test() ->
    ?assertEqual(s([{[x],1,[2]}]),s(poly_solver:handler({mult,{poly_examples:ex1(),poly_examples:ex1()}}))),
    ?assertEqual(s([{[x],6,[2]},{[x],-2,[1]}]),s(poly_solver:handler({mult,{poly_examples:ex1(),poly_examples:ex3()}}))),
    ?assertEqual(s([{[x,y],-8460,[2,2]}, {[x,y],2250,[3,3]}, {[x,y],3200,[1,1]}, {[x,y,z],-320,[3,4,1]}, {[x,y,z],10,[2,4,3]}, {[x,y,z],30,[1,2,2]}, {[x,y,z],750,[4,5,1]}, {[y,z],-100,[1,2]}]),s(poly_solver:handler({mult,{poly_examples:ex9(),poly_examples:ex7()}}))).

rec_test() ->
    ?assertEqual(s([{[x],3,[1]}]),s(poly_solver:handler({sum,{poly_examples:sumRec1(),poly_examples:ex1()}}))),
    ?assertEqual(s([{[x],4,[1]}]),s(poly_solver:handler({sum,{poly_examples:sumRec1(),poly_examples:sumRec1()}}))).
