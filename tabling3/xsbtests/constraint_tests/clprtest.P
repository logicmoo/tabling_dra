
%:- [clpr].

:- import {}/1 from clpr.
:- import dump/3,projecting_assert/1 from dump.
:- import entailed/1 from clpr.
:- import sup/4, sup/2, maximize/1 from clpr.
:- import inf/4, inf/2, minimize/1 from clpr.
:- import member/2 from basics.
:- import bb_inf/3 from clpr.

:- import bootstrap_package/2 from packaging.
:- bootstrap_package('clpqr', clpr).

test:- test0.
test:- test1.
test:- test2.
test:- test3.
test:- test4.
test:- test5.
test:- test6.
test:- test7.
test:- test8.
test:- test9.
test:- test10.
test:- test11.
test:- test12.
test:- test13.
test:- test14.
test:- test14a.
test:- test15.
test:- test16.
test:- test18.
test:- test19.
test:- test20.
test:- test21.
test:- test22.
test:- test23.
test:- test24.
test:- test25.
test:- test26.
test:- test27.
test:- test28.
test:- test29.
test:- test30.
test:- test31.
test:- test32.
test:- test33.
test:- test34.
test:- test35.
test:- test36.
test:- test37.
test.

test0:- {X > 3},X = 4,writeln(test0('{X > 3},X = 4 succeeded')),fail.
test0:- (({X > 3},X = 2) -> 
	    writeln('{X > 3},X = 2 succeeded')
	  ; 
	    writeln(test0('{X > 3},X = 2 failed ok'))),
	 fail.
test1:- {X  > 3},	dump([X],[x],F),writeln(test1(F)),fail.
test2:- {X + 3 > 4},	dump([X],[x],F),writeln(test2(F)),fail.
test3:- {X > 3},{Y > 4},	dump([X,Y],[x,y],F),writeln(test3(F)),fail.
test4:- {X - 3 > Y},Y = 0,	dump([X],[x],F),writeln(test4(F)),fail.
test5:- {X  > 3},entailed(X > 2),writeln(test5_succeeded),fail.
test6:- {X  > 3},entailed(X > 4),writeln(test_6_error),fail.
test7:- {X  > 3 + Y},Y = 1,entailed(X > 2),writeln(test7_succeeded),fail.
test8:- {X  > 3 + Y},Y = 1,entailed(X > 5),writeln(error(test8)),fail.
test9:- {X  > 3*Y + 2},Y = 1,entailed(X > 5),writeln(succeeded(test9)),fail.
test10:- {X  > 3*Y + 2},Y = 1,	entailed(X > 10),writeln(error(test10)),fail.
test11:- {X  > 3*Y + 2},{Y > 0},entailed(X > Y),writeln(succeeded(test11)),fail.
test12:- {X  > 3*Y + 2},{Y > 0},entailed(X < Y),writeln(error(test12)),fail.
test13:- {X + Y = 10},X = Y,writeln(test13(X,Y)),fail.
test14:-{ 2*X+Y =< 16, X+2*Y =< 11,X+3*Y =< 15, Z = 30*X+50*Y}, sup(Z,Sup,[X,Y],Vertex),
        dump([X,Y,Z],[x,y,z],D),writeln(test14(Sup,Vertex)),
	member(C,D),writeln(test14(C)),fail.
test14a:-{ 2*X+Y =< 16, X+2*Y =< 11,X+3*Y =< 15, Z = 30*X+50*Y}, sup(Z,Sup),
        dump([X,Y,Z],[x,y,z],D),writeln(test14a(Sup)),
	member(C,D),writeln(test14a(C)),fail.
test15:- { 2*X+Y =< 16, X+2*Y =< 11,X+3*Y =< 15, Z = 30*X+50*Y}, 
	maximize(Z),writeln(test15(Z,X,Y)),fail.
test16:-{2*A+3*B=C/2}, C=10.0, A=B,writeln(test16(A,B,C)),fail.
test17:- {A-B+C=10, C=5+5},dump([A,B],[a,b],F),writeln(test17(F)),fail.
test18:-{sin(cos(X)) = 1/2},writeln(test18(X)),fail.
test19:-{sin(X+2+2)/sin(4+X) = Y},writeln(test19(Y)),fail.
test20:-  % probably ok -- cf. 424
	{ ((X+Y)*(Y+X)) / X = Y*Y/X+99},
	dump([X,Y],[x,y],F),
	writeln(test20(F)),fail.
test21:- {exp(X+Y+1,2) = 3*X*X+Y*Y}, X=Y,writeln(test21(X,Y)),fail.
test22:- {exp(X+Y+Z+1,2)=0},dump([X,Y,Z],[x,y,z],F),writeln(test22(F)),fail.
test23:- {X >= Y+Z, Y > 1, Z > 1}, bb_inf([Y,Z],X,Inf),	writeln(test23(Inf)),fail.
test24:- {X > 3}, projecting_assert(r(X)),fail.
test24:- r(X),entailed(X > 2),writeln(test24_succeeded),fail.
test25:- {12 = pow(X,3.585)},writeln(test25(X)),fail.
test26:- root(3,R),writeln(test26(R)),fail.
test27:- e(8,E),writeln(test27(E)),fail.
test28:- mg(P,12,0.01,B,Mp), dump([P,B,Mp],[p,b,mp],F),writeln(test28(F)),fail.
test29:- conv_hull([ [1,1], [2,0], [3,0], [1,2], [2,2] ], [X,Y]),dump([X,Y],[x,y],F),
	sort(F,F1),writeln(test29(F1)),fail.
test30:-{ 2*X+Y >= 16, X+2*Y >= 11,X+3*Y >= 15, Z = 30*X+50*Y}, inf(Z,Sup,[X,Y],Vertex),
        dump([X,Y,Z],[x,y,z],D),writeln(test30(Sup,Vertex)),
	member(C,D),writeln(test30(C)),fail.
test31:-{ 2*X+Y >= 16, X+2*Y >= 11,X+3*Y >= 15, Z = 30*X+50*Y}, inf(Z,Sup),
        dump([X,Y,Z],[x,y,z],D),writeln(test31(Sup)),
	member(C,D),writeln(test31(C)),fail.
test32:-{ 2*X+Y >= 16, X+2*Y >= 11,X+3*Y >= 15, Z = 30*X+50*Y}, minimize(Z),
	writeln(test32(X,Y,Z)),fail.

% TLS: added with nf_r change of 03/12.
test33:- {1 = cos(X)}, writeln(test33(X)),fail.
test34:- {-1 = cos(X)}, writeln(test34(X)),fail.
test35:- {1 = -cos(X)}, writeln(test35(X)),fail.
% TLS: these two should not fire.
test36:- { -25 = _X^(-2.5) }, writeln(test36(X)),fail.
test37:- { -25 = _X^2.5 }, writeln(test37(X)),fail.

/* Below are copied out of Sicstus manual */
/* Newton method for square root of 2 */
root(N, R) :-
root(N, 1, R).
root(0, S, R) :- !, S=R.
root(N, S, R) :-
	N1 is N-1,
	{ S1 = S/2 + 1/S },
	root(N1, S1, R).

/* N iterations in computation of E = e */
e(N, E) :-
	{ Err =:= exp(10,-(N+2)), Half =:= 1/2 },
	inv_e_series(Half, Half, 3, Err, Inv_E),
	{ E =:= 1/Inv_E }.
inv_e_series(Term, S0, _, Err, Sum) :-
	{ abs(Term) =< Err }, !,
	S0 = Sum.
inv_e_series(Term, S0, N, Err, Sum) :-
	N1 is N+1,
	{ Term1 =:= -Term/N, S1 =:= Term1+S0 },
	inv_e_series(Term1, S1, N1, Err, Sum).

mg(P,T,I,B,MP):-
   {
    T = 1,
	B + MP = P * (1 + I)
    }.
mg(P,T,I,B,MP):-
   {
    T > 1,
	P1 = P * (1 + I) - MP,
	T1 = T - 1
    },
mg(P1, T1, I, B, MP).

example(2, [X0,X1,X2,X3,X4]) :-
{
+87*X0 +52*X1 +27*X2 -54*X3 +56*X4 =< -93,
+33*X0 -10*X1 +61*X2 -28*X3 -29*X4 =< 63,
-68*X0 +8*X1 +35*X2 +68*X3 +35*X4 =< -85,
+90*X0 +60*X1 -76*X2 -53*X3 +24*X4 =< -68,
-95*X0 -10*X1 +64*X2 +76*X3 -24*X4 =< 33,
+43*X0 -22*X1 +67*X2 -68*X3 -92*X4 =< -97,
+39*X0 +7*X1 +62*X2 +54*X3 -26*X4 =< -27,
+48*X0 -13*X1 +7*X2 -61*X3 -59*X4 =< -2,
+49*X0 -23*X1 -31*X2 -76*X3 +27*X4 =< 3,
-50*X0 +58*X1 -1*X2 +57*X3 +20*X4 =< 6,
-13*X0 -63*X1 +81*X2 -3*X3 +70*X4 =< 64,
+20*X0 +67*X1 -23*X2 -41*X3 -66*X4 =< 52,
-81*X0 -44*X1 +19*X2 -22*X3 -73*X4 =< -17,
-43*X0 -9*X1 +14*X2 +27*X3 +40*X4 =< 39,
+16*X0 +83*X1 +89*X2 +25*X3 +55*X4 =< 36,
+2*X0 +40*X1 +65*X2 +59*X3 -32*X4 =< 13,
-65*X0 -11*X1 +10*X2 -13*X3 +91*X4 =< 49,
+93*X0 -73*X1 +91*X2 -1*X3 +23*X4 =< -87
}.

conv_hull(Points, Xs) :-
	lin_comb(Points, Lambdas, Zero, Xs),
	zero(Zero),
	polytope(Lambdas).
polytope(Xs) :-
	positive_sum(Xs, 1).
positive_sum([], Z) :- {Z=0}.
positive_sum([X|Xs], SumX) :-
	{ X >= 0, SumX = X+Sum },
	positive_sum(Xs, Sum).
zero([]).
zero([Z|Zs]) :- {Z=0}, zero(Zs).
lin_comb([], [], S1, S1).
lin_comb([Ps|Rest], [K|Ks], S1, S3) :-
	lin_comb_r(Ps, K, S1, S2),
	lin_comb(Rest, Ks, S2, S3).
lin_comb_r([], _, [], []).
lin_comb_r([P|Ps], K, [S|Ss], [Kps|Ss1]) :-
	{ Kps = K*P+S },
	lin_comb_r(Ps, K, Ss, Ss1).

filled_rectangle(A, C) :-
	{ A >= 1 },
	distinct_squares(C),
	filled_zone([-1,A,1], _, C, []).
distinct_squares([]).
distinct_squares([B|C]) :-
	{ B > 0 },
	outof(C, B),
	distinct_squares(C).
outof([], _).
outof([B1|C], B) :-
	{ B =\= B1 }, % *** note disequation ***
	outof(C, B).
filled_zone([V|L], [W|L], C0, C0) :-
	{ V=W,V >= 0 }.
filled_zone([V|L], L3, [B|C], C2) :-
	{ V < 0 },
	placed_square(B, L, L1),
	filled_zone(L1, L2, C, C1),
	{ Vb=V+B },
filled_zone([Vb,B|L2], L3, C1, C2).
placed_square(B, [H,H0,H1|L], L1) :-
	{ B > H, H0=0, H2=H+H1 },
placed_square(B, [H2|L], L1).
placed_square(B, [B,V|L], [X|L]) :-
	{ X=V-B }.
placed_square(B, [H|L], [X,Y|L]) :-
	{ B < H, X= -B, Y=H-B }.

test15(Z,X,Y):- { 2*X+Y =< 16, X+2*Y =< 11,X+3*Y =< 15, Z = 30*X+50*Y},maximize(Z).

end_of_file.




