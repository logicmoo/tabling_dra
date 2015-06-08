:- import asserti/2, assertai/2, retract_nr/1, reclaim_space/1 from assert.


test0 :- 
	retractall(p(_,_)),
	asserti(p(a,1),[1,2]),
	asserti(p(b,2),[1,2]),
	asserti(p(c,3),[1,2]),
	asserti(p(d,4),[1,2]),
	asserti(p(e,5),[1,2]),
	findall(X,p(X,1),[a]),
	findall(X,p(X,2),[b]),
	findall(X,p(X,3),[c]),
	findall(X,p(X,4),[d]),
	findall(X,p(X,5),[e]),
	findall(X,p(a,X),[1]),
	findall(X,p(b,X),[2]),
	findall(X,p(c,X),[3]),
	findall(X,p(d,X),[4]),
	findall(X,p(e,X),[5]),
	findall(p(X,Y),p(X,Y),[p(a,1),p(b,2),p(c,3),p(d,4),p(e,5)]),
	test00.

test00 :-
	findall(X,clause(p(X,1),true),[a]),
	findall(X,clause(p(X,2),true),[b]),
	findall(X,clause(p(X,3),true),[c]),
	findall(X,clause(p(X,4),true),[d]),
	findall(X,clause(p(X,5),true),[e]),
	findall(X,clause(p(a,X),true),[1]),
	findall(X,clause(p(b,X),true),[2]),
	findall(X,clause(p(c,X),true),[3]),
	findall(X,clause(p(d,X),true),[4]),
	findall(X,clause(p(e,X),true),[5]),
	findall(p(X,Y),clause(p(X,Y),true),[p(a,1),p(b,2),p(c,3),p(d,4),p(e,5)]),

	retractall(p(_,_)),
	true.

test0a :-
	retractall(p(_,_)),
	assertai(p(a,1),[1,2]),
	assertai(p(b,2),[1,2]),
	assertai(p(c,3),[1,2]),
	assertai(p(d,4),[1,2]),
	assertai(p(e,5),[1,2]),
	findall(X,p(X,1),[a]),
	findall(X,p(X,2),[b]),
	findall(X,p(X,3),[c]),
	findall(X,p(X,4),[d]),
	findall(X,p(X,5),[e]),
	findall(X,p(a,X),[1]),
	findall(X,p(b,X),[2]),
	findall(X,p(c,X),[3]),
	findall(X,p(d,X),[4]),
	findall(X,p(e,X),[5]),
	findall(p(X,Y),p(X,Y),[p(e,5),p(d,4),p(c,3),p(b,2),p(a,1)]),
	test0a0.

test0a0 :-
	findall(X,clause(p(X,1),true),[a]),
	findall(X,clause(p(X,2),true),[b]),
	findall(X,clause(p(X,3),true),[c]),
	findall(X,clause(p(X,4),true),[d]),
	findall(X,clause(p(X,5),true),[e]),
	findall(X,clause(p(a,X),true),[1]),
	findall(X,clause(p(b,X),true),[2]),
	findall(X,clause(p(c,X),true),[3]),
	findall(X,clause(p(d,X),true),[4]),
	findall(X,clause(p(e,X),true),[5]),
	findall(p(X,Y),clause(p(X,Y),true),
			[p(e,5),p(d,4),p(c,3),p(b,2),p(a,1)]),

	retractall(p(_,_)),
	true.

test1 :- 
	retractall(p(_,_,_)),
 	asserti(p(a,1,1),[1,2,3]),
	asserti(p(a,2,3),[1,2,3]),
	asserti(p(a,3,3),[1,2,3]),
	asserti(p(b,4,3),[1,2,3]),
	asserti(p(b,5,4),[1,2,3]),
	findall(p(X,Y,Z),p(X,Y,Z),
		[p(a,1,1),p(a,2,3),p(a,3,3),p(b,4,3),p(b,5,4)]),
	findall(X,p(X,Y,Y),[a,a]),
	findall(X,p(a,X,_),[1,2,3]),
	findall(X,p(a,X,3),[2,3]),
	findall(X,p(_,X,3),[2,3,4]),
	findall(X,p(_,4,X),[3]),
	test10.

test10 :-
	findall(p(X,Y,Z),clause(p(X,Y,Z),true),
		[p(a,1,1),p(a,2,3),p(a,3,3),p(b,4,3),p(b,5,4)]),
	findall(X,clause(p(X,Y,Y),true),[a,a]),
	findall(X,clause(p(a,X,_),true),[1,2,3]),
	findall(X,clause(p(a,X,3),true),[2,3]),
	findall(X,clause(p(_,X,3),true),[2,3,4]),
	findall(X,clause(p(_,4,X),true),[3]),

	retractall(p(a,_,_)),findall(p(X,Y,Z),p(X,Y,Z),[p(b,4,3),p(b,5,4)]),
	retractall(p(_,_,4)),findall(p(X,Y,Z),p(X,Y,Z),[p(b,4,3)]),
	retractall(p(_,_,_)),findall(p(X,Y,X),p(X,Y,Z),[]),
	true.

test2 :-
	retractall(q(_,_,_)),
 	asserti(q(a,1,1),[1,2,3]),
	asserti(q(a,2,3),[1,2,3]),
	asserti(q(a,6,Y),[1,2,3]),
	asserti(q(a,3,3),[1,2,3]),
	asserti(q(X,3,7),[1,2,3]),
	asserti(q(b,4,3),[1,2,3]),
	asserti(q(X,Y,Z),[1,2,3]),
	asserti(q(b,5,4),[1,2,3]),
	findall(q(X,Y,Z),q(X,Y,Z),
		[q(a,1,1),q(a,2,3),q(a,6,v1),q(a,3,3),q(v2,3,7),
			q(b,4,3),q(v3,v4,v5),q(b,5,4)]),
	findall(X,q(X,Y,Y),[a,a,a,v6]),
	findall(X,q(a,X,_),[1,2,6,3,3,v7]),
	findall(X,q(a,X,3),[2,6,3,v8]),
	findall(X,q(_,X,3),[2,6,3,4,v9]),
	findall(X,q(_,4,X),[3,v10]),
	test20.

test20 :-
	findall(q(X,Y,Z),clause(q(X,Y,Z),true),
		[q(a,1,1),q(a,2,3),q(a,6,v1),q(a,3,3),q(v2,3,7),
			q(b,4,3),q(v3,v4,v5),q(b,5,4)]),
	findall(X,clause(q(X,Y,Y),true),[a,a,a,v6]),
	findall(X,clause(q(a,X,_),true),[1,2,6,3,3,v7]),
	findall(X,clause(q(a,X,3),true),[2,6,3,v8]),
	findall(X,clause(q(_,X,3),true),[2,6,3,4,v9]),
	findall(X,clause(q(_,4,X),true),[3,v10]),

	retractall(q(_,_,3)),findall(q(X,Y,Z),q(X,Y,Z),
			[q(a,1,1),q(v1,3,7),q(b,5,4)]),
	retractall(q(_,3,_)),findall(q(X,Y,Z),q(X,Y,Z),
			[q(a,1,1),q(b,5,4)]),
	retractall(q(_,_,_)),findall([],q(X,Y,Z),[]),

	true.


longtest(N) :- N > 2,
	N1 is N-1, N2 is N-2,
	assert(r(N,N1,N2)),
	longtest(N1).
longtest(N) :- N=<2.
