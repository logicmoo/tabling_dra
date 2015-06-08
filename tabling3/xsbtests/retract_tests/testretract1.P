:- import asserti/2, assertai/2, retract_nr/1, reclaim_space/1 from assert.

:- import pp/0 from usermod.

test_0a :- abolish(p), retractall(p),
	(  retract(p) ; 
	   retract_nr(p) ; 
	   reclaim_space(p) 
	),
	   retractall(pp),
	(  retract(pp) ; 
	   retract_nr(pp) ; 
	   reclaim_space(pp)
	).

test_a :-
	abolish(p/4),
	assert(p(b,1,2.5,f(4,a,1.5,g(b,2,5.1)))),
	findall(p(X,Y,Z,W),p(X,Y,Z,W),L1), 
	L1 == [p(b,1,2.5,f(4,a,1.5,g(b,2,5.1)))],
	retractall(p(_,_,_,_)),retractall(p(_)),
	assert(p(_,_,_,_)),
	assert((p(1) :- p(b,1,2.5,f(4,a,1.5,g(b,2,5.1))))),
	findall(X,p(X),[1]),
	retractall(p(_,_,_,_)),retractall(p(_)),
	assert(p(b,1,_,f(4,a,_,g(b,2,_)))),
	assert((p(1) :- p(b,1,2.5,f(4,a,1.5,g(b,2,5.1))))),
	findall(X,p(X),[1]),
	retractall(p(_)),findall(X,p(X),[]),
	abolish(p/1),

	retractall(p(_,_,_,_)),retractall(p(_)),
	assert(p(b,1,c,f(4,a,e,g(b,2,d)))),
	assert((p(1) :- p(b,1,_,f(4,a,_,g(b,2,_))))),
	findall(X,p(X),[1]),
	retractall(p(_)),findall(X,p(X),[]),
	abolish(p/4), abolish(p/1),

	assert(p(b,1,c,f(4,a,e,g(b,2,d)))),
	assert((p(1) :- p(b,1,_,f(4,a,X,g(b,2,X))))),
	findall(X,p(X),[]),
	abolish(p/4), abolish(p/1),

	assert(p(b,1,c,f(4,a,e,g(b,2,d)))),
	assert((p(1) :- p(b,1,X,f(4,a,X,g(b,2,_))))),
	findall(X,p(X),[]),
	retractall(p(_,_,_,_)),
	abolish(p/4), abolish(p/1),

	retractall(p(_,_)), 
	assert(p(X,f(X))),
	assert((p(X,f(Y)) :- X=Y)),
	assert((p(X,f(Y)) :- g(X)=g(Y))),
	findall(p(X,Y),p(X,f(Y)),[p(X1,X2),p(X3,X4),p(X5,X6)]),
	var(X1),var(X3),var(X5),X1==X2,X3==X4,X5==X6,
	abolish(p/2), abolish(p/4), abolish(p/1),

	true.

test_b :-
	retractall(p(_)), retractall(q(_,_,_)),
	assert((p(X) :- q(X,a,X))),
	assert(q(a,b,a)), assert(q(b,a,b)), assert(q(c,a,d)), 
	findall(X,p(X),[b]),

	retractall(p(_)), retractall(q(_,_,_)),
	assert((p(1) :- q(X,a,X))),
	assert(q(a,b,a)), assert(q(b,a,b)), assert(q(c,a,d)), 
	findall(X,p(X),[1]),

	retractall(p(_)), retractall(q(_)),
	assert((p(1) :- q(f(X,a,X)))),
	assert(q(f(a,b,a))), assert(q(f(b,a,b))), assert(q(f(c,a,d))), 
	findall(X,p(X),[1]),

	retractall(p(_)), retractall(q(_)),
	assert((p(1) :- q(f(X,a,X)))),
	assert(q(f(a,b,a))), assert(q(f(c,a,d))), 
	findall(X,p(X),[]),

	retractall(p(_)), retractall(q(_,_,_)),
	assert((p(1) :- q(X,a,X))),
	assert(q(a,b,a)), assert(q(c,a,d)), 
	findall(X,p(X),[]),

	retractall(p(_)), retractall(q(_)), retractall(r(_)),
	assert((p(X) :- q(X),r(X))),
	assert(p(a)),
	assert(q(b)),assert(q(c)),assert(q(d)),
	assert(r(c)),assert(r(d)),assert(r(e)),
	findall(X,p(X),[c,d,a]),
	retractall(p(_)), retractall(q(_)), retractall(r(_)),
	assert((p(X) :- q(X),!,r(X))),
	assert(p(a)),
	assert(q(b)),assert(q(c)),assert(q(d)),
	assert(r(c)),assert(r(d)),assert(r(e)),
	findall(X,p(X),[]),
	retractall(p(_)), retractall(q(_)), retractall(r(_)),
	assert((p(X) :- q(X),r(X),!)),
	assert(p(a)),
	assert(q(b)),assert(q(c)),assert(q(d)),
	assert(r(c)),assert(r(d)),assert(r(e)),
	findall(X,p(X),[c]),
	retractall(q(_,_,_)),		% ???, delete and doesn't work
	retractall(p(_)), retractall(q(_)), retractall(r(_)),
	true.

test_c :-
	retractall(c(_,_)),
	assert(c(X,a)),retractall(c(X,a)),findall(Y,c(X,Y),[]),
	assert(c(X,a)),assert(c(X,b)),retractall(c(X,b)),
			findall(Y,c(X,Y),[a]),
	assert(c(X,b)),assert(c(X,c)),retractall(c(X,b)),
			findall(Y,c(X,Y),[a,c]),
	assert(c(X,d)),retractall(c(X,c)),findall(Y,c(X,Y),[a,d]),
	retractall(c(X,a)),findall(Y,c(X,Y),[d]),
	retractall(c(X,d)),findall(Y,c(X,Y),[]),

	assert(c(a,X)),retractall(c(a,X)),findall(X,c(X,Y),[]),
	assert(c(a,X)),assert(c(b,X)),retractall(c(b,X)),
			findall(X,c(X,Y),[a]),
	assert(c(b,X)),assert(c(c,X)),retractall(c(b,X)),
			findall(X,c(X,Y),[a,c]),
	assert(c(d,X)),retractall(c(c,X)),findall(X,c(X,Y),[a,d]),
	retractall(c(a,X)),findall(X,c(X,Y),[d]),
	retractall(c(d,X)),findall(X,c(X,Y),[]),
	retractall(c(_,_)).

test_d :-
	retractall(c(_,_)),
	asserta(c(X,a)),retractall(c(X,a)),findall(Y,c(X,Y),[]),
	asserta(c(X,a)),asserta(c(X,b)),retractall(c(X,b)),
			findall(Y,c(X,Y),[a]),
	asserta(c(X,b)),asserta(c(X,c)),retractall(c(X,b)),
			findall(Y,c(X,Y),[c,a]),
	asserta(c(X,d)),retractall(c(X,c)),findall(Y,c(X,Y),[d,a]),
	retractall(c(X,a)),findall(Y,c(X,Y),[d]),
	retractall(c(X,d)),findall(Y,c(X,Y),[]),

	asserta(c(a,X)),retractall(c(a,X)),findall(X,c(X,Y),[]),
	asserta(c(a,X)),asserta(c(b,X)),retractall(c(b,X)),
			findall(X,c(X,Y),[a]),
	asserta(c(b,X)),asserta(c(c,X)),retractall(c(b,X)),
			findall(X,c(X,Y),[c,a]),
	asserta(c(d,X)),retractall(c(c,X)),findall(X,c(X,Y),[d,a]),
	retractall(c(a,X)),findall(X,c(X,Y),[d]),
	retractall(c(d,X)),findall(X,c(X,Y),[]),
	retractall(c(_,_)).


test_r :-
	retractall(a(_)),
	assert(a(1)), findall(X,a(X),[1]),
	retractall(a(1)), findall(X,a(X),[]),
	assert(a(1)),assert(a(2)),assert(a(3)),assert(a(4)),
			findall(X,a(X),[1,2,3,4]),
	retractall(a(3)), findall(X,a(X),[1,2,4]),
	retractall(a(1)), findall(X,a(X),[2,4]),
	retractall(a(2)), findall(X,a(X),[4]),
	retractall(a(4)), findall(X,a(X),[]),
	asserta(a(1)), findall(X,a(X),[1]),
	retractall(a(1)), findall(X,a(X),[]),
	asserta(a(1)),asserta(a(2)),asserta(a(3)),asserta(a(4)),
			findall(X,a(X),[4,3,2,1]),
	retractall(a(3)), findall(X,a(X),[4,2,1]),
	retractall(a(1)), findall(X,a(X),[4,2]),
	retractall(a(2)), findall(X,a(X),[4]),
	retractall(a(4)), findall(X,a(X),[]),

	retractall(a(_,_)),
	assert(a(a,1)), findall(X,a(a,X),[1]),
	retractall(a(a,1)), findall(X,a(a,X),[]),
	assert(a(a,1)),assert(a(a,2)),assert(a(a,3)),assert(a(a,4)),
			findall(X,a(a,X),[1,2,3,4]),
	retractall(a(a,3)), findall(X,a(a,X),[1,2,4]),
	retractall(a(a,1)), findall(X,a(a,X),[2,4]),
	retractall(a(a,2)), findall(X,a(a,X),[4]),
	retractall(a(a,4)), findall(X,a(a,X),[]),
	asserta(a(a,1)), findall(X,a(a,X),[1]),
	retractall(a(a,1)), findall(X,a(a,X),[]),
	asserta(a(a,1)),asserta(a(a,2)),asserta(a(a,3)),asserta(a(a,4)),
			findall(X,a(a,X),[4,3,2,1]),
	retractall(a(a,3)), findall(X,a(a,X),[4,2,1]),
	retractall(a(a,1)), findall(X,a(a,X),[4,2]),
	retractall(a(a,2)), findall(X,a(a,X),[4]),
	retractall(a(a,4)), findall(X,a(a,X),[]),
	retractall(b(_)),
	assert(b(1)),assert(b(2)),retractall(b(2)),assert(b(3)),
		findall(X,b(X),[1,3]),
	retractall(b(_)), retractall(a(_,_)),
	true.

test_nr :-
% test non-reclaiming of space
	retractall(b(_)),
	assert(b(1)),assert(b(2)),assert(b(3)),assert(b(4)),
	(b(X),retract_nr(b(X)),fail;X=X,true),findall(X,b(X),[]),

	assert(b(1)), findall(X,b(X),[1]),
	retractall_nr(b(1)), findall(X,b(X),[]),
	assert(b(1)),assert(b(2)),assert(b(3)),assert(b(4)),
			findall(X,b(X),[1,2,3,4]),
	retractall_nr(b(3)), findall(X,b(X),[1,2,4]),
	retractall_nr(b(1)), findall(X,b(X),[2,4]),
	retractall_nr(b(2)), findall(X,b(X),[4]),
	retractall_nr(b(4)), findall(X,b(X),[]),
	asserta(b(1)), findall(X,b(X),[1]),
	retractall_nr(b(1)), findall(X,b(X),[]),
	reclaim_space(b(_)),
	asserta(b(1)),asserta(b(2)),asserta(b(3)),asserta(b(4)),
			findall(X,b(X),[4,3,2,1]),
	retractall_nr(b(3)), findall(X,b(X),[4,2,1]),
	retractall_nr(b(1)), findall(X,b(X),[4,2]),
	retractall_nr(b(2)), findall(X,b(X),[4]),
	retractall_nr(b(4)), findall(X,b(X),[]),
	reclaim_space(b(_,_)),

	retractall_nr(b(_,_)),
	assert(b(a,1)), findall(X,b(a,X),[1]),
	retractall_nr(b(a,1)), findall(X,b(a,X),[]),
	reclaim_space(b(_,_)),
	assert(b(a,1)),assert(b(a,2)),assert(b(a,3)),assert(b(a,4)),
			findall(X,b(a,X),[1,2,3,4]),
	retractall_nr(b(a,3)), findall(X,b(a,X),[1,2,4]),
	retractall_nr(b(a,1)), findall(X,b(a,X),[2,4]),
	retractall_nr(b(a,2)), findall(X,b(a,X),[4]),
	retractall_nr(b(a,4)), findall(X,b(a,X),[]),
	asserta(b(a,1)), findall(X,b(a,X),[1]),
	retractall_nr(b(a,1)), findall(X,b(a,X),[]),
	asserta(b(a,1)),asserta(b(a,2)),asserta(b(a,3)),asserta(b(a,4)),
			findall(X,b(a,X),[4,3,2,1]),
	retractall_nr(b(a,3)), findall(X,b(a,X),[4,2,1]),
	retractall_nr(b(a,1)), findall(X,b(a,X),[4,2]),
	retractall_nr(b(a,2)), findall(X,b(a,X),[4]),
	retractall_nr(b(a,4)), findall(X,b(a,X),[]),
	retractall_nr(b(_)),retractall_nr(b(_,_)),
	reclaim_space(b(_)), 
	true.

retractall_nr(X) :- retract_nr(X),fail.
retractall_nr(_).

cmppred(1).
cmppred(2).

test_cm :-
%	(\+ cmppred(3) -> assert(cmppred(3)) ; true),
%	findall(X,cmppred(X),[1,2,3]),
%	retractall(cmppred(3)),		%illegal!
	true.

