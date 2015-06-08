/* test very large asserts */

iter(500).
do_stat(no).
%iter(10000).
%do_stat(yes).

disp_stat :- do_stat(yes),!,statistics.
disp_stat.

makelist(N,_,[]) :- N=<0.
makelist(N,X,[X|R]) :- N>0, N1 is N-1, makelist(N1,X,R).

between(L,L,H) :- L=<H.
between(L,M,H) :- L<H, L1 is L+1, between(L1,M,H).

test8 :- \+ \+ test8nn,
	\+ \+ test9nn,
	\+ \+ test10nn,
	\+ \+ test11nn,
	\+ \+ test12nn,
	test13nn.

test8nn :-
	abolish(p/1),
%	makelist(10000,atom,Bigterm),  % this seg faults on SGI and NEXT
	makelist(1000,atom,Bigterm),
	assert(p(Bigterm)),
	p(Thebigterm),
	Bigterm == Thebigterm.

test9nn :-
	abolish(p/1),
	makelist(100,_,Bigterm),
	assert(p(Bigterm)),
	p(Thebigterm), 
	Thebigterm = [X|_],
	Bigterm = [X|_], 
	Bigterm == Thebigterm.

test10nn :-
	abolish(p/1),
	makelist(200,a,Littleterm),
	makelist(200,Littleterm,Bigterm),
	assert(p(Bigterm)),
	p(Thebigterm),
	Bigterm == Thebigterm.

test11nn :-
	iter(M),
	abolish(p/1),
	(between(1,_N,M),
	 assert(p(1)),
	 retract(p(1))
	;
	 \+ p(1)
	).

test12nn :-
	abolish(p/2),
	abolish(q/2),
	assert(q(1,2)),
	assert(q(3,4)),
	assert((p(X,Y):-q(Y,X))),
	findall(p(X,Y),p(X,Y),List),
	List == [p(2,1),p(4,3)].

% test assert/retract sequences

:- import retract_nr/1, reclaim_space/1 from assert.

test13_x(T) :-
	call(T), !,
	(do_stat(yes) -> nl,nl,nl, writeln(after(T));true),
	disp_stat.

test13nn :-
	disp_stat,
	test13_x(test13_abol),
	test13_x(test13_nr),
	test13_x(test13_d),
	test13_x(test13_nd),
	test13_x(test13_mix).

test13_abol :-
	iter(M),
	abolish(p/1),
	repeat,
        (	between(1,_N,M),
	 	assert(p(1)),
	 	abolish(p/1) ,
		fail
	; !
	).

test13_nr :-
	iter(M),
	abolish(p/1),
	repeat,
	(	between(1,_N,M),
	 	assert(p(1)),
		retract_nr(p(1)),
		p(1)
	; !
	),
	reclaim_space(p(_)),
	\+ p(1).


test13_d :-
	iter(M),
	abolish(p/1),
	repeat,
	(	(between(1,N,M),
	 	 assert(p(N)),
		 fail
		)
		; !
	),
	repeat,
	(	(between(1,N,M),
	 	 retract(p(N)),
		 fail
		)
		; !
	),
	\+ p(_).

test13_nd :-
	iter(M),
	abolish(p/1),
	repeat,
	(	(between(1,N,M),
	 	 assert(p(N)),
		 fail
		)
		; !
	),
	repeat,
	(	(between(1,_N,M),
	 	 retract(p(_)),
		 fail
		)
		; !
	),
	\+ p(_).


test13_mix :-
	iter(M),
	abolish(p/1),
	repeat,
	( between(1,_N,M),
	  assert(p(1)),
	  assert(p(1)),
	  retract_nr(p(_)),
	  retract(p(_)),
	  p(1)
	; !
	),
	reclaim_space(p(_)),
	\+ p(1).
