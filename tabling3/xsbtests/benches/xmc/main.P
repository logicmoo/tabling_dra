:- import close_open_tables/0 from machine.

:- import trie_assert/1, trie_retract/1  from tables.

?- [basics, calculus2, modelcheck1, comp2,lform].

bench:- bench_leader, bench_sieve.

bench_sieve:- [sieve4], comp, quick_bench([sieve4,a],a),quick_bench([sieve4,b],b).

bench_leader:- ['leader5.P'], comp, quick_bench([leader5],c).


bench(Type,X):-
	statistics(0),	% reset statistics
	abolish_all_tables,
	writeln('Bench'(X)),
	cputime(T0),
	(test(X), fail; true),
	cputime(T1),
	T is T1 - T0, 
	writeq(datum(Type,T)),writeln('.'),nl,
	statistics.

quick_bench(Type,X):-
%	statistics(0),	% reset statistics
	abolish_all_tables,
	(test(X), fail; true),
	abolish_all_tables,
	cputime(T0),
	(test(X), fail; true),
	cputime(T1),
	T is T1 - T0,
	writeq(datum(Type,T)),writeln('.'),nl.

test(a) :-
	models(proc(sieve), form(ae_finish)).
test(b) :-
	models(proc(sieve), form(ee_fail)).

test(c) :-
	models(proc(systemLeader), form(one_leader)).

test(d):- tc(proc(sieve),_).

test(e):- ttc(proc(sieve),_).

test(f):- ttc(proc(systemLeader),_).
 
tc(State,Stateout):- ttrans(State,_,Stateout).
tc(State,Stateout):-
	ttrans(State,_,Statemid),tc(Statemid,Stateout). 

:- table ttc/2.

ttc(State,Stateout):- trans(State,_,Stateout).
ttc(State,Stateout):- ttc(State,Statemid),trans(Statemid,_,Stateout).

count:- assert(p(0)),
	ttc(proc(sieve),_),
	retract(p(N)),
	N1 is N + 1,
	assert(p(N1)),
	fail.
count.

tcount:-trie_assert(p(0)),
	ttc(proc(sieve),_),
	trie_retract(p(N)),
	N1 is N + 1,
	trie_assert(p(N1)),
	fail.
tcount.

:- import conget/2, conset/2 from gensym.

ccount(Term,_Count):- 
	conset(numcalls, 0), 
	call(Term),
	incr(numcalls),
	fail.
ccount(_Term,Count):- 
	conget(numcalls, Count).

incr(Ctr) :-
	conget(Ctr, X),
	Y is X+1,
	conset(Ctr,Y).


%%

/* 
?- writeln('************************************************************').
?- ['leader2.P'].
?- bench.

?- writeln('************************************************************').
?- ['leader3.P'].
?- bench.

?- writeln('************************************************************').
?- ['leader4.P'].
?- bench.

?- writeln('************************************************************').
?- ['leader5.P'].
?- bench.
*/



	
	
	

