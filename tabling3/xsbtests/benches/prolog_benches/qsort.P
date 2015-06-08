% generated: 16 November 1989
% option(s): SOURCE_TRANSFORM_1
%
%   qsort
%
%   David H. D. Warren
%
%   quicksort a list of 50 integers

%:- option(analyze).

:- xsb_configuration(compiler, CC), consult(rand, [CC]).

:- import prand/1 from rand.

bench_qsort(Type,N,Length):- 
	create_reverse_list(Length,L),
	cputime(T1),
	main1(N,L),
	cputime(T2),T is T2-T1,
	writeq(datum(Type,T)),writeln('.'),nl.

main1(N,L) :- repeat(N), qsort(L,_,[]), fail.
main1(_,_).

repeat(N) :- N>=1.
repeat(N) :- N>1, N1 is N-1, repeat(N1).

create_random_list(1,[R]):- !,prand(R).
create_random_list(N,[R|T]):- 
	N > 1,
	prand(R),
	N1 is N - 1,
	create_random_list(N1,T).

create_reverse_list(1,[1]):- !.
create_reverse_list(N,[N|T]):- 
	N > 1,
	N1 is N - 1,
	create_reverse_list(N1,T).

qsort([X|L],R,R0) :-
	partition(L,X,L1,L2),
	qsort(L2,R1,R0),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([X|L],Y,[X|L1],L2) :-
	X =< Y, !,
	partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
	partition(L,Y,L1,L2).
partition([],_,[],[]).






