% generated: 25 October 1989
% option(s): 
%
%   nreverse
%
%   David H. D. Warren
%
%   "naive"-reverse a list of 30 integers

%:- option(analyze).
%:- notoption(float).

bench_nrev(Type,N,Length):- 
	create_list(Length,L),
	cputime(T1),
	main1(N,L),
	cputime(T2),T is T2-T1,
	writeq(datum(Type,T)),writeln('.'),nl.

main1(N,L) :- 
	repeat(N),
	nreverse(L,_), fail.
main1(_,_).

create_list(1,[1]):- !.
create_list(N,[N|T]):- 
	N > 1,
	N1 is N - 1,
	create_list(N1,T).

repeat(N) :- N>=1.
repeat(N) :- N>1, N1 is N-1, repeat(N1).

nreverse([X|L0],L) :- nreverse(L0,L1), concatenate(L1,[X],L).
nreverse([],[]).

concatenate([X|L1],L2,[X|L3]) :- concatenate(L1,L2,L3).
concatenate([],L,L).

end_of_file.

nreverse :- nreverse([1,2,3,4,5,6,7,8,9,10,11,12,
		      13,14,15,16,17,18,19,20,21,
		      22,23,24,25,26,27,28,29,30],_).

