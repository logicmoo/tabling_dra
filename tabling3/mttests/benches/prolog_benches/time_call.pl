
:- 	current_predicate(xsb_configuration,_) -> 
	assert((get_cputime(X):- cputime(X)))
	; (prolog_flag(dialect,swi) -> 
	    assert((get_cputime(X):- statistics(cputime,X)))
	    ; 	    assert((get_cputime(X):- statistics(cputime,[X|_]))) ).

mt_bench_call(Type,Length,Numthreads):- 
	make_list(Length,List),
	walltime(T1),
	n_par_execute(call_list(List),Numthreads),
	walltime(T2),T is T2-T1,
	writeq(datum(Type,Numthreads,T)),writeln('.'),nl,
	flush_output.

cpubench_call(Length):- 
	make_list(Length,List),
	cputime(T1),
	call_list(List),
	cputime(T2),T is T2-T1,
	writeq(datum([call],T)),writeln('.'),nl,
	flush_output.

/* For benchmark tests */
test:- 
	cputime_call(100000,Time),
	writeq(datum([call],Time)),writeln('.'),nl.


make_list(0,[]):- !.
make_list(N,[p(1)|T]):- 
	N1 is N - 1,
	make_list(N1,T).
	
cputime_call(N,Time):-
	make_list(N,List),
	get_cputime(Before),
	call_list(List),
	get_cputime(After),
	Time is After - Before.

call_list([]).
call_list([H|T]):-
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),call(H),call(H),
	call(H),call(H),
	call_list(T).


p(1).
p(1,2).

cputime_call2(N,Time):-
	make_list(N,List),
	get_cputime(Before),
	call2_list(List),
	get_cputime(After),
	Time is After - Before.

call2_list([]).
call2_list([H|T]):-
	call(H,_),call(H,_),call(H,_),call(H,_),
	call(H,_),call(H,_),call(H,_),call(H,_),
	call(H,_),call(H,_),
	call(H,_),call(H,_),call(H,_),call(H,_),
	call(H,_),call(H,_),call(H,_),call(H,_),
	call(H,_),call(H,_),
	call2_list(T).

end_of_file.

:- import call_c/1 from standard.

cputime_code_call(N,Time):-
	make_list(N,List),
	get_cputime(Before),
	code_call_list(List),
	get_cputime(After),
	Time is After - Before.

code_call_list([]).
code_call_list([H|T]):-
	call_c(H),call_c(H),call_c(H),call_c(H),
	call_c(H),call_c(H),call_c(H),call_c(H),
	call_c(H),call_c(H),
	call_c(H),call_c(H),call_c(H),call_c(H),
	call_c(H),call_c(H),call_c(H),call_c(H),
	call_c(H),call_c(H),
	code_call_list(T).
	
