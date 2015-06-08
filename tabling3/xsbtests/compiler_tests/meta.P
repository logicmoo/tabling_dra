:- import get_residual/2,get_calls/3,tnot/1 from tables.
:- import member/2 from basics.
:- auto_table.
:- op(700,xfx,'/=/').
:- op(900,fy,'~').
:- op(900,fy,'not').
:- op(1200,xfx,'<-').

demo(A):- demo_t(A).
demo(A):- demo_tu(A).

demo_t(true).
demo_tu(true).
demo_t(','(A,B)):-!,
	demo_t(A),demo_t(B).
demo_tu(','(A,B)):-!,
	demo_tu(A),demo_tu(B).
demo_t(not(A)):-!,
	tnot(demo_tu(A)).
demo_tu(not(A)):-!,
	tnot(demo_t(A)).
demo_t('/=/'(X,Y)):- X \== Y.
demo_tu('/=/'(X,Y)):- X \== Y.

demo_t(A):- '<-'(A,B), demo_t(B).
demo_tu(A):- '<-'(A,B), demo_tu(B),
	     exchange(A,A_opp),demo_tu(not(A_opp)).

exchange(~B,B):-!.
exchange(B,~B).


show_model:-
	domain(List),
	member(X,List),
	exchange(X,Xopp),
	(demo(X) ; demo(Xopp)),fail.

show_model:-
	domain(List),
	show_facts(true,List),
	show_facts(false,List),
	show_facts(undefined,List).

show_facts(true,List):- 
	write('true facts are: '),
	member(X,List),sign(X,Xsign),
	true(Xsign),
	write(Xsign),write(',  '),fail.
show_facts(false,List):- 
	write('(default) false facts are: '),
	member(X,List),sign(X,Xsign),
	false(Xsign),
	write(Xsign),write(',  '),fail.
show_facts(undefined,List):- 
	write('undefined facts are: '),
	member(X,List),sign(X,Xsign),
	undefined(Xsign),
	write(Xsign),write(',  '),fail.
show_facts(_,_):- nl.

true(Atom):- get_residual(demo_t(Atom),[]).
false(Atom):- get_calls(demo_tu(Atom),_,_), 
		\+ get_residual(demo_tu(Atom),_).
undefined(Atom):-get_residual((demo_t(Atom)),List), List \== [],
		 \+ false(Atom).

sign(X,X).
sign(X,~X).

at:- abolish_all_tables.
sm:- show_model.

end_of_file.
