
:- export mycall/2.
:- import append/3 from basics.

mycall(Term,Var):- 
	apply_last(Term,Var,Newcall),
	call(Newcall).

apply_last(Call,Var,Newcall):-
	Call =.. List,
	append(List,[Var],Newlist),
	Newcall =.. Newlist.

