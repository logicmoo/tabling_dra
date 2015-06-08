:- op(999, xfx, '::=').
:- op(900, xfy, ['||','#']).
:- op(800, yfx, 'o').
:- op(750, xfx, ['@','\']).
:- op(700, xfx, ':=').

:- import conget/2, conset/2 from gensym.

comp :-
	retractall(newdef(_,_)),
	retractall(mem(_,_)),
	retractall(map(_,_)),
	fail.
comp :-
	Pid ::= Pexpr,
	comp(Pid, proc(Qid)),
	comp(Pexpr, Qexpr),
	assert(newdef(Qid, Qexpr)),
	fail.
comp.

comp((P1 # P2), choice(Q1, Q2)) :- !,
	comp(P1, Q1), comp(P2, Q2).
comp((P1 o P2), Q) :- !, 
	comp(P1, Q1), comp(P2, Q2), compose(Q1, Q2, Q).
comp((P1 '||' P2), par(Q1, Q2)) :- !, 
	comp(P1, Q1), comp(P2, Q2).
comp((P \ L), rest(Q, S)) :- !, 
	comp(P, Q),
	comp_set(L, S).
comp((P @ F), relab(Q, FN)) :- !, 
	comp(P, Q),
	comp_map(F, FN).
comp(in(V1), in(V2)) :- !, 
	V1 =.. [F|Args],
	gencodelist(Args, NewArgs, _),
	V2 =.. [F|NewArgs].
comp(out(V1), out(V2)) :- !, 
	V1 =.. [F|Args],
	gencodelist(Args, NewArgs, _),
	V2 =.. [F|NewArgs].
comp(type(X, T), code(true)) :- !, 
	X = (_,T).
comp((X := E), code(Code)) :- !,
	X = (Value, Dom),
	gencode(E, Ret, Ecode),
	seq(Ecode, (Value is Ret), Ecode1),
	(var(Dom) -> Code = Ecode1
		; seq(Ecode1, dom(Dom, Value), Code)).
comp(if(C, P1, P2), Q) :- !,
	gencond(C, Cond, Code),
	comp(P1, Q1),
	comp(P2, Q2),
	compose(code(Code), cond(Cond, Q1, Q2), Q).
comp(nil, nil) :- !.   %% YSR
comp(P, action(P)) :- atom(P), \+ process(P), !.

comp(P, Q) :-
	P =.. [F|Args],
	gencodelist(Args, NewArgs, Code),
	Q1 =.. [F|NewArgs],
	(process(P) -> Q = proc(Q1) ;
		       Q = code(C), seq(Code, Q1, C)).

process(P) :- P ::=  _.

gencode(X, Value, Code)  :- X = (Value, Dom), !,
	(var(Dom) -> Code = true; code = dom(Dom, Value)).
gencode(I, I, true) :- atomic(I), !.
gencode(T, Value, Code) :-
	T =.. [Op | Args],
	gencodelist(Args, NewArgs, C),
	Value =.. [Op | NewArgs],
	seq(C, true, Code).

gencodelist([], [], true).
gencodelist([X|Xs], [V|Vs], Code) :-
	gencode(X, V, C), gencodelist(Xs, Vs, Cs), seq(C, Cs, Code).

gencond(Cond, NewCond, Code) :-
	Cond =.. [Op | Args],
	gencodelist(Args, NewArgs, Code),
	NewCond =.. [Op | NewArgs].

compose(code(true), Q, Q) :- !.
compose(Q, code(true), Q) :- !.
compose(code(Q1), code(Q2), code(Q)) :- !, seq(Q1, Q2, Q).
compose(code(Q1), pref(code(Q2), P), pref(code(C), P)) :- !, 
			  seq(Q1, Q2, C).
compose(Q1, Q2, pref(Q1, Q2)).

seq(true, X, X) :- !.
seq(','(X,Xs), Y, ','(X,Z)) :- !, seq(Xs, Y, Z).
seq(X, true, X) :- !.
seq(X, Y, ','(X,Y)).

comp_map(F, FN) :-
	conget(function_name, N1),
	N is N1 + 1,
	conset(function_name, N),
	FN = N,
	comp_map_list(F, FN).

comp_map_list([], _).
comp_map_list([(From/To) | Rest], FN) :-
	assert(defmap(FN, From, To)),
	comp_map_list(Rest, FN).

comp_set(L, S) :-
	conget(set_name, N1),
	N is N1 + 1,
	conset(set_name, N),
	S = N,
	( L = '{}' -> true
	; L = '{}'(L1), comp_set_list(L1, S)
	).

comp_set_list(','(X, Xs), S) :- !,
	assert(mem(S, X)),
	comp_set_list(Xs, S). 
comp_set_list(X, S) :-
	assert(mem(S, X)).	


%% test

p ::= type(X,i) o type(Y,i) o in(a(X)) o Y := X+1 o q(X,Y).

q1(X) ::= in(chanIn(Y)) o 
		if(Y<X,
			failq,
			Z := Y+1 o out(chanOut(Z)) o 
			 if(Y>9, cycleq o Z := 1 o q1(Z), q1(Z))
		   ).

