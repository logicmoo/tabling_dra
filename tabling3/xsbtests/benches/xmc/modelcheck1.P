:- import ground/1 from basics.

:- index models/2-2.
:- table rec_models/2.

:- table tab_models/2, tab_not_models/2.
tab_models(A,B) :- models(A,B).
tab_not_models(A,B) :- not_models(A,B).

:- table ttrans/3.
ttrans(X,Y,Z) :- trans(X,Y,Z).

rec_models(State_s, X) :-
	fDef(X, Y), 
	(
	 (Y = lfp(Z), models(State_s, Z)) ;
	 (Y = gfp(Z), (ground(State_s) -> tnot(tab_not_models(State_s, Z))
			; writeln(tnot(tab_not_models(State_s, Z)))))
	).

models(State_s, form(X)) :-
	rec_models(State_s, X).

models(_State_s, tt).

models(State_s, fAnd(X_1, X_2))	:-
	models(State_s, X_1), models(State_s, X_2).
 
models(State_s, fOr(X_1, X_2))	:-
	models(State_s, X_1) ; 	models(State_s, X_2).

models(State_s, fDiam(Act_a, X)) :-
	trans(State_s, Act_a, State_t),
	models(State_t, X).

models(State_s, fDiamMinus(Act_a, X)) :-
	trans_not_a(State_s, Act_a, State_t),
	models(State_t, X).

models(State_s, fBox(Act_a, X))	:-
	tfindall(State_t, ttrans(State_s, Act_a, State_t), State_ts),
	lmodel(State_ts, X).

models(State_s, fBoxany(X))	:-
	tfindall(State_t, ttrans(State_s, Act_a, State_t), State_ts),
	lmodel(State_ts, X).

models(State_s, fBoxMinus(Act_a, X)) :-
	tfindall(State_t, ttrans_not_a(State_s, Act_a, State_t), State_ts),
	lmodel(State_ts, X).


:- table ttrans_not_a/3.

ttrans_not_a(State_s, Act_a, State_t) :-
	trans_not_a(State_s, Act_a, State_t).

trans_not_a(State_s, Act_a, State_t) :-
	trans(State_s, Act_b, State_t),
	\+ (Act_b == Act_a).

lmodel([], _X).		% empty list of states satisfy any property.
lmodel([State_s|List_state], X)	:-
	models(State_s, X),
	lmodel(List_state, X).


:- index not_models/2-2.
:- table rec_not_models/2.

rec_not_models(State_s, X) :-
	(fDef(X,Y) ->
		((Y = lfp(Z), (ground(State_s) ->
					tnot(tab_models(State_s, Z))
				; writeln(tnot(tab_models(State_s, Z))))
			) ;
		 (Y = gfp(Z), not_models(State_s, Z))
		)
	; true	% no definition for formula, so no state satisfies it.
	).
 
not_models(State_s, form(X)) :-
	rec_not_models(State_s, X).

not_models(_State_s, tt) :- fail.

not_models(State_s, fAnd(X_1, X_2)) :-
	not_models(State_s, X_1); not_models(State_s, X_2).

not_models(State_s, fOr(X_1, X_2)) :-
	not_models(State_s, X_1), not_models(State_s, X_2).

not_models(State_s, fDiam(Act_a, X)) :-
	tfindall(State_t, ttrans(State_s, Act_a, State_t), State_ts),
	l_not_models(State_ts, X).

not_models(State_s, fDiamMinus(Act_a, X)) :-
	tfindall(State_t, ttrans_not_a(State_s, Act_a, State_t), State_ts),
	l_not_models(State_ts, X).

not_models(State_s, fBox(Act_a, X)) :-
	trans(State_s, Act_a, State_t),
	not_models(State_t, X).

not_models(State_s, fBoxany(X)) :-
	trans(State_s, Act_a, State_t),
	not_models(State_t, X).

not_models(State_s, fBoxMinus(Act_a, X)) :-
	trans_not_a(State_s, Act_a, State_t),
	not_models(State_t, X).

l_not_models([], _X).
l_not_models([State_t|State_ts], X) :-
	not_models(State_t, X), l_not_models(State_ts, X).




