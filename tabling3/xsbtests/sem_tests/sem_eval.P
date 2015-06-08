/************************************************************************/
/*									*/
/* SEMANTICA System							*/
/* Copyright (C) SUNY at Stony Brook, 1995				*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  sem_eval.P
  Author(s)		:  Kostis F. Sagonas
  Last modification	:  June 1, 1995
========================================================================*/


:- export evaluate/1.

:- export back_of/2, left_of/2, right_of/2, in_front_of/2, between/3.

:- dynamic loc/5, property/3, world/3.

:- import loc/5, property/3, world/3 from usermod.

/*======================================================================*/
/*======================================================================*/

evaluate(Expr) :- etv(Expr).

/*======================================================================*/
/*  etv(+Expression)							*/
/*	Evaluates an Expression and finds its truth value.		*/
/*======================================================================*/

etv(true) :- !.
etv(not(Expr)) :- !, fail_if(etv(Expr)).
etv((Expr1,Expr2)) :- !, etv(Expr1), etv(Expr2).
etv((Expr1;Expr2)) :- !, ( etv(Expr1) -> true ; etv(Expr2) ).
etv(Evaluable) :- is_evaluable(Evaluable), call(Evaluable).

/*----------------------------------------------------------------------*/
/*  List of evaluable predicates, and stuff like that...		*/
/*----------------------------------------------------------------------*/

is_evaluable(back_of(_,_)).
is_evaluable(left_of(_,_)).
is_evaluable(right_of(_,_)).
is_evaluable(in_front_of(_,_)).
is_evaluable(between(_,_,_)).

/*----------------------------------------------------------------------*/
/*  Definitions of evaluable predicates.				*/
/*----------------------------------------------------------------------*/

back_of(Obj1, Obj2) :-
	loc(World, Obj1, _, Row1, _),
	loc(World, Obj2, _, Row2, _),
	Row1 > Row2.
left_of(Obj1, Obj2) :-
	loc(World, Obj1, _, _, Column1),
	loc(World, Obj2, _, _, Column2),
	Column1 < Column2.
right_of(Obj1, Obj2) :-
	loc(World, Obj1, _, _, Column1),
	loc(World, Obj2, _, _, Column2),
	Column1 > Column2.
in_front_of(Obj1, Obj2) :-
	loc(World, Obj1, _, Row1, _),
	loc(World, Obj2, _, Row2, _),
	Row1 < Row2.
between(BetweenObj, Obj1, Obj2) :-		%--- Obj1 BetweenObj Obj2 ---
	loc(World, Obj1, _, Row1, Col1),
	loc(World, Obj2, _, Row2, Col2),
	( Row1 =< Row2 -> 
		( Col1 =< Col2 ->
			loc(World, BetweenObj, _, BetweenRow, BetweenCol),
			fail
		; writeln('Not yet implemented') , fail
		)
	; between(BetweenObj, Obj2, Obj2)
	).

/* ------------------- end of file sem_eval.P ------------------------- */

