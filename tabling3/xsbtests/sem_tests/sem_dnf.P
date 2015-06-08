/************************************************************************/
/*									*/
/* SEMANTICA System							*/
/* Copyright (C) SUNY at Stony Brook, 1995				*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  sem_dnf.P
  Author(s)		:  Kostis F. Sagonas
  Last modification	:  July 20, 1995
========================================================================*/

:- export dnf/2.

/*======================================================================*/
/*  dnf(+Expression1, -Expression2)					*/
/*	Expression2 is Expression1 in disjunctive normal form.		*/
/*======================================================================*/

:- ti dnf/2.

dnf(ExprIn, ExprOut) :-
	simple_conjunctive_expr(ExprIn), !, ExprOut = ExprIn.
%% dnf(not(ExprIn), ExprOut) :-
%% 	simple_expr(ExprIn), !, reverse_expr(ExprIn, ExprOut).
dnf(not(not(ExprIn)), ExprOut) :-
	!,			%% RULE: ~(~A) = A
	dnf(ExprIn, ExprOut).
dnf(not(';'(ExprIn1,ExprIn2)), ExprOut) :-
	!,			%% RULE: ~(A \/ B) = (~A) & (~B)
	dnf(ExprIn1, ExprMid1),
	dnf(ExprIn2, ExprMid2),
	dnf(','(not(ExprMid1),not(ExprMid2)), ExprOut).
dnf(not(','(ExprIn1,ExprIn2)), ExprOut) :-
	!,			%% RULE: ~(A & B) = (~A) \/ (~B)
	dnf(ExprIn1, ExprMid1),
	dnf(ExprIn2, ExprMid2),
	dnf(';'(not(ExprMid1),not(ExprMid2)), ExprOut).
dnf(';'(';'(ExprIn1,ExprIn2),ExprIn3), ExprOut) :-
	!,			%% RULE: ((A \/ B) \/ C) = (A \/ (B \/ C))
	dnf(';'(ExprIn1,';'(ExprIn2,ExprIn3)), ExprOut).
dnf(';'(ExprIn1,ExprIn2), ';'(ExprOut1,ExprOut2)) :-
	simple_conjunctive_expr(ExprIn1),
	!,
	ExprOut1 = ExprIn1,
	dnf(ExprIn2, ExprOut2).
dnf(';'(ExprIn1,ExprIn2), ExprOut) :-
	!,
	dnf(ExprIn1, ExprMid1),
	dnf(ExprIn2, ExprMid2),
	dnf(';'(ExprMid1,ExprMid2), ExprOut).
dnf(','(','(ExprIn1,ExprIn2),ExprIn3), ExprOut) :-
	!,			%% RULE: ((A & B) & C) = (A & (B & C))
	dnf(','(ExprIn1,','(ExprIn2,ExprIn3)), ExprOut).
dnf(','(';'(ExprIn1,ExprIn2),ExprIn3), ExprOut) :-
	!,			%% RULE: ((A \/ B) & C) = ((A & C) \/ (B & C))
	dnf(';'(','(ExprIn1,ExprIn3),','(ExprIn2,ExprIn3)), ExprOut).
dnf(','(ExprIn1,';'(ExprIn2,ExprIn3)), ExprOut) :-
	!,			%% RULE: (A & (B \/ C)) = ((A & B) \/ (A & C))
	dnf(';'(','(ExprIn1,ExprIn2),','(ExprIn1,ExprIn3)), ExprOut).
dnf(','(ExprIn1,ExprIn2), ExprOut) :-
	!,
	dnf(ExprIn1, ExprMid1),
	dnf(ExprIn2, ExprMid2),
	dnf(','(ExprMid1,ExprMid2), ExprOut).


/*----------------------------------------------------------------------*/
/*  simple_conjunctive_expr(+Expression)				*/
/*	Expression can contain only ``and'' and ``not'' operators.	*/
/*----------------------------------------------------------------------*/

simple_conjunctive_expr(Expr) :-
	simple_expr(Expr).
simple_conjunctive_expr(not(Expr)) :-
	simple_conjunctive_expr(Expr).
simple_conjunctive_expr(','(Expr1,Expr2)) :-
	simple_conjunctive_expr(Expr1), simple_conjunctive_expr(Expr2).

/*----------------------------------------------------------------------*/
/*  simple_expr(+Expression)						*/
/*----------------------------------------------------------------------*/

simple_expr(not(_)) :- !, fail.
simple_expr(','(_,_)) :- !, fail.
simple_expr(';'(_,_)) :- !, fail.
simple_expr(_).

/* -------------------- end of file sem_dnf.P ------------------------- */

end_of_file.

%----------------------------------------------------------------------

| ?- dnf(','(not(a), ','(b,c)), E).

E = (not a  ','  b  ','  c);

no
| ?- dnf(','(not(a), ';'(b,c)), E).

E = (not a  ','  b  ';'  not a  ','  c);

no
| ?- dnf(','(';'(X=t,Y=t),','(val(X,s(chris,walks)),val(Y,s(mary,walks)))), E).

X = _677688
Y = _677752
E = (_677688 = t ',' val(_677688,s(chris,walks)) ',' val(_677752,s(mary,walks))
    ';'
     _677752 = t ',' val(_677688,s(chris,walks)) ',' val(_677752,s(mary,walks)))

no
| ?- dnf(','(val(X,s(chris,walks)),','(';'(X=t,Y=t),val(Y,s(mary,walks)))), E).

X = _677604
Y = _677752
E = (val(_677604,s(chris,walks)) ',' _677604 = t ',' val(_677752,s(mary,walks))
    ';'
     val(_677604,s(chris,walks)) ',' _677752 = t ',' val(_677752,s(mary,walks)))

no

