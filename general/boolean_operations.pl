%%%  Utilities for performing operations on boolean expressions.             %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (March 2009).                         %%%
%%%                                                                          %%%
%%%  Last update: 2 March 2009.                                              %%%
%%%                                                                          %%%

:- ensure_loaded( errors_and_warnings ).


:- op( 500, yfx, and ).
:- op( 500, yfx, or  ).   % Following Dijkstra...
:- op( 490, fy , neg ).


%%------------------------------------------------------------------------------
%% bool_eval( + boolean expression, - true or false ):
%% Evaluate the expression.

bool_eval( V, _ ) :-
        var( V ),
        !,
        error( 'Variable boolean' ).

bool_eval( Expression, Value ) :-
        once( beval( Expression, Value ) ).

%
beval( false, false ).

beval( true, true ).

beval( neg false, true ).

beval( neg true, false ).

beval( neg Exp, Value ) :-
        beval( Exp, V ),
        beval( neg V, Value ).

beval( false and false, false ).

beval( false and true, false ).

beval( true and false, false ).

beval( true and true, true ).

beval( A and B, Value ) :-
        beval( A, VA ),
        beval( B, VB ),
        beval( VA and VB, Value ).

beval( false or false, false ).

beval( false or true, true ).

beval( true or false, true ).

beval( true or true, true ).

beval( A or B, Value ) :-
        beval( A, VA ),
        beval( B, VB ),
        beval( VA or VB, Value ).


%%------------------------------------------------------------------------------
%% Predicates for the operators.

bool_and( EA, EB, V ) :-
        bool_eval( EA and EB, V ).

bool_or( EA, EB, V ) :-
        bool_eval( EA or EB, V ).

bool_neg( E, V ) :-
        bool_eval( neg E, V ).

%%------------------------------------------------------------------------------
