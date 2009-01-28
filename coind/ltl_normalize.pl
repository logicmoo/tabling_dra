%--- Normalize an LTL formula by pushing negations to the level of propositions
%--- and applying some absorption and idempotency laws.

normalize( not( or( A, B ) ), Normalized ) :-
        normalize( and( not( A ), not( B ) ), Normalized ).

normalize( not( and( A, B ) ), Normalized ) :-
        normalize( or( not( A ), not( B ) ), Normalized ).

normalize( not( not( A ) ), Normalized ) :-
        normalize( A, Normalized ).

normalize( not( x( A ) ), Normalized ) :-
        normalize( x( not( A ) ), Normalized ).

normalize( not( f( A ) ), Normalized ) :-
        normalize( g( not( A ) ), Normalized ).

normalize( not( g( A ) ), Normalized ) :-
        normalize( f( not( A ) ), Normalized ).

normalize( not( u( A, B ) ), Normalized ) :-
        normalize( r( not( A ), not( B ) ), Normalized ).

normalize( not( r( A, B ) ), Normalized ) :-
        normalize( u( not( A ), not( B ) ), Normalized ).

normalize( or( A, B ), or( NA, NB ) ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( and( A, B ), and( NA, NB ) ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( not( A ), not( NA ) ) :-
        normalize( A, NA ).

normalize( x( A ), x( NA ) ) :-
        normalize( A, NA ).

normalize( f( A ), f( NA ) ) :-
        normalize( A, NA ).

normalize( g( A ), g( NA ) ) :-
        normalize( A, NA ).

normalize( u( A, B ), u( NA, NB ) ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( r( A, B ), r( NA, NB ) ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( f( f( A ) ), Normalized ) :-
        normalize( f( A ), Normalized ).

normalize( g( g( A ) ), Normalized ) :-
        normalize( g( A ), Normalized ).

normalize( u( A, u( A, B ) ), Normalized ) :-
        normalize( u( A, B ), Normalized  ).

normalize( u( u( A, B ), B ), Normalized ) :-
        normalize( u( A, B ), Normalized  ).

normalize( f( g( f( A ) ) ), Normalized ) :-
        normalize( g( f( A ) ), Normalized ).

normalize( g( f( g( A ) ) ), Normalized ) :-
        normalize( f( g( A ) ), Normalized ).

normalize( P, P ) :-
        proposition( P ).
