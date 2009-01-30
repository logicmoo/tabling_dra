%--- Normalize an LTL formula by pushing negations to the level of propositions
%--- and applying some absorption and idempotency laws.
%---
%--- NOTE: see ltl_interpreter for the declarations of operators.



normalize( ~( A v B ) , Normalized ) :-
        normalize( ~ A ^ ~ B , Normalized ).

normalize( ~( A ^ B ) , Normalized ) :-
        normalize( ~ A v ~ B , Normalized ).

normalize( ~ ~ A , Normalized ) :-
        normalize( A , Normalized ).

normalize( ~ x A , Normalized ) :-
        normalize( x ~ A , Normalized ).

normalize( ~ f A , Normalized ) :-
        normalize( g ~ A , Normalized ).

normalize( ~ g A , Normalized ) :-
        normalize( f ~ A , Normalized ).

normalize( ~( A u B ) , Normalized ) :-
        normalize( ~ A r ~ B , Normalized ).

normalize( ~( A r B ) , Normalized ) :-
        normalize( ~ A u ~ B , Normalized ).

normalize( A v B , NA v NB ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( A ^ B , NA ^ NB ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( ~ A , ~ NA ) :-
        normalize( A, NA ).

normalize( x A , x NA ) :-
        normalize( A, NA ).

normalize( f A , f NA ) :-
        normalize( A, NA ).

normalize( g A , g NA ) :-
        normalize( A, NA ).

normalize( A u B , NA u NB ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( A r B , NA r NB ) :-
        normalize( A, NA ),
        normalize( B, NB ).

normalize( f f A , Normalized ) :-
        normalize( f A , Normalized ).

normalize( g g A , Normalized ) :-
        normalize( g A , Normalized ).

normalize( A u (A u B) , Normalized ) :-
        normalize( A u B , Normalized  ).

normalize( (A u B) u B , Normalized ) :-
        normalize( A u B , Normalized  ).

normalize( f g f A , Normalized ) :-
        normalize( g f A , Normalized ).

normalize( g f g A , Normalized ) :-
        normalize( f g A , Normalized ).

normalize( ~ P, ~ P ) :-
        proposition( P ).

normalize( P, P ) :-
        proposition( P ).
