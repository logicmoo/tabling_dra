%% Gopal Gupta's LTL interpreter (modified by F.K.).

:- op( 10,  fy , ~   ).   % not
:- op( 20, xfy , ^   ).   % and
:- op( 30, xfy , v   ).   % or
:- op( 10,  fy , x   ).   % LTL: "next"
:- op( 10,  fy , f   ).   % LTL: "eventually"
:- op( 10,  fy , g   ).   % LTL: "always"
:- op( 20, xfx , u   ).   % LTL: "until"
:- op( 20, xfx , r   ).   % LTL: "release"

:- [ 'normalize.pl' ].


:- top check/2.          % make check( S, F ) available in its original form

% Don't transform these:
:- bottom holds/2, normalize/2, proposition/1, state/1, trans/2, trans_all/2.


%% Check whether the state satisfies the formula.
%% This is done by checking that it does not satisfy the formula's negation.
%% (We have to apply the conditional, because our tabling interpreter does not
%%  support the cut.)

check( State, Formula ) :-
        check_consistency,
        once( normalize( ~ Formula, NormalizedNegationOfFormula ) ),
        (
            once( verify( State, NormalizedNegationOfFormula ) )
        ->
            fail
        ;
            true
        ).


% Check the consistency of the automaton's description.

check_consistency :-
        check_propositions,
        check_transitions.


% Make sure propositions don't clash with operators.

check_propositions :-
        proposition( P ),
        member( P, [ 'v', 'x', 'f', 'g', 'u', 'r' ] ),
        !,
        write( user_error, '\"' ),
        write( user_error, P ),
        write( user_error, '\": ' ),
        write( user_error,
          '\"v\", \"x\", \"f\", \"g\", \"u\" and \"r\" cannot be propositions!'
             ),
        nl( user_error ),
        fail.

check_propositions :-
        proposition( P ),
        \+ atom( P ),
        !,
        write( user_error, '\"' ),
        write( user_error, P ),
        write( user_error, '\": ' ),
        write( user_error, 'a proposition must be an atom!' ),
        nl( user_error ),
        fail.

check_propositions.


% Make sure that there is no state with no outgoing transitions, and that all
% transitions are between states.

check_transitions :-
        trans( S1, S2 ),
        (var( S1 ) ;  var( S2 ) ; \+ state( S1 ) ; \+ state( S2 )),
        !,
        write( user_error, 'Transitions can only occur between states: ' ),
        write( user_error, S1 ),
        write( user_error, ' ---> ' ),
        write( user_error, S2 ),
        nl( user_error),
        fail.

check_transitions :-
        state( S ),
        (\+ trans( S, Set ) ; trans( S, [] )),
        !,
        write( user_error, 'No transition out of state ' ),
        write( user_error, S ),
        nl( user_error ),
        fail.

check_transitions.



%--- The formula is normalized: only propositions can be negated.

verify( S, g A   ) :-  once( coverify( S, g A   ) ).
verify( S, A r b ) :-  once( coverify( S, A r B ) ).
verify( S, A     ) :-  A \= g _,  a \= _ r _,  tverify( S, A ).


:- tabled tverify/2.

tverify( S, A     ) :-  proposition( A ),    holds( S, A ).

tverify( S, ~ A   ) :-  proposition( A ), \+ holds( S, A ).

tverify( S, A ^ B ) :-  verify( S, A )  , verify( S, B ).

tverify( S, A v B ) :-  verify( S, A )  ; verify( S, B ).

tverify( S, f A   ) :-  verify( S, A )  ; verify( S, x f A ).

tverify( S, A u B ) :-  verify( S, B )  ; verify( S, A ^ x( A u B) ).

tverify( S, x A   ) :-  trans( S, S2 )  , verify( S2, A ).

                          % The last clause is correct only because the query is
                          % always negated, so for a successful query we will
                          % try out all the relevant clauses of trans/2 through
                          % failing.


:- coinductive coverify/2.

coverify( S, g A   ) :-  verify( S, A ^ x g A ).

coverify( S, A r B ) :-  verify( S, A ^ B ).

coverify( S, A r B ) :-  verify( S, B ^ x( A r B ) ).
