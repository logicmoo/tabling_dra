%% This is an experimental version of a pure Prolog counterpart of verifier.tlp.
%% Still under development!

:- op( 10,  fy , ~   ).   % not
:- op( 20, xfy , ^   ).   % and
:- op( 30, xfy , v   ).   % or
:- op( 10,  fy , x   ).   % LTL: "next"
:- op( 10,  fy , f   ).   % LTL: "eventually"
:- op( 10,  fy , g   ).   % LTL: "always"
:- op( 20, xfx , u   ).   % LTL: "until"
:- op( 20, xfx , r   ).   % LTL: "release"

:- [ 'normalize.pl' ].


%% Check whether the state satisfies the formula.
%% This is done by checking that it does not satisfy the formula's negation.
%% (We have to apply the conditional, because our tabling interpreter does not
%%  support the cut, and we don't yet support negation for coinduction.)

check( State, Formula ) :-
        check_consistency,
        (
            state( State )
        ->
            true
        ;
            write( '\"' ),
            write( State ),
            write( '\" is not a state' ),
            nl,
            fail
        ),
        write( 'Query for state ' ),
        write( State ),
        write( ': ' ),
        write( Formula ),
        nl,
        once( normalize( ~ Formula, NormalizedNegationOfFormula ) ),
        write( '(Negated and normalized: ' ),
        write( NormalizedNegationOfFormula ),
        write( ')' ),
        nl,
        (
            once( verify( State, NormalizedNegationOfFormula, [] ) )
        ->
            fail
        ;
            true
        ).


% Check the consistency of the automaton's description.
% NOTE: The dynamic declaration is necessary for Eclipse.
%       On Sicstus we will see a warning, but things will work fine otherwise.

:- dynamic automaton_error/0.

check_consistency :-
        retractall( automaton_error ),
        check_propositions,
        check_transitions,
        (
            automaton_error
        ->
            fail
        ;
            true
        ).


% Make sure propositions don't clash with operators.

check_propositions :-
        proposition( P ),
        (
            \+ atom( P )
        ->
            write( 'A proposition must be an atom: ' ),
            write( '\"' ),
            write( P ),
            write( '\"' ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        (
            member( P, [ 'v', 'x', 'f', 'g', 'u', 'r' ] )
        ->
            write( '\"v\", \"x\", \"f\", \"g\", \"u\" and \"r\" ' ),
            write( 'cannot be propositions: ' ),
            write( '\"' ),
            write( P ),
            write( '\"' ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_propositions.


% Make sure that there is no state with no outgoing transitions, and that all
% transitions are between states.

check_transitions :-
        trans( S1, S2 ),
        (
            (var( S1 ) ;  var( S2 ) ; \+ state( S1 ) ; \+ state( S2 ))
        ->
            write( 'Transitions can only occur between states: ' ),
            write( S1 ),
            write( ' ---> ' ),
            write( S2 ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_transitions :-
        state( S ),
        (
            (\+ trans( S, _Set ) ; trans( S, [] ))
        ->
            write( 'No transition out of state ' ),
            write( S ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_transitions.



%--- The formula is normalized: only propositions can be negated.

%% verify( + state, + formula, + path ) :
%% Verify whether the formula holds for this state (which we reached by this
%% path).
%% (The formula is our negated thesis, so we are looking for one path.)

verify( S, F, Path ) :-
        once( rewrite( F, S, NF ) ),
        (
            NF = true
        ->
            true
        ;
            NF = false
        ->
            fail
        ;
            (
                member( pair( S, F ), Path )
            ->
                (
                    disjunct( g _, F )
                ->
                    true
                ;
                    fail
                )
            ;

                trans( S, NS ),
                verify( NS, NF, [ pair( S, F ) | Path ] )
            )
        ).


%% disjunct( +- disjunct, + formula ):
%% Like member, only of an outermost disjunction rather than a list.

disjunct( A, A v _ ).
disjunct( A, _ v B ) :-  disjunct( A, B ).
disjunct( A, A     ).


%% rewrite( + formula, + state, - new formula ):

rewrite( f A  , S, NF ) :-  rewrite( A, S, NA ), simplifyI( NA v f A, S, NF ).
rewrite( g A  , S, NF ) :-  rewrite( A, S, NA ), simplifyI( NA ^ g A, S, NF ).
rewrite( A ^ B, S, NF ) :-  rewrite( A, S, NA ), rewrite( B, S, NB ),
                            simplifyI( NA ^ NB, S, NF ).
rewrite( A v B, S, NF ) :-  rewrite( A, S, NA ), rewrite( B, S, NB ),
                            simplify( NA v NB, S, NF ).
rewrite( F    , S, NF ) :-  simplify( F, S, NF ).

simplifyI( F, S, NF ) :-
        simplifyIS( F, S, [], NF ).

simplifyIS( F, _, T, F  ) :-  member( F, T ).
simplifyIS( F, S, T, NF ) :-  simplify( F, S, F2 ),
                              simplifyIS( F2, S, [ F | T ], NF ).

%% simplify( + formula, + state,- new formula ):
%% The subformulae have now been rewritten.
%%
%% NOTE: Need also rules for until and release!  <<<<<<<<<<<<

simplify( ~P, S, true  ) :-  proposition( P ),  \+ holds( S, P ).
simplify( ~P, S, false ) :-  proposition( P ),     holds( S, P ).

simplify( P, S, true  ) :-  proposition( P ),     holds( S, P ).
simplify( P, S, false ) :-  proposition( P ),  \+ holds( S, P ).

simplify( false ^ _    , _, false ).
simplify( _     ^ false, _, false ).
simplify( true  ^ F    , _, F     ).
simplify( F     ^ true , _, F     ).
simplify( A     ^ A    , _, A     ).
simplify( A ^ A ^ F    , _, A ^ F ).

simplify( true  v _    , _, true  ).
simplify( _     v true , _, true  ).
simplify( false v F    , _, F     ).
simplify( F     v false, _, F     ).
simplify( A     v A    , _, A     ).
simplify( A v A v F    , _, A v F ).

simplify( F            , _, F     ).
