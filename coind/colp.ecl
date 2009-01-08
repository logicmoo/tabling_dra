%%%                                                                      %%%
%%%  A meta-interpreter for co-logic programming.                        %%%
%%%  Based on "colp.pro" by Luke Evans Simon.                            %%%
%%%  Written by Feliks Kluzniak at UTD.                                  %%%
%%%                                                                      %%%

%%% NOTE:
%%%
%%%    1. To load a program use the query:
%%%           ?- prog( filename ).
%%%       If the filename has no extension, ".clp" is added.
%%%
%%%       A coinductive predicate should be declared as such in the program
%%%       file, e.g.,
%%%           :- coinductive comember/2 .
%%%
%%%       To include files use the usual Prolog syntax:
%%            :- [ file1, file2, ... ].
%%%
%%%    2. The program should contain no other directives. It may, however,
%%%       contain queries, which will be executed immediately upon reading.
%%%
%%%    3. If the program invokes a built-in predicate, that predicate must
%%%       be declared in the table builtin/1 below.

%%% LIMITATIONS: - The interpreted program should not contain cuts or
%%%                occurrences of the if-then-else construct.
%%%              - Error detection is quite rudimentary.


:- [ '../general/top_level.ecl' ].



%%%%%  Built-in predicates  %%%%
%%
%%  NOTE: Just adding "!" or " _ -> _ ; _" won't do the trick, the main
%%        metainterpreter would have to be modified.

builtin( true      ).
builtin( false     ).
builtin( fail      ).
builtin( \+( _ )   ).
builtin( _ = _     ).
builtin( _ \= _    ).
builtin( once( _ ) ).   % there is special treatment for this, see
                        % solve_builtin_call/2 below.



%%%%%  Administration  %%%%%

:- dynamic coinductive/1 .         % e.g., coinductive( comember( _, _ ) ).
:- dynamic known/2 .               % known( p, k ). if p/k has already been seen

:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."


default_extension( ".clp" ).       % default extension for file names


%% initialise:
%% Get rid of previous state.

initialise :-
        retractall( known( _, _ )    ),
        retractall( coinductive( _ ) ).


%% The legal directives (check external form only).

legal_directive( coinductive _ ).


%% process_directive( + directive ):
%% Check and process the legal directives.

process_directive( coinductive P / K ) :-          % declaration of coinductive
        (atom( P ), integer( K ), K >= 0),         %  seems OK
        !,
        mk_pattern( P, K, Pattern ),               % Pattern = P( _, _, ... )
        assert( coinductive( Pattern ) ).

process_directive( coinductive P / K ) :-          % declaration of coinductive
        (\+ atom( P ) ; \+ integer( K ) ; K < 0),  %  obviously wrong
        !,
        write( error, 'Erroneous directive: \"' ),
        write( error, (:- coinductive P / K) ),
        writeln( error, '\"' ).


%% mk_pattern( + an atom representing the name of a predicate,
%%             + an integer representing the arity of the predicate,
%%             - the most general pattern that matches all invocations of the
%%               predicate
%%           )
%% Given p/k, produce p( _, _, ... _ )  (of arity k)

mk_pattern( P, K, Pattern ) :-
        length( Args, K ),                           % Args = K fresh variables
        Pattern =.. [ P | Args ].


%% ensure_dynamic( + clause ):
%% Make sure the predicate of this clause is dynamic.
%% known/2 is used to avoid multiple declarations (not that it matters...)

ensure_dynamic( Clause ) :-
        ( Clause = (Hd :- _ ) ;  Hd = Clause ),                   % get the head
        functor( Hd, PredicateSymbol, Arity ),
        \+ known( PredicateSymbol, Arity ),
        assert( known( PredicateSymbol, Arity ) ),
        dynamic( PredicateSymbol / Arity ),
        fail.

ensure_dynamic( _ ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%  The meta-interpreter  %%%%%


%% query( + goal ):
%% Execute a query.

query( Goal ) :-
        solve( [], Goal ).



%% solve( + list of coinductive hypotheses, + goal ):
%% Solve a goal, given this table of co-inductive hypotheses.
%% NOTE: cut or if-then-else not supported.

% General goal:

solve( Hypotheses, ( Goal ; _ ) ) :-          % a disjunction, 1st alternative
        solve( Hypotheses, Goal ).

solve( Hypotheses, ( _ ; Goal ) ) :-          % a disjunction, 2nd alternative
        !,
        solve( Hypotheses, Goal ).

solve( Hypotheses, ( Goal1 , Goal2 ) ) :-     % a conjunction
        !,
        solve( Hypotheses, Goal1 ),
        solve( Hypotheses, Goal2 ).

solve( Hypotheses, Call ) :-                  % a single call
        Call \= ( _ ; _ ),
        Call \= ( _ , _ ),
        solve_call( Hypotheses, Call ).


% A single call:

solve_call( Hypotheses, Call ) :-                         % a built-in
        builtin( Call ),
        !,
        solve_builtin_call( Hypotheses, Call ).

solve_call( Hypotheses, Call ) :-                         % coinductive
        coinductive( Call ),
        !,
        solve_coinductive_call( Hypotheses, Call ).

solve_call( Hypotheses, Call ) :-                         % not coinductive
        \+ builtin( Call ),
        \+ coinductive( Call ),
        clause( Call, Body ),
        solve( Hypotheses, Body ).


% A single coinductive call:

solve_coinductive_call( Hypotheses, Call ) :-
            member( Call, Hypotheses ).                   % the hypotheses first

solve_coinductive_call( Hypotheses, Call ) :-
            clause( Call, Body ),
            solve( [ Call | Hypotheses ], Body ).          % then the clauses


% A single built-in call:

solve_builtin_call( Hypotheses, once( Call ) ) :-
        once( solve( Hypotheses, Call ) ).


solve_builtin_call( _, Call ) :-
        Call \= once( _ ),
        Call.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
