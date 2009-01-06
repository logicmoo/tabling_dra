%%%                                                                     %%%
%%% A meta-interpreter for co-logic programming.                        %%%
%%% Based on "colp.pro" by Luke Evans Simon.                            %%%
%%%                                                                     %%%

%%% NOTE:
%%%
%%%    1. To load a program use the query:
%%%           ?- prog( filename ).
%%%
%%%       A coinductive predicate should be declared as such in the program
%%%       file, e.g.,
%%%           :- coinductive comember/2 .
%%%
%%%    2. The program should contain no other directives. It may, however,
%%%       contain queries, which will be executed immediately upon reading.
%%%
%%%    3. If the program invokes a built-in predicate, that predicate must
%%%       be declared in the table builtin/1 below.

%%% LIMITATIONS: - The interpreted program should not contain cuts or
%%%                occurrences of the if-then-else construct.
%%%              - Error detection is quite rudimentary.



%%%%%  Built-in predicates  %%%%
%%
%%  NOTE: Just adding "!" or " _ -> _ ; _" won't do the trick, the main
%%        metainterpreter would have to be modified.

builtin( true ).
builtin( false ).
builtin( fail ).
builtin( \+( _ ) ).
builtin( _ = _ ).
builtin( _ \= _ ).
builtin( once( _ ) ).  % there is special treatment for this, see below



%%%%%  Administration  %%%%%

:-dynamic coinductive/1 .          % e.g., coinductive( comember( _, _ ) ).
:-dynamic known/2 .                % known( p, k ). if p/k has already been seen

:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."


%% Load a program from this file.

prog( Filename ) :-
        clean_up,
        open( Filename, read, ProgStream ),

        repeat,
        readvar( ProgStream, Term, VarDict ),
        % write( '<processing \"' ),  write( Term ),  writeln( '\">' ),
        treat_term( Term, VarDict ),
        Term = end_of_file,

        !,
        close( ProgStream ).


%% Get rid of previous state:

clean_up :-  retract( known( _, _ )    ),  !,  clean_up.
clean_up :-  retract( coinductive( _ ) ),  !,  clean_up.
clean_up.


%% Treat a term, which should be a directive, a program clause or end_of_file.
%% The second argument is a variable dictionary, used for printing out the
%% results of a query

treat_term( end_of_file, _ ) :-  !.                % just ignore this

treat_term( (:- coinductive P / K), _ ) :-         % declaration of coinductive
        (atom( P ), integer( K ), K >= 0),         %  seems OK
        !,
        mk_pattern( P, K, Pattern ),               % Pattern = P( _, _, ... )
        assert( coinductive( Pattern ) ).

treat_term( (:- coinductive P / K), _ ) :-         % declaration of coinductive
        (\+ atom( P ) ; \+ integer( K ) ; K < 0),  %  obviously wrong
        !,
        write( error, 'Erroeneous directive: \"' ),
        write( error, (:- coinductive P / K) ),
        writeln( error, '\"' ).

treat_term( (:- Directive), _ ) :-                 % another directive
        Directive \= (coinductive _),
        !,
        write( error, 'Unknown directive: \"' ),
        write( error, Directive ),
        writeln( error, '\"' ).

treat_term( (?- Query), VarDict ) :-
        !,
        write( 'Query: ' ),  write( Query ), writeln( '.' ),
        treat_query( Query, VarDict ).

treat_term( Clause, _ ) :-
        Clause \= end_of_file, Clause \= (:- _), Clause \= (?- _),
        ( good_head( Clause ) ; Clause = (H :- _), good_head( H ) ),
        !,
        ensure_dynamic( Clause ),
        assertz( Clause ).

treat_term( Clause, _ ) :-
        Clause \= end_of_file, Clause \= (:- _), Clause \= (?- _),
        \+ ( good_head( Clause ) ; Clause = (H :- _), good_head( H ) ),
        !,
        write( error, 'Erroeneous clause: \"' ),
        write( error, Clause ),
        writeln( error, '\"' ).


%% Treat a query, i.e., produce and display solutions until
%% no more can be found.

treat_query( Query, VarDict ) :-
        query( Query ),
        write( 'Yes: ' ),  show_results( VarDict ).

treat_query( _, _ ) :-
            writeln( 'No' ).


%% Is this term a good head of a clause?

good_head( Hd ) :-  atom( Hd ),  !.
good_head( Hd ) :-  compound( Hd ),  \+ is_list( Hd ).


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


%% Use the variable dictionary to show the results of a query.

show_results( Dict ) :-
        member( [ Name | Var ], Dict ),
        write( Name ), write( ' = ' ),  writeln( Var ),
        fail.
show_results( _ ).


%% Given p/k, produce p( _, _, ... _ )  (of arity k)

mk_pattern( P, K, Pattern ) :-
        length( Args, K ),                           % Args = K fresh variables
        Pattern =.. [ P | Args ].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%  The meta-interpreter  %%%%%


%% Execute a query.
query( Goal ) :- solve( [], Goal ).



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


% A single built-in call

solve_builtin_call( Hypotheses, once( Call ) ) :-
        once( solve( Hypotheses, Call ) ).


solve_builtin_call( _, Call ) :-
        Call \= once( _ ),
        Call.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
