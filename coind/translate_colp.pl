%%%                                                                      %%%
%%%  A translator for co-logic programming.                              %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                   %%%
%%%                                                                      %%%
%%%  Last update: 28 January 2009.                                       %%%
%%%                                                                      %%%
%%%  NOTE: Some of the code may be Eclipse-specific and may require      %%%
%%%        minor tweaking for other Prolog systems.                      %%%


%%% General description
%%% -------------------
%%%
%%% This translator transforms "co-logic programming" programs into
%%% straightforward logic programs that can be compiled/executed in any
%%% logic programming system that does not incorporate the "occur check"
%%% in normal unification.
%%%
%%% The transformed programs may cause errors if they invoke built-in predicates
%%% that cannot handle cyclic terms (e.g., copy_term/2).
%%%
%%% NOTE: A transformed program cannot contain variable literals or invocations
%%%       of "call/1".
%%%       A transformed program cannot contain negative literals, unless they 
%%%       are invocations of predicates that were declared as "bottom".
%%%       (It could be done, but the machinery would have to be much heavier.)
%%%


%%% Usage
%%% -----
%%%
%%% To translate a program contained in file "filename.clp", invoke
%%%
%%%    tc( filename ).
%%%
%%% (Notice that one does not write down the extension ".clp".)
%%% The translator will read the program and --- if there are no fatal
%%% errors --- will write the transformed program on "filename.pl".
%%%
%%% NOTE: If "filename.pl" exists, it will be overwritten without warning.
%%%
%%% NOTE: A program that works in the coinductive interpreter should work almost
%%%       in the same fashion after it is translated and loaded directly into
%%%       Eclipse. The only difference should be that the interpreter executes
%%%       queries somewhat differently (more or less as if they were entered
%%%       interactively at the top level).
%%%       Unlike the interpreter, the translator does not act upon directives
%%%       that include files (e.g., ":- [ filename ].": such directives are
%%%       simply copied to output.  This is done on purpose, to allow separate
%%%       translation: if the file names are given without extensions, and all
%%%       the included files are also translated, then the effect of directives
%%%       in the translated program will be to include the translated files
%%%       (because the default extension will be different when loaded into
%%%       Eclipse).
%%%       There is, however, one special directive that must be recognized if we
%%%       are to run programs that take advantage of both coinduction and
%%%       tabling: namely ":- tabled ... ".  So the translator automatically 
%%%       transforms the predicate specifcations in such directives (unless they
%%%       refer to predicates declared as "bottom").


%%% Directives
%%% ----------
%%%
%%% Queries will undergo the same transformation as clause bodies and be output
%%% with the translated program.
%%% In general, directives will be just copied to the translated program (i.e.,
%%% without transformation. However, there are two kinds of exceptions.  The
%%% first is ":- tabled...", discussed above.  The second is that the following 
%%% directives will be interpreted directly by the translator (and not copied):
%%%
%%% 1.
%%%     :- coinductive PredSpec .
%%%
%%%  (where PredSpec is of the form "p/k", or "p/k, q/k, ...": the item that
%%%   precedes the slash is the name of a predicate, and the item that follows
%%%   the slash is a natural number denoting the arity of the predicate).
%%%
%%%  These directives are treated as declarations of coinductive predicates.
%%%
%%% 2.
%%%     :- top PredSpec.
%%%
%%%  These are treated as declarations of predicates that will be invoked
%%%  directly by the user or non-coinductive programs.  In the translated
%%%  program these predicates will be available also in their original form
%%%  (i.e., there will be no need to take the transformation into account by
%%%  providing an additional argument: see the description of the transformation
%%%  below).
%%%
%%% 3.
%%%     :- bottom PredSpec.
%%%
%%%  These are treated as declarations of predicates that should be treated as
%%%  "normal" logic programming predicates, and should, therefore, not be
%%%  subjected to transformation.  A declaration of this sort constitutes a
%%%  claim by the programmer that the "bottom" predicate will not invoke ---
%%%  directly or indirectly --- any coinductive predicates.
%%%
%%%  NOTE: 1. It is an error for a coinductive predicate to be declared as
%%%           "bottom".
%%%        2. It is not necessary to declare built-in predicates in this
%%%           fashion.
%%%
%%%
%%% Please note that the various declarations of predicates should precede their
%%% definitions.  (It is a fatal error if they do not.)


%%% The transformation
%%% ------------------
%%%
%%% A. Every "normal" predicate (i.e., one that has not been declared as
%%%    "coinductive" or "bottom") will:
%%%
%%%      1. have its name extended with an underscore;
%%%
%%%      2. receive an additional argument (at the end of the argument list):
%%%         the argument will carry around a data structure that contains the
%%%         set of coinductive hypotheses (in the first version the data
%%%         structure is just a list).
%%%
%%%    For example, the predicate
%%%
%%%            p( X, Y ) :-  q( X, Z ),  r( Z, Y ).
%%%            p( X, X ).
%%%
%%%    will be transformed to
%%%
%%%            p_( X, Y, Hyp ) :-  q_( X, Z, Hyp ),  r_( Z, Y, Hyp ).
%%%            p_( X, X, _   ).
%%%
%%%
%%% B. If a predicate has been declared as "top", an additional hook will be
%%%    provided to facilitate invocation.  For example, if the definition of
%%%    p/2 above were preceded by
%%%
%%%           :- top p/2.
%%%
%%%    then the translator would output also
%%%
%%%           p( X, Y ) :-  p_( X, Y, [] ).
%%%
%%%    N.B. This feature is the reason predicate names are extended with an
%%%         undescore.  Our example program might well contain also occurrences
%%%         of p/1, which --- if the names were not changed --- would get
%%%         transformed to occurrences of p/2, and those would clash with this
%%%         additional "convenience predicate".
%%%
%%%
%%% C. If a predicate has been declared as "bottom", none of its occurrences is
%%%    transformed.  (This applies also to built-in predicates, with the obvious
%%%    exception of "meta-predicates" such as ",/2", "once/1" etc., some of
%%%    whose arguments are interpreted as predicates and must undergo the
%%%    transformation.)
%%%
%%%
%%% D. If a predicate has been declared as "coinductive", the transformation
%%%    is a little more involved:
%%%
%%%      1. the definition will be preceded by an additional clause that
%%%         nondeterministically queries the set of coinductive hypotheses;
%%%
%%%      2. the other clauses will extend the set of coinductive hypotheses
%%%         with an appropriate instantiation of the the goal that invoked them
%%%         (in the original form of the program).
%%%
%%%    For example, if the definition of p/2 above were preceded by
%%%
%%%           :- coinductive p/2.
%%%
%%%    then the transformed form would be (assuming the set of hypotheses is
%%%    represented as a list):
%%%
%%%
%%%           p_( X, Y, Hyp ) :-
%%%                   member( p( X, Y ), Hyp ).
%%%
%%%           p_( X, Y, Hyp ) :-
%%%                   NewHyp = [ p( X, Y ) | Hyp ],
%%%                   q_( X, Z, NewHyp ),
%%%                   r_( Z, Y, NewHyp ).
%%%
%%%           p_( X, X, _ ).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded( '../general/utilities' ).



%-----  Wrapper  -----


%% tc( + filename ):
%% Open "filename.clp". If successful, translate its contents, producing output
%% on "filename.pl".

tc( FileName ) :-
        open_streams( FileName, InputStream, OutputStream ),

        translate( InputStream, OutputStream ),

        close( InputStream  ),
        close( OutputStream ).

%
:- mode open_streams( +, -, - ).

open_streams( RootFileName, InputStream, OutputStream ) :-
        ensure_filename_is_an_atom( RootFileName ),
        name( RootFileName, RootFileNameChars ),
        open_file( RootFileNameChars, ".clp", read , InputStream  ),
        open_file( RootFileNameChars, ".pl" , write, OutputStream ).



%% translate( + input stream, + output stream ):
%% Translate the program on the input stream, writing the translation onto
%% the output stream.
%%
%% For greater flexibility (though not necessarily efficiency) we read all the
%% terms from the input stream to memory first, then process them in memory,
%% then write them out.

:- mode translate( +, + ).

translate( InputStream, OutputStream ) :-
        read_terms( InputStream, Terms ),
        verify_program( Terms ),
        initialise_tables,
        transform( Terms, '', ProcessedTerms ),
        write_declarations_as_comments( OutputStream ),
        write_top_predicates( OutputStream ),
        write_clauses( ProcessedTerms, OutputStream ).




%-----  The main translator  -----


:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."
:- op( 1000, fy, bottom ).         % allow  ":- bottom p/k ."
:- op( 1000, fy, top ).            % allow  ":- top p/k ."

:- op( 1000, fy, tabled ).         % allow also  ":- tabled p/k ."


%% A translator directive will be remembered in a dedicated table,
%% e.g., ":- coinductive p/2" as "coinductive( p( _, _ ) )".
%%
%% Moreover, the table "defined" will contain the name of each predicate the
%% processing of whose definition has been started (and possibly finished).

:- dynamic coinductive/1.
:- dynamic bottom/1.
:- dynamic top/1.
:- dynamic defined/1.


%% initialise_tables:
%% Initialise the translator (make sure that no garbage is left from a previous
%% invocation).

initialise_tables :-
        retractall( coinductive( _ ) ),
        retractall( bottom( _ )      ),
        retractall( top( _ )         ),
        retractall( defined( _ )     ).



%% Write_declarations_as_comments( + output stream ):
%% Output comments that list the declarations of coinductive and bottom
%% predicates.

:- mode write_declarations_as_comments( + ).

write_declarations_as_comments( OutputStream ) :-
        writeln( OutputStream, '%% COINDUCTIVE PREDICATES:' ),
        coinductive( Pattern ),                   % i.e., sequence through these
        write_pred_spec_comment( Pattern, OutputStream ),
        fail.

write_declarations_as_comments( OutputStream ) :-
        writeln( OutputStream, '%% \"BOTTOM\" PREDICATES:' ),
        bottom( Pattern ),                        % i.e., sequence through these
        write_pred_spec_comment( Pattern, OutputStream ),
        fail.

write_declarations_as_comments( OutputStream ) :-
        nl( OutputStream ).

%
:- mode write_pred_spec_comment( +, + ).

write_pred_spec_comment( Pattern, OutputStream ) :-
        functor( Pattern, P, K ),
        write(   OutputStream, '%%   ' ),
        writeln( OutputStream, P / K ).



%% write_top_predicates( + output stream ).
%% Output the top predicates.

:- mode write_top_predicates( + ).

write_top_predicates( OutputStream ) :-
        top( Pattern ),                           % i.e., sequence through these
        Pattern =.. [ F | Args ],
        transform_predicate_name( F, NF ),
        once( append( Args, [ [] ], ExtendedArgs ) ),
        Call =.. [ NF | ExtendedArgs ],
        write_clause( (Pattern :- Call), OutputStream ),
        fail.

write_top_predicates( OutputStream ) :-
        nl( OutputStream ).




%% transform( + list of terms,
%%            + most general instance of the current predicate,
%%            - list of processed terms
%%          ):
%% Process the terms, i.e., interpret translator directives and transform
%% other directives, queries and clauses.
%% Information about the current predicate is provided to assist in the
%% detection of the first clause of a coinductive predicate, which requires
%% special treatment; it also helps detect non-contiguous predicate definitions.

:- mode transform( +, +, - ).

transform( [], _, [] ) :-
        !.

transform( [ (:- tabled PredSpecs) | Terms ], 
           _, 
           [(:- tabled NewPredSpecs) | NewTerms ]
         ) :-
        transform_pred_specs( PredSpecs, NewPredSpecs ),
        transform( Terms, '', NewTerms ).

transform( [ (:- Directive) | Terms ], _, NewTerms ) :-
        is_a_translator_directive( Directive ),
        !,
        process_translator_directive( Directive ),
        transform( Terms, '', NewTerms ).

transform( [ (:- Directive) | Terms ], _, [ (:- Directive) | NewTerms ] ) :-
        !,
        transform( Terms, '', NewTerms ).

transform( [ (?- Query) | Terms ], _, [ (?- NewQuery) | NewTerms ] ) :-
        !,
        transform_body( Query, [], NewQuery ),
        transform( Terms, '', NewTerms ).

transform( [ Clause | Terms ], CurrentPred,
           [ ExtraClause, NewClause | NewTerms ]
         ) :-
        get_clause_head( Clause, Head ),
        Head \= CurrentPred,
        coinductive( Head ),             % beginning a new coinductive predicate
        !,
        most_general_instance( Head, Pattern ),
        starting_new_predicate( Pattern ),
        transform_logical_atom( Pattern, HypVar, NewPattern ),
        ExtraClause = (NewPattern :- member( Pattern, HypVar )),
        transform_clause( Clause, NewClause ),
        transform( Terms, Pattern, NewTerms ).

transform( [ Clause | Terms ], CurrentPred, [ NewClause | NewTerms ] ) :-
        get_clause_head( Clause, Head ),
        Head \= CurrentPred,
        % \+ coinductive( Head ),        % beginning a new "normal" predicate
        !,
        most_general_instance( Head, Pattern ),
        starting_new_predicate( Pattern ),
        transform_clause( Clause, NewClause ),
        transform( Terms, Pattern, NewTerms ).

transform( [ Clause | Terms ], CurrentPred, [ NewClause | NewTerms ] ) :-
        % get_clause_head( Clause, Head ),
        % check( Head = CurrentPred ),              % i.e., not the first clause
        !,
        transform_clause( Clause, NewClause ),
        transform( Terms, CurrentPred, NewTerms ).



%% starting_new_predicate( + most general instance of a predicate ):
%% We are beginning to work on the definition of this predicate.
%% Check that there is no contiguity error, mark the predicate as defined.

:- mode starting_new_predicate( + ).

starting_new_predicate( Pattern ) :-
        check_contiguity( Pattern ),
        assert( defined( Pattern ) ).


%% check_contiguity( + head of a clause ):
%% We have encountered what seems to be the first clause of a predicate.
%% If this predicate has already been defined, raise a fatal error.

:- mode check_contiguity( + ).

check_contiguity( Head ) :-
        defined( Head ),
        !,
        functor( Head, P, K ),
        error( [ 'Clauses for predicate ', P/K, ' are not contiguous' ] ).

check_contiguity( _ ).




%% transform_pred_specs( + predicate specifications, - ditto transformed ):
%% Transform each p/k into p_/(k+1), unless p/k is declared as "bottom".

transform_pred_specs( (PredSpec , PredSpecs), (NewPredSpec , NewPredSpecs) ) :-
        !,
        transform_pred_spec( PredSpec, NewPredSpec ),
        transform_pred_specs( PredSpecs, NewPredSpecs ).

transform_pred_specs( PredSpec, NewPredSpec ) :-
        transform_pred_spec( PredSpec, NewPredSpec ).

%
transform_pred_spec( PredSpec, PredSpec ) :-
        predspec_to_pattern( PredSpec, Pattern ),      % also checks correctness
        bottom( Pattern ),
        !.

transform_pred_spec( P / K, NP / K1 ) :-
        transform_predicate_name( P, NP ),
        K1 is K + 1.


%% transform_clause( + clause, - transformed clause ):
%% Transform a clause.
%% See the main comment for a description of the transformation.

:- mode transform_clause( +, - ).

transform_clause( (Head :- Body), NewClause ) :-
        coinductive( Head ),
        !,
        transform_logical_atom( Head, HypVar, NewHead ),
        transform_body( Body, NewHypVar, NewBody ),
        NewClause = (NewHead :- NewHypVar = [ Head | HypVar ], NewBody).

transform_clause( (Head :- Body), (NewHead :- NewBody) ) :-
        % \+ coinductive( Head ),
        !,
        transform_logical_atom( Head, HypVar, NewHead ),
        transform_body( Body, HypVar, NewBody ).

transform_clause( Fact, NewFact ) :-
        % Fact \= (_ :- _),
        transform_logical_atom( Fact, _, NewFact ).



%% transform_body( + body, + set of hypotheses, - new body ):
%% Transform the body of a clause.
%% NOTE: The set of hypotheses may be a variable that will be instantiated only
%%       when the transformed program is run.


transform_body( Var ) :-
        !,
        error( [ 'Variable literal: \"', Var, '\"' ] ).

transform_body( (Calls1 , Calls2), HypVar, (NewCalls1 , NewCalls2) ) :-
        !,
        transform_body( Calls1, HypVar, NewCalls1 ),
        transform_body( Calls2, HypVar, NewCalls2 ).

transform_body( (Calls1 ; Calls2), HypVar, (NewCalls1 ; NewCalls2) ) :-
        !,
        transform_body( Calls1, HypVar, NewCalls1 ),
        transform_body( Calls2, HypVar, NewCalls2 ).

transform_body( (If -> Then ; Else), HypVar, (NewIf -> NewThen ; NewElse) ) :-
        !,
        transform_body( If, HypVar, NewIf ),
        transform_body( Then, HypVar, NewThen ),
        transform_body( Else, HypVar, NewElse ).

transform_body( (If -> Then), HypVar, (NewIf -> NewThen) ) :-
        !,
        transform_body( If, HypVar, NewIf ),
        transform_body( Then, HypVar, NewThen ).


transform_body( Call, HypVar, NewCall ) :-
        transform_logical_atom( Call, HypVar, NewCall ).



%% transform_logical_atom( + logical atom,
%%                         + variable with set of hypotheses,
%%                         - transformed atom
%%                       ):
%% Transform this head or simple call.

:- mode transform_logical_atom( +, ?, - ).

transform_logical_atom( \+ C, _, \+ C ) :-
        bottom( C ),
        !.

transform_logical_atom( \+ C, _, _ ) :-
        !,
        error( [ 'Negative literal: \"', \+ C, '\"' ] ).

transform_logical_atom( call( C ), _, _ ) :-
        !,
        error( [ 'Invocation of \"call/1\": \"', call( C ), '\"' ] ).

transform_logical_atom( once( Calls ), Hyp, once( NewCalls ) ) :-
        !,
        transform_body( Calls, Hyp, NewCalls ).

transform_logical_atom( Pred, _, Pred ) :-
        (bottom( Pred ) ; is_builtin( Pred )),
        !.

transform_logical_atom( Pred, HypVar, NewPred ) :-
        % \+ (bottom( Pred ) ; is_built_in( Pred )),
        Pred =.. [ Name | Args ],
        transform_predicate_name( Name, NewName ),
        once( append( Args, [ HypVar ], NewArgs ) ),
        NewPred =.. [ NewName | NewArgs ].


%%
is_builtin( Pred ) :-
        predicate_property( Pred, built_in ).


%% transform_predicate_name( + name, - new name ):
%% Transform the name of a predicate (by extending it with "_").

:- mode transform_predicate_name( +, - ).

transform_predicate_name( Name, NewName ) :-
        name( Name, NameChars ),
        once( append( NameChars, "_", NewNameChars ) ),
        name( NewName, NewNameChars ).




%%%%%  Translator directives


%% is_a_translator_directive( + directive ):
%% Is this one of the directives that are interpreted by the translator?

:- mode is_a_translator_directive( + ).

is_a_translator_directive( (coinductive _) ).
is_a_translator_directive( (bottom      _) ).
is_a_translator_directive( (top         _) ).



%% process_translator_directive( + directive ):
%% Interpret a translator directive.

:-  mode process_translator_directive( + ).

process_translator_directive( (coinductive PredSpecs) ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        declare_coinductive( Patterns ).

process_translator_directive( (bottom PredSpecs) ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        declare_bottom( Patterns ).

process_translator_directive( (top PredSpecs) ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        declare_top( Patterns ).



%% declare_coinductive( + list of general instances ):
%% Store the general instances in "coinductive", warning about duplications.
%% An overlap with "bottom" is a fatal error.

declare_coinductive( Patterns ) :-
        member( Pattern, Patterns ),              % i.e., sequence through these
        check_declaration_order( Pattern, 'coinductive' ),
        (
            bottom( Pattern )
        ->
            overlap_error( Pattern )
        ;
            true
        ),
        (
            coinductive( Pattern )
        ->
            duplicate_warning( Pattern, 'coinductive' )
        ;
            assert( coinductive( Pattern ) )
        ),
        fail.

declare_coinductive( _ ).


%% declare_bottom( + list of general instances ):
%% Store the general instances in "bottom", warning about duplications.
%% An overlap with "coinductive" is a fatal error.

declare_bottom( Patterns ) :-
        member( Pattern, Patterns ),              % i.e., sequence through these
        check_declaration_order( Pattern, 'bottom' ),
        (
            coinductive( Pattern )
        ->
            overlap_error( Pattern )
        ;
            true
        ),
        (
            bottom( Pattern )
        ->
            duplicate_warning( Pattern, 'bottom' )
        ;
            assert( bottom( Pattern ) )
        ),
        fail.

declare_bottom( _ ).


%% declare_top( + list of general instances ):
%% Store the general instances in "top", warning about duplications.

declare_top( Patterns ) :-
        member( Pattern, Patterns ),             % i.e., sequence through these
        check_declaration_order( Pattern, 'top' ),
        (
            top( Pattern )
        ->
            duplicate_warning( Pattern, 'top' )
        ;
            assert( top( Pattern ) )
        ),
        fail.

declare_top( _ ).


%% duplicate_warning( + most general instance of a predicate,
%%                    + kind of declaration
%%                  ):
%% The predicate has been declared twice for this kind: raise a warning.

:- mode duplicate_warning( +, + ).

duplicate_warning( Pattern, Kind ) :-
            functor( Pattern, P, K ),
            warning( [ 'Duplicate declaration of ', P / K,
                       ' as a \"', Kind, '\" predicate'
                     ]
                   ).


%% overlap_error( + most general instance of a predicate ):
%% The predicate has been declared as both "coinductive" and "bottom":
%% raise a fatal error.

:- mode overlap_error( + ).

overlap_error( Pattern ) :-
            functor( Pattern, P, K ),
            error( [ P/K, ' declared both as \"coinductive\" and as \"bottom\"'
                   ]
                 ).


%% check_declaration_order( + the most general instance of a predicate,
%%                          + kind of declaration
%%                        ):
%% Raise a fatal error if the definition of this predicate has already been
%% seen.

:- mode check_declaration_order( +, + ).

check_declaration_order( Pattern, Kind ) :-
        defined( Pattern ),
        !,
        functor( Pattern, P, K ),
        error( [ P/K, ' declared as \"', Kind, '\" after it has been defined' ]
             ).

check_declaration_order( _, _ ).

%-------------------------------------------------------------------------------
