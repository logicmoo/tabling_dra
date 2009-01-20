%%%                                                                      %%%
%%%  A translator for co-logic programming.                              %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                   %%%
%%%                                                                      %%%


%%% General description
%%% -------------------
%%%
%%% This translator transforms co-logic programming programs into
%%% straightforward logic programming programs that can be compiled/executed
%%% in any logic programming system that does not incorporate occur check.
%%%
%%% The transformed programs may cause errors if they invoke built-in predicates
%%% that cannot handle cyclic terms (e.g., copy_term/2).


%%% Usage
%%% -----
%%%
%%% To translate a program contained in file "filename.clp", invoke
%%%
%%%    tc( filename ).
%%%
%%% (Notice that one does not write down the extension ".clp".)
%%% The translator will read the program and --- if there are no fatal
%%% errors --- will write the transformed program on "filename.ecl".
%%
%%% NOTE: If "filename.ecl" exists, it will be overwritten without warning.


%%% Directives
%%% ----------
%%%
%%% Queries and directives will undergo suitable transformation and be output
%%% with the translated program.  However, the following directives will be
%%% interpreted directly by the translator (and not written to the output):
%%%
%%% 1.
%%%     :- coinductive PredSpec .
%%%
%%%  (where PredSpec is of the form "p/k", or "p/k, q/k, ...": the item that
%%%   precedes the slash is the name of a predicate, and the item that follows
%%%   the slash is a natural number denoting the predicate's arity).
%%%
%%%  These directives are treated as declarations of coinductive predicates.
%%%
%%% 2.
%%%     :- top PredSpec.
%%%
%%%  These are treated as declarations of predicates that will be invoked
%%%  directly by the user or non-coinductive programs.  In the translated form,
%%%  these predicates will be available also in their original form (i.e., there
%%%  will be no need to take the transformation into account by providing an
%%%  additional argument, see the description of the transformation below).
%%%
%%% 3.
%%%     :- bottom PredSpec.
%%%
%%%  These are treated as declarations of predicates that should be treated as
%%%  "normal" logic programming predicates, and should not therefore undergo
%%%  transformation.  A declaration of this sort constitues a claim by the
%%%  programmer that the "bottom" predicate will not invoke --- directly or
%%%  indirectly --- any coinductive predicates.
%%%  If any of the "bottom" predicates are defined in the translated file,
%%%  the translator will actually verify this claim.
%%%
%%%  NOTE: 1. It is an error for a coinductive predicate to be declared as
%%%           "bottom".
%%%        2. It is not necessary to declare built-in predicates in this
%%%           fashion.
%%%
%%%
%%% Please note that the declarations of predicates should precede their
%%% definitions.  (In this version of the translator a declaration that follows
%%% a definition will work anyway, but a warning will be given.)


%%% The transformation
%%% ------------------
%%%
%%% A. Every "normal" predicate (i.e., one that has not been declared as
%%%    "coinductive", "top" or "bottom") will:
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
%%% B. If a predicate has been declared as "top", an additional "hook" will be
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
%%%         transformed to occurrences of p/2 that would clash with this
%%%         additional "convenience predicate".
%%%
%%%
%%% C. If a predicate has been declared as "bottom", none of its occurrences is
%%%    transformed.  (This applies also to built-in predicates, with the obvious
%%%    exception of "meta-predicates" such as ",/2", "once/1" etc., some of
%%%    whose arguments are interpreted as predicates and must undergo the
%%%    transformation).
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


% :- module( translate_colp ).

% :- export tc/1.   % i.e., translate_colp( + filename )


:- ensure_loaded( '../general/utilities' ).



%-----  Wrapper  -----


%% tc( + filename ):
%% Open "filename.clp". If successful, translate its contents, producing output
%% on "filename.ecl".

tc( FileName ) :-
        open_streams( FileName, InputStream, OutputStream ),

        translate( InputStream, OutputStream ),

        close( InputStream  ),
        close( OutputStream ).

%
:- mode open_streams( +, -, - ).

open_streams( FileName, InputStream, OutputStream ) :-
        ensure_filename_is_an_atom( FileName ),
        atom_string( FileName, FileNameString ),
        concat_strings( FileNameString, ".clp", InputFileName ),
        concat_strings( FileNameString, ".ecl", OutputFileName ),
        open( InputFileName , read , InputStream  ),
        open( OutputFileName, write, OutputStream ).



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
        initialise_tables,
        process_terms( Terms, '', ProcessedTerms ),
        write_terms( ProcessedTerms, OutputStream ).




%-----  The main translator  -----


:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."
:- op( 1000, fy, top ).            % allow  ":- top p/k ."
:- op( 1000, fy, bottom ).         % allow  ":- bottom p/k ."


%% A translator directive will be remembered in a dedicated table,
%% e.g., ":- coinductive p/2" as "coinductive( p( _, _ ) ).
%%
%% Moreover, information about each predicate the processing of whose definition
%% has been started (and possibly finished) will be stored in "defined".

:- dynamic coinductive/1.
:- dynamic bottom/1.
:- dynamic top/1.
:- dynamic defined/1.


%% initialise_tables:
%% Initialise the translator (make sure that no garbage is left from a previous
%% invocation.

initialise_tables :-
        retractall( coinductive( _ ) ),
        retractall( bottom( _ )      ),
        retractall( top( _ )         ),
        retractall( defined( _ )     ).




%% process_terms( + list of terms,
%%                + most general instance of the current predicate,
%%                - list of processed terms
%%              ):
%% Process the terms, i.e., interpret translator directives and transform
%% other directives, queries and clauses.
%% Information about the current predicate is provided to assist in the
%% detection of the first clause of a coinductive predicate, which requires
%% special treatment; it also helps detect non-contiguous predicate definitions.

%% >>> THIS ERROR CHECKING SHOULD BE FACTORED OUT TO A GENERAL ROUTINE
%% >>> FOR VERIFYING THAT A LIST OF TERMS REPRESENTS A PROGRAM.

process_terms( [], _, [] ) :-
        !.

process_terms( [ Var | _ ], _, _ ) :-
        var( Var ),
        !,
        error( [ "A variable clause: \"", Var, "\"" ] ).

process_terms( [ (:- Var) | _ ], _, _ ) :-
        var( Var ),
        !,
        error( [ "A variable directive: \"", (:- Var), "\"" ] ).

process_terms( [ (?- Var) | _ ], _, _ ) :-
        var( Var ),
        !,
        error( [ "A variable query: \"", (?- Var), "\"" ] ).

process_terms( [ (Var :- Body) | _ ], _, _ ) :-
        var( Var ),
        !,
        error( [ "A variable clause head: \"", (Var :- Body), "\"" ] ).

process_terms( [ (:- Directive) | Terms ], _, NewTerms ) :-
        is_a_translator_directive( Directive ),
        !,
        process_translator_directive( Directive ),
        process_terms( Terms, '', NewTerms ).

process_terms( [ Term | Terms ], CurrentPred, [ NewTerm | NewTerms ] ) :-
        process_term( Term, CurrentPred, NewTerm, NewCurrentPred ),
        process_terms( Terms, NewCurrentPred, NewTerms ).



%% is_a_translator_directive( + directive ):
%% Is this one of the directives that are interpreted by the translator?

:- mode is_a_translator_directive( + ).
is_a_translator_directive( coinductive _ ).
is_a_translator_directive( bottom      _ ).
is_a_translator_directive( top         _ ).



%% process_translator_directive( + directive ):
%% Interpret a translator directive.

:-  mode process_translator_directive( + ).

process_translator_directive( coinductive PredSpecs ) :-
        check_predspecs( PredSpecs, PredSpecList ),
        declare_coinductive( PredSpecList ).

process_translator_directive( bottom PredSpecs ) :-
        check_predspecs( PredSpecs ),
        declare_bottom( PredSpecs ).

process_translator_directive( top PredSpecs ) :-
        check_predspecs( PredSpecs ),
        declare_top( PredSpecs ).



%% check_predspecs( + a conjunction of predicate specification (or just one),
%%                  - list of predicate specifications
%%                ):
%% Given one or several predicate specifications (in the form "p/k" or
%% "p/k, q/l, ...") check whether they are well-formed: if not, raise a fatal
%% error; otherwise return a list of the most general instances that correspond
%% to the predicate specifications.

check_predspecs( Var, _ ) :-
        var( Var ),
        !,
        error( [ "A variable instead of predicate specifications: \", ",
                 Var,
                 "\""
               ]
             ).

check_predspecs( (PredSpec , PredSpecs), [ Pattern | Patterns ] ) :-
        !,
        check_predspec( PredSpec, Pattern ),
        check_predspecs( PredSpecs, Patterns ).

check_predspecs( PredSpec, [ Pattern ] ) :-
        check_predspec( PredSpec, Pattern ).


%%
check_predspec( Var, _ ) :-
        var( Var ),
        !,
        error( [ "A variable instead of a predicate specification: \", ",
                 Var,
                 "\""
               ]
             ).

check_predspec( P / K, Pattern ) :-
        atom( P ),
        integer( K ),
        K >= 0,
        !,
        mk_pattern( P, K, Pattern ).

check_predspec( PredSpec, _ ) :-
        error( [ "An incorrect predicate specification: \"", PredSpec, "\"" ] ).



%% declare_coinductive( + list of general instances ):
%% Store the general instances in "coinductive", warning about duplications.
%% An overlap with bottom is a fatal error.

declare_coinductive( Patterns ) :-
        member( Pattern, Patterns ),
        check_declaration_order( Pattern, "coinductive" ),
        (
            coinductive( Pattern )
        ->
            duplicate_warning( Pattern, "coinductive" )
        ;
            bottom( Pattern )
        ->
            overlap_error( Pattern )
        ;
            assert( coinductive( Pattern ) )
        ),
        fail.

declare_coinductive( _ ).


%% declare_bottom( + list of general instances ):
%% Store the general instances in "bottom", warning about duplications.
%% An overlap with coinductive is a fatal error.

declare_bottom( Patterns ) :-
        member( Pattern, Patterns ),
        check_declaration_order( Pattern, "bottom" ),
        (
            bottom( Pattern )
        ->
            duplicate_warning( Pattern, "bottom" )
        ;
            coinductive( Pattern )
        ->
            overlap_error( Pattern )
        ;
            assert( bottom( Pattern ) )
        ),
        fail.

declare_bottom( _ ).


%% declare_top( + list of general instances ):
%% Store the general instances in "top", warning about duplications.

declare_top( Patterns ) :-
        member( Pattern, Patterns ),
        check_declaration_order( Pattern, "top" ),
        (
            top( Pattern )
        ->
            functor( Pattern, P, K ),
            warning( [ "Duplicate declaration of ", P / K, " as a top predicate"
                     ]
                   )
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
            warning( [ "Duplicate declaration of ", P / K,
                       " as a \"", Kind, "\" predicate"
                     ]
                   ).


%% overlap_error( + most general instance of a predicate ):
%% The predicate has been declared as both "coinductive" and "bottom":
%% raise a fatal error.

:- mode overlap_error( + ).

overlap_error( Pattern ) :-
            functor( Pattern, P, K ),
            error( [ P/K, " declared both as \"coinductive\" and as \"bottom\""
                   ]
                 ).


%% check_declaration_order( + most general instance of a predicate,
%%                          + kind of declaration
%%                        ):
%% Raise a warning if the definition of this predicate has already been seen

:- mode check_declaration_order( +, + ).

check_declaration_order( Pattern, Kind ) :-
        (
            defined( Pattern )
        ->
            functor( Pattern, P, K ),
            warning( [ P/K,
                       " declared as \"", Kind, "\" after it has been defined" ]
                   )
        ;
            true
        ).
