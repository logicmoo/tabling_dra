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
%%%  These directives are treated as declarations of coinductive predicates.  A
%%%  coinductive predicate should be declared in this fashion before it is
%%%  defined.
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
        process( Terms, ''/0, ProcessedTerms ),
        write_terms( ProcessedTerms, OutputStream ).




%-----  The main translator  -----


:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."
:- op( 1000, fy, top ).            % allow  ":- top p/k ."
:- op( 1000, fy, bottom ).         % allow  ":- bottom p/k ."


%% A translator directive, e.g., ":- coinductive p/2" will be remembered in
%% a table, e.g., as "coinductive( p( _, _ ), p/2 ).

:- dynamic coinductive/2.
:- dynamic bottom/2.
:- dynamic top/2.


%% initialise_tables:
%% Initialise the translator (make sure that no garbage is left from a previous
%% invocation.

initialise_tables :-
        retractall( coinductive( _, _ ) ),
        retractall( bottom( _, _ )      ),
        retractall( top( _, _ )         ).




%% process( + list of terms,
%%          + name/arity of the current predicate,
%%          - list of processed terms
%%        ):
%% Process the terms, i.e., interpret translator directives and transform
%% other directives, queries and clauses.
%% Information about the current predicate is provided to assist in the
%% detection of the first clause of a coinductive predicate, which requires
%% special treatment; it also helps detect non-contiguous predicate definitions.

process( Terms, _, Terms ).
