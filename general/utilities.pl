%%%  Some generally-useful utilities.                                        %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                       %%%
%%%                                                                          %%%
%%%  Last update: 24 February 2009.                                          %%%
%%%                                                                          %%%


:- ensure_loaded( set_in_list ).
:- ensure_loaded( higher_order ).
:- ensure_loaded( compatibility_utilities ).



%%------------------------------------------------------------------------------
%% check( + goal ) :
%%    Succeeds iff goal succeeds, but without instantiating any variables
%%    (side-effects may appear, though).

:- mode check( + ).

check( Goal ) :-  \+ \+ Goal .



%%------------------------------------------------------------------------------
%% mk_ground( +- term ) :
%%    Instantiates all the unbound variables in the term as constants of
%%    the form ' V0', ' V1'. ' V2' etc.
%%    (Note: This is mainly for "safety". If the strange names of the constants
%%           are not needed, consider using:
%%              mk_ground( T ) :- numbervars( T, 0, _ ).
%%           on Prolog systems that support numbervars/3.
%%    )

:- mode mk_ground( ? ).

mk_ground( T ) :-  mk_ground_aux( T, 0, _ ).

%
:- mode mk_ground_aux( ?, +, - ).

mk_ground_aux( V, N, N1 ) :-
        var( V ),
        !,
        name_chars( N, CharsOfN ),
        N1 is N + 1,
        name_chars( ' V', CharsOfV ),
        append( CharsOfV, CharsOfN, CharsOfVN ),
        name_chars( V, CharsOfVN ).

mk_ground_aux( T, N, N ) :-
        atomic( T ),
        !.

mk_ground_aux( T, N, N1 ) :-
        % \+ var( T ), \+ atomic( T ),
        T =.. [ _Functor | Args ],
        mk_ground_auxs( Args, N, N1 ).

%
:- mode mk_ground_auxs( +, +, - ).

mk_ground_auxs( []        , N, N  ).
mk_ground_auxs( [ T | Ts ], N, N2 ) :-
        mk_ground_aux( T, N, N1 ),
        mk_ground_auxs( Ts, N1, N2 ).


%%------------------------------------------------------------------------------
%% is_ground_var( + term ):
%% Is this term a variable that was ground by mk_ground/1?

is_ground_var( T ) :-
        atom( T ),
        atom_string( T, S ),
        substring( S, " V", 1 ).


%%------------------------------------------------------------------------------
%% ground_term_variables( + term, - set of variables ):
%% Given a term grounded by mk_ground/1, produce the set of the grounded form of
%% variables occurring in the term.
%% (Note that the term may be a subset of a larger term grounded by mk_ground/1,
%%  so the variable numbers need not be contiguous.)

:- mode ground_term_variables( +, - ).

ground_term_variables( T, S ) :-
        empty_set( S0 ),
        gtv_( T, S0, S ),
        !.

ground_term_variables( T, _ ) :-
        error( [ 'Bad call to ground_term_variables (term not ground): ', T ] ).

%
:- mode gtv_( +, +, - ).

gtv_( V, _, _ ) :-
        var( V ),
        !,
        fail.

gtv_( T, S, NS ) :-
        is_ground_var( T ),
        !,
        add_to_set( T, S, NS ).

gtv_( T, S, S ) :-
        atom( T ),
        !.

gtv_( [ H | T ], S, NS ) :-
        !,
        gtv_( H,  S, S2 ),
        gtv_( T, S2, NS ).

gtv_( T, S, NS ) :-
        T =.. [ _ | Args ],
        gtv_( Args, S, NS ).



%%------------------------------------------------------------------------------
%% is_an_instance( + term, + term ) :
%%    Succeeds iff arg1 is an instance of arg2, but does not instantiate any
%%    variables.

% is_an_instance( T1, T2 ) :-
%         check( (mk_ground( T1 ) , T1 = T2) ).

is_an_instance( T1, T2 ) :-  instance( T1, T2 ).              % use the built-in


%%------------------------------------------------------------------------------
%% are_variants( + term, + term ) :
%%    Succeeds only if both arguments are variants of each other.
%%    Does not instantiate any variables.

% are_variants( T1, T2 ) :-
%         check( T1 = T2 ),              % to weed out obvious "misfits" cheaply
%         is_an_instance( T1, T2 ),
%         is_an_instance( T2, T1 ).

are_variants( T1, T2 ) :-  variant( T1, T2 ).                 % use the built-in



%%------------------------------------------------------------------------------
%% mk_pattern( + an atom representing the name of a predicate,
%%             + an integer representing the arity of the predicate,
%%             - the most general pattern that matches all invocations of the
%%               predicate
%%           )
%% Given p/k, produce p( _, _, ... _ )  (of arity k)

:- mode mk_pattern( +, +, - ).

mk_pattern( P, K, Pattern ) :-
        length( Args, K ),                            % Args = K fresh variables
        Pattern =.. [ P | Args ].


%%------------------------------------------------------------------------------
%% most_general_instance( + a term,
%%                        - a most general instance with the same main functor
%%                      ):
%% E.g., p( a, q( X, Y ) )  is transformed to  p( _, _ ).

:- mode most_general_instance( +, - ).

most_general_instance( Term, Pattern ) :-
        functor( Term, P, K ),
        mk_pattern( P, K, Pattern ).



%%------------------------------------------------------------------------------
%% predspecs_to_patterns( + a conjunction of predicate specifications,
%%                        - list of most general instances of these predicates
%%                      ):
%% Given one or several predicate specifications (in the form "p/k" or
%% "p/k, q/l, ...") check whether they are well-formed: if not, raise a fatal
%% error; otherwise return a list of the most general instances that correspond
%% to the predicate specifications.

predspecs_to_patterns( Var, _ ) :-
        var( Var ),
        !,
        error( [ 'A variable instead of predicate specifications: \"',
                 Var,
                 '\"'
               ]
             ).

predspecs_to_patterns( (PredSpec , PredSpecs), [ Pattern | Patterns ] ) :-
        !,
        predspec_to_pattern( PredSpec, Pattern ),
        predspecs_to_patterns( PredSpecs, Patterns ).

predspecs_to_patterns( PredSpec, [ Pattern ] ) :-
        predspec_to_pattern( PredSpec, Pattern ).


%%------------------------------------------------------------------------------
%% predspec_to_pattern( + a predicate specification,
%%                      - a most general instance of this predicate
%%                    ):
%% Given a predicate specification (in the form "p/k") check whether it is
%% well-formed: if not, raise a fatal error; otherwise return a most general
%% instance that correspond to the predicate specification.

predspec_to_pattern( PredSpec, Pattern ) :-
        check_predspec( PredSpec ),
        PredSpec = P / K,
        mk_pattern( P, K, Pattern ).


%%------------------------------------------------------------------------------
%% check_predspec:
%% Raise an error if this is not a good predicate specification.
check_predspec( Var ) :-
        var( Var ),
        !,
        error( [ 'A variable instead of a predicate specification: \"',
                 Var,
                 '\"'
               ]
             ).

check_predspec( P / K ) :-
        atom( P ),
        integer( K ),
        K >= 0,
        !.

check_predspec( PredSpec ) :-
        error( [ 'An incorrect predicate specification: \"', PredSpec, '\"' ] ).



%%------------------------------------------------------------------------------
%% bind_all_variables_to_names( +- term, +- variable dictionary  ):
%% Make sure that all the variables in this term are instantiated to their
%% names. This is not quite the same as bind_variables_to_names/1 (see below),
%% because variables that were originally represented by underscores do not find
%% their way into the variable dictionary.

bind_all_variables_to_names( Term, VarDict ) :-
        bind_variables_to_names( VarDict ),
        term_variables( Term, AnonymousVars ),
        bind_anonymous( AnonymousVars ).

%
bind_anonymous( [] ).

bind_anonymous( [ '_' | Vs ] ) :-
        bind_anonymous( Vs ).



%%------------------------------------------------------------------------------
%% bind_variables_to_names( +- variable dictionary  ):
%% The variable dictionary is of the format returned by readvar/3, i.e., a list
%% of pairs of the form "[ name | variable ]".  Go through the dictionary,
%% binding each variable to the associated name.
%% NOTE: See bind_all_variables_to_names/2 above!

bind_variables_to_names( VarDict ) :-
        map( bind_var_to_name, VarDict, _ ).

%
bind_var_to_name( [ Name | Name ], _ ).


%%------------------------------------------------------------------------------
%% mk_variable_dictionary( + term, - a variable dictionary ):
%% Produce a variable dictionary for this term, as if it had been read by
%% readvar/3.
%% Since the original variable names are not available, we will use the names
%% A, B, C, ... Z, V0, V1 etc. This has an important disadvantage: each variable
%% whose name originally began with underscores (e.g., an anonymous variable)
%% will obtain a "normal" name, and look like a singleton.
%%
%% (All this is done "by hand", since numbervars/3 are not very useful in
%% Eclipse: the writing predicates are not "aware" of '$VAR'(n).)

mk_variable_dictionary( T, VarDict ) :-
        term_variables( T, Vars ),
        mk_variable_dictionary_( Vars, 'A', VarDict ).

mk_variable_dictionary_( [], _, [] ).

mk_variable_dictionary_( [ V | Vs ], Name, [ [ Name | V ] | VarDict ] ) :-
        mk_next_name( Name, NextName ),
        mk_variable_dictionary_( Vs, NextName, VarDict ).

%
% Once we run out of letters, we will use V0, V1 etc.
%
mk_next_name( 'Z', 'V0' ) :-
        !.

mk_next_name( Name, Next ) :-
        name_chars( Name, [ C ] ),      % still in single letters?
        !,
        NC is C + 1,         % good for ASCII, might not work for some encodings
        name_chars( Next, [ NC ] ).

mk_next_name( Name, Next ) :-
        name_chars( Name, [ CodeOfV | DigitChars ] ),
        name_chars( Number, DigitChars ),
        NextNumber is Number + 1,                               % good for ASCII
        name_chars( NextNumber, NewDigitChars ),
        name_chars( Next, [ CodeOfV | NewDigitChars ] ).


%%------------------------------------------------------------------------------
%% is_a_variable_name( + term ):
%% Is this the name of a variable?

is_a_variable_name( Term ) :-
        atom( Term ),
        name_chars( Term, [ FirstChar | _ ] ),
        name_chars( FirstCharAtom, [ FirstChar ] ),
        (
            FirstCharAtom = '_',
            !
        ;
            upper_case_letter( FirstCharAtom )
        ).

%
upper_case_letter( Atom ) :-  uc( Atom ),  !.

uc( 'A' ).  uc( 'B' ). uc( 'C' ).  uc( 'D' ).  uc( 'E' ).  uc( 'F' ).
uc( 'G' ).  uc( 'H' ). uc( 'I' ).  uc( 'J' ).  uc( 'K' ).  uc( 'L' ).
uc( 'M' ).  uc( 'N' ). uc( 'O' ).  uc( 'P' ).  uc( 'Q' ).  uc( 'R' ).
uc( 'S' ).  uc( 'T' ). uc( 'U' ).  uc( 'V' ).  uc( 'W' ).  uc( 'X' ).
uc( 'Y' ).  uc( 'Z' ).



%%------------------------------------------------------------------------------
%% is_a_good_clause( + term ):
%% Is this term a reasonable clause?  Even if it is,  warnings may be produced.

is_a_good_clause( T ) :-
        mk_variable_dictionary( T, VarDict ),
        is_a_good_clause( VarDict ).


%%------------------------------------------------------------------------------
%% is_a_good_clause( + term, + variable dictionary ):
%% Is this term a reasonable clause?  Even if it is,  warnings may be produced.

is_a_good_clause( T, VarDict ) :-
        nonvar( T ),
        get_clause_head( T, H ),
        is_a_good_clause_head( H ),
        has_a_good_clause_body( T, VarDict ).


%%------------------------------------------------------------------------------
%% get_clause_head( + term, - head ):
%% Treat this non-variable term as a clause, get its head.

:-mode get_clause_head( +, - ).

get_clause_head( (H :- _), H ) :-  !.
get_clause_head( H       , H ).


%%------------------------------------------------------------------------------
%% is_a_good_clause_head( + term ):
%% Is this term a good head for a clause?

is_a_good_clause_head( Var ) :-
        var( Var ),
        !,
        fail.

is_a_good_clause_head( Hd ) :-
        atom( Hd ),
        !.

is_a_good_clause_head( Hd ) :-
        Hd \= [ _ | _ ].


%%------------------------------------------------------------------------------
%% has_a_good_clause_body( + term, + variable dictionary ):
%% Treat this non-variable term as a clause, check it for elementary sanity.
%% Assume that the head is not a variable.
%%
%% NOTE: The check is mainly to ensure that there are no variable literals.
%%       Invocations of call/1 are allowed, but an error will be raised if
%%       the argument is a variable that had no earlier occurrences.
%%       As an added bonus, we produce warnings about singleton variables.
%%
%% Throughout most of the code we carry a context argument (Ctxt): this is
%% a structure that holds the entire clause and the variable dictionary, and
%% we use it to produce better diagnostics.

:- mode has_a_good_clause_body( ?, + ).

has_a_good_clause_body( Clause, VarDict ) :-
        has_a_good_clause_body_( Clause, ctxt( VarDict, Clause ) ).

has_a_good_clause_body_( Clause, Ctxt ) :-
        Clause = (Head :- Body),
        !,
        term_variables( Head, HeadVars ),
        check_for_variable_calls( Body, HeadVars, Ctxt ),
        check_for_singleton_variables( Clause, Ctxt ).

has_a_good_clause_body_( Fact, Ctxt ) :-
        check_for_singleton_variables( Fact, Ctxt ).


% clause_error( + list to be displayed (the error message), + the context ):
% Show the error and the entire clause, with proper variable names.
% For the time being V and 'V' will both appear as V on output.

clause_error( MsgList, ctxt( VarDict, Clause ) ) :-
        bind_all_variables_to_names( Clause, VarDict ),     % may affect MSgList
        append( MsgList, [ ':' ], FullMsgList ),
        error( lines( [ FullMsgList, [ Clause, '.' ] ] ) ).


% clause_warning( + list to be displayed (the warning message), + the context ):
% Show the warning and the entire clause, with proper variable names.
% For the time being V and 'V' will both appear as V on output.

clause_warning( MsgList, ctxt( VarDict, Clause ) ) :-
        bind_all_variables_to_names( Clause, VarDict ),     % may affect MSgList
        append( MsgList, [ ' in' ], FullMsgList ),
        warning( lines( [ FullMsgList, [ Clause, '.' ] ] ) ),
        fail.

clause_warning( _, _ ).



% check_for_variable_calls( + (part of a) clause body,
%                           + the set of variables seen so far,
%                           + the context (just for better diagnostics)
%                         )

check_for_variable_calls( V, _, Ctxt ) :-
        var( V ),
        !,
        clause_error( [ 'A variable literal (\"', V, '\")' ], Ctxt ).

check_for_variable_calls( (A ; B), Vars, Ctxt ) :-
        !,
        check_for_variable_calls( A, Vars, Ctxt ),
        check_for_variable_calls( B, Vars, Ctxt ).

check_for_variable_calls( (A -> B), Vars, Ctxt ) :-
        !,
        check_for_variable_calls( (A , B), Vars, Ctxt ).

check_for_variable_calls( (A , B), Vars, Ctxt ) :-
        !,
        check_for_variable_calls( A, Vars, Ctxt ),
        term_variables( A, AVars ),
        set_union( AVars, Vars, NVars ),
        check_for_variable_calls( B, NVars, Ctxt ).

check_for_variable_calls( \+ A, Vars, Ctxt ) :-
        !,
        check_for_variable_calls( A, Vars, Ctxt ).

check_for_variable_calls( once( A ), Vars, Ctxt ) :-
        !,
        check_for_variable_calls( A, Vars, Ctxt ).

check_for_variable_calls( call( T ), Vars, Ctxt ) :-
        !,
        (
            nonvar( T )
        ->
            check_for_variable_calls( T, Vars, Ctxt )
        ;
            % var( T ),
            (
                is_in_set( T, Vars )
            ->
                true
            ;
                clause_error( [ 'The variable argument of \"', call( T ),
                                '\" does not have previous occurrences'
                              ],
                              Ctxt
                            )
            )
        ).

check_for_variable_calls( T, _, Ctxt ) :-
        \+ callable( T ),
        !,
        clause_error( [ 'Incorrect literal (\"', T, '\")' ], Ctxt ).

check_for_variable_calls( _, _, _ ).



%% check_for_singleton_variables( + clause, + context ):
%% Produce a warning if there is a path in the clause on which a variable occurs
%% only once, that occurrence of the variable is not a unique occurrence of the
%% variable on other paths, and the name of the variable does not begin with an
%% underscore.
%% Always succeed.
%% Assume the clause has been verified by is_a_good_clause/1.

check_for_singleton_variables( Clause, Ctxt ) :-
        Ctxt = ctxt( VarDict, _ ),
        cs( Clause, VarDict, _, Singletons ),
        (
            empty_set( Singletons )
        ->
            true
        ;
            clause_warning( [ 'Singleton variables ', Singletons ], Ctxt )
        ).

%
% cs( + (sub)term,      + variable dictionary,
%     - variables seen, - set of singletons
%   ):
% Produce the set of variables occurring in this term, as well as the set of
% variables that are singletons in this term.
% Note that the set of singletons is a subset of the set of seen.
% Note also that a variable whose name begins with an underscore "does not
% count".

cs( V, VarDict, Seen, Single ) :-
        var( V ),
        member( [ Name | Var ], VarDict ),
        Var == V,
        !,
        name_chars( '_',  [ Underscore ] ),
        (
            name_chars( Name, [ Underscore | _ ] )
        ->
            empty_set( Seen ),
            empty_set( Single )
        ;
            Seen   = [ V ],
            Single = [ V ]
        ).

cs( V, _, Seen, Single ) :-    % turns out '_' is not included in VarDict!
        var( V ),
        empty_set( Seen ),
        empty_set( Single ).

cs( A, _, [], [] ) :-
        atomic( A ),
        !.

cs( (A ; B), VarDict, Seen, Single ) :-
        !,
        cs( A, VarDict, SeenA, SingleA ),
        cs( B, VarDict, SeenB, SingleB ),
        set_union( SeenA,   SeenB,   Seen   ),
        set_union( SingleA, SingleB, Single ).

cs( (A , B), VarDict, Seen, Single ) :-
        !,
        cs( [ A | B ], VarDict, Seen, Single ).

cs( (A :- B), VarDict, Seen, Single ) :-
        !,
        cs( [ A | B ], VarDict, Seen, Single ).

cs( (A -> B), VarDict, Seen, Single ) :-
        !,
        cs( [ A | B ], VarDict, Seen, Single ).

cs( [ A | B ], VarDict, Seen, Single ) :-
        !,
        cs( A, VarDict, SeenA, SingleA ),
        cs( B, VarDict, SeenB, SingleB ),
        set_union( SeenA, SeenB, Seen ),
        set_difference( SingleA, SeenB, SA ),
        set_difference( SingleB, SeenA, SB ),
        set_union( SA, SB, Single ).

cs( C, VarDict, Seen, Single ) :-
        compound( C ),
        !,
        C =.. [ _ | Args ],
        cs( Args, VarDict, Seen, Single ).

cs( T, VarDict, Seen, Single ) :-
        error( [ 'INTERNAL ERROR: (utilities)',
                 cs( T, VarDict, Seen, Single )
               ]
             ).



%%------------------------------------------------------------------------------
%% verify_program_with_vars( + list of pairs ):
%% Like verify_program/1 below, but instead of a list of terms we have a list
%% of pairs (pair( term, variable dictionary for the term)), i.e., we can use
%% information about original variable names.

:- mode verify_program_with_vars( + ).

verify_program_with_vars( Pairs ) :-
        member( Pair, Pairs ),
        Pair = pair( Term, VarDict ),
        verify_program_item( Term, VarDict ),
        fail.

verify_program_with_vars( _ ).



%%------------------------------------------------------------------------------
%% verify_program( + list of terms ):
%% Given a list of terms that should all be clauses, directives, or queries,
%% raise an error if any of the terms is obviously incorrect.
%% See also verify_program_with_vars/1 above.

:- mode verify_program( + ).

verify_program( Terms ) :-
        member( Term, Terms ),
        mk_variable_dictionary( Term, VarDict ),
        verify_program_item( Term, VarDict ),
        fail.

verify_program( _ ).


%%------------------------------------------------------------------------------
%% verify_program_item( + list of terms, + variable dictionary ):
%% Given a term that should  be a clause, a directive, or a query,
%% raise an error if it is obviously incorrect.

verify_program_item( Var, VarDict ) :-
        var( Var ),
        !,
        bind_variables_to_names( VarDict ),
        error( [ 'A variable clause: \"', Var, '\"' ] ).

verify_program_item( (:- Var), VarDict ) :-
        var( Var ),
        !,
        bind_variables_to_names( VarDict ),
        error( [ 'A variable directive: \"', (:- Var), '\"' ] ).

verify_program_item( (?- Var), VarDict ) :-
        var( Var ),
        !,
        bind_variables_to_names( VarDict ),
        error( [ 'A variable query: \"', (?- Var), '\"' ] ).

verify_program_item( Clause, VarDict ) :-
        \+ is_a_good_clause( Clause, VarDict ),
        !,
        bind_variables_to_names( VarDict ),
        error( [ 'Incorrect clause: \"', Clause, '.\"' ] ).

verify_program_item( _, _ ).



%%------------------------------------------------------------------------------
%% ensure_filename_is_an_atom( + filename ):
%% Verify that the filename is an atom.  If not, produce a fatal error.

ensure_filename_is_an_atom( FileName ) :-
        atom( FileName ),
        !.

ensure_filename_is_an_atom( FileName ) :-
        % \+ atom( FileName ),
        error( [ 'Illegal file name \"', FileName, '\" (not an atom).' ]
             ).



%%------------------------------------------------------------------------------
%% ensure_extension( + file name chars,
%%                   + extension chars,
%%                   - the root file name,
%%                   - file name, possibly extended
%%                 ):
%% If the file name has no extension, add the provided extension (which must
%% include the period; it can also be empty) and return the file name as the
%% root name; if the file name has an extension, don't change it, but extract
%% the root name.

:- mode ensure_extension( +, +, -, - ).

ensure_extension( FileNameChars, _, RootFileNameChars, FileNameChars ) :-
        name_chars( '.', [ Dot ] ),
        append( RootFileNameChars, [ Dot | _ ], FileNameChars ), % has extension
        !.

ensure_extension( FileNameChars, ExtChars,
                  FileNameChars, FullFileNameChars
                ) :-
        append( FileNameChars, ExtChars, FullFileNameChars ).



%%------------------------------------------------------------------------------
%% read_terms_with_vars( + input stream,
%%                       - list of terms with variable dictionaries
%%                     ):
%% Like read_terms/2 (see below), but each item on the resulting list is of
%% the form "pair( term, variable dictionary )", where the variable dictionary
%% is of the format returned by readvar.

:- mode read_terms_with_vars( +, - ).

read_terms_with_vars( InputStream, Terms ) :-
        readvar( InputStream, Term, VarDict ),
        read_terms_with_vars_( InputStream, pair( Term, VarDict ), Terms ).

%
read_terms_with_vars_( _, pair( end_of_file, _ ), [] ) :-
        !.

read_terms_with_vars_( InputStream, Pair, [ Pair | Pairs ] ) :-
        % Pair \= pair( end_of_file, _ ),
        process_if_op_directive( Pair ),
        readvar( InputStream, NextTerm, NextVarDict ),
        read_terms_with_vars_( InputStream,
                               pair( NextTerm, NextVarDict ), Pairs
                             ).

%
process_if_op_directive( pair( (:- op( P, F, Ops)), _ ) ) :-
        !,
        op( P, F, Ops ).

process_if_op_directive( _ ).


%%------------------------------------------------------------------------------
%% read_terms( + input stream, - list of terms ):
%% Given an open input stream, produce all the terms that can be read from it.
%%
%% NOTE: Operator declarations are interpreted on the fly, but not deleted from
%%       output.
%%       See also read_terms_with_vars/2 above.

:- mode read_terms( +, - ).

read_terms( InputStream, Terms ) :-
        read( InputStream, Term ),
        read_terms_( InputStream, Term, Terms ).

%
read_terms_( _, end_of_file, [] ) :-
        !.

read_terms_( InputStream, Term, [ Term | Terms ] ) :-
        % term \= end_of_file,
        process_if_op_directive( Term ),
        read( InputStream, NextTerm ),
        read_terms_( InputStream, NextTerm, Terms ).



%%------------------------------------------------------------------------------
%% write_terms( + list of terms, + output stream ):
%% Given an open output stream, write onto it all the terms from the list,
%% one per line but without any other pretty-printing.

:- mode write_terms( +, + ).

write_terms( Terms, OutputStream ) :-
        member( Term, Terms ),
        write( OutputStream, Term ),
        writeln( OutputStream, '.' ),
        fail.

write_terms( _, _ ).


%%------------------------------------------------------------------------------
%% write_clauses( + list of clauses, + output stream ):
%% Given an open output stream, write onto it all the clauses from the list.

:- mode write_clauses( +, + ).

write_clauses( Clauses, OutputStream ) :-
        member( Clause, Clauses ),
        writeclause( OutputStream, Clause ),
        fail.

write_clauses( _, _ ).



%%------------------------------------------------------------------------------
%% write_list( +stream, +list ):
%% Output the items on this list to this stream.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

write_list( S, V ) :-
        var( V ),
        !,
        warning( [ 'Incorrect invocation of write_list/1: \"',
                   write_list( S, V ),
                   '\"'
                 ]
               ).

write_list( _S, [] ) :-
        !.

write_list( S, [ H | T ] ) :-
        !,
        write( S, H ),
        write_list( S, T ).

write_list( S, NotAList ) :-
        !,
        warning( [ 'Incorrect invocation of write_list/1: \"',
                   write_list( S, NotAList ),
                   '\"'
                 ]
               ).



%%------------------------------------------------------------------------------
%% getline( + input stream, - list of character atoms ) :
%%    Reads characters from this input stream upto (and including) the nearest
%%    newline.  The newline is not included in the list of characters that is
%%    returned.

:- mode getline( +, - ).

getline( InputStream, Line ) :-
        getchar( InputStream, C ),
        getline_( InputStream, C, Line ).

%
:- mode getline_( +, +, - ).

getline_( _InputStream, '\n', []        ) :-  !.

getline_( InputStream, C   , [ C | Cs ] ) :-
        getchar( InputStream, NC ),
        getline_( InputStream, NC, Cs ).


%%------------------------------------------------------------------------------
%% putline( + output stream, + list of character strings ) :
%%    Writes the characters to this stream and follows them with a newline.

:- mode putline( +, + ).

putline( OutputStream, Cs ) :-
        putchars( OutputStream, Cs ),
        nl( OutputStream ).


%%------------------------------------------------------------------------------
%% putchars( + output stream, + list of character strings ) :
%%    Writes the characters to the current output stream.

:- mode putchars( +, + ).

putchars( _OutputStream, []         ).

putchars( OutputStream, [ C | Cs ] ) :-
        put_char( OutputStream, C ),
        putchars( OutputStream, Cs ).



%%------------------------------------------------------------------------------
%% warning( + term ):
%% warning( + list of terms ):
%% Print this term or list of terms as a warning.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.
%% NOTE: If the term is "lines/1", then the argument should be a non-empty list.
%%       Each of the top level items on the list is printed in a separate line.

warning( V ) :-
        var( V ),
        !,
        warning( [ 'Incorrect invocation of warning/1: \"',
                   warning( V ),
                   '\"'
                 ]
               ).

warning( [] ) :-
        !,
        begin_warning,
        end_warning.

warning( [ A | B ] ) :-
        !,
        std_warning_stream( WS ),
        begin_warning,
        write_list( WS, [ A | B ] ),
        write( WS, ' ' ),
        end_warning.

warning( lines( [ FirstLine | OtherLines ]  ) ) :-
        !,
        begin_warning,
        std_warning_stream( WS ),
        warning_line( WS, FirstLine ),
        warning_lines( OtherLines, WS ),
        end_warning.

warning( NotAList ) :-
        begin_warning,
        std_warning_stream( WS ),
        write( WS, NotAList ),
        write( WS, ' ' ),
        end_warning.

%
warning_lines( [], _ ).

warning_lines( [ Line | Lines ], WS ) :-
        write( WS, '---          ' ),
        warning_line( WS, Line ),
        warning_lines( Lines, WS ).

%
warning_line( WS, List ) :-
        ( List = [] ; List = [ _ | _ ] ),
        !,
        write_list( WS, List ),
        nl( WS ).

warning_line( WS, Term ) :-
        writeln( WS, Term ).


%%------------------------------------------------------------------------------
%% begin_warning:
%% Begin a warning printout.

begin_warning :-
        std_warning_stream( WS ),
        write( WS, '--- WARNING: ' ).


%%------------------------------------------------------------------------------
%% end_warning:
%% End a warning printout.

end_warning :-
        std_warning_stream( WS ),
        writeln( WS, '---' ).



%%------------------------------------------------------------------------------
%% error( + term ):
%% error( + list of terms ):
%% Print this term or list of terms as an error, then abort the computation.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.
%% NOTE: If the term is "lines/1", then the argument should be a non-empty list.
%%       Each of the top level items on the list is printed in a separate line.

error( V ) :-
        var( V ),
        !,
        error( [ 'Incorrect invocation of error/1: \"', error( V ), '\"' ] ).

error( [] ) :-
        !,
        begin_error,
        end_error.

error( [ A | B ] ) :-
        !,
        begin_error,
        std_error_stream( ES ),
        write_list( ES, [ A | B ] ),
        write( ES, ' ' ),
        end_error.

error( lines( [ FirstLine | OtherLines ]  ) ) :-
        !,
        begin_error,
        std_error_stream( ES ),
        error_line( ES, FirstLine ),
        error_lines( OtherLines, ES ),
        end_error.

error( NotAList ) :-
        begin_error,
        std_error_stream( ES ),
        write( ES, NotAList ),
        write( ES, ' ' ),
        end_error.

%
error_lines( [], _ ).

error_lines( [ Line | Lines ], ES ) :-
        write( ES, '***        ' ),
        error_line( ES, Line ),
        error_lines( Lines, ES ).

%
error_line( ES, List ) :-
        ( List = [] ; List = [ _ | _ ] ),
        !,
        write_list( ES, List ),
        nl( ES ).

error_line( ES, Term ) :-
        writeln( ES, Term ).



%%------------------------------------------------------------------------------
%% begin_error:
%% Begin an error printout.

begin_error :-
        std_error_stream( ES ),
        write( ES, '*** ERROR: ' ).


%%------------------------------------------------------------------------------
%% end_error:
%% End an error printout.

end_error :-
        std_error_stream( ES ),
        writeln( ES, '***' ),
        abort.

%%------------------------------------------------------------------------------
