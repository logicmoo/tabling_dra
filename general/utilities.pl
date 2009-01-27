%%%  Some generally-useful utilities.                                    %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                   %%%
%%%                                                                      %%%
%%%  Last update: 23 January 2009.                                       %%%
%%%                                                                      %%%
%%%  Converted to Sicstus Prolog: 26 January 2009.                       %%%
%%%                                                                      %%%
%%%  NOTE: Some of the code may be Sicstus-specific (or even still       %%%
%%%        Eclipse-specific) and may require some tweaking for other     %%%
%%%        Prolog systems.                                               %%%


:- ensure_loaded( library( terms ) ). % A Sicstus library, needed for variant/2.


%%------------------------------------------------------------------------------
%% setval( + name, + value ):
%% A naive implementation of Eclipse's setval/2: set this counter to this value.

setval( Name, Value ) :-
        Pattern =.. [ Name, _ ],
        retractall( Pattern ),
        Fact =.. [ Name, Value ],
        assert( Fact ).


%%------------------------------------------------------------------------------
%% getval( + name, - value ):
%% A naive implementation of Eclipse's getval/2: get the value associated with
%% this counter.

getval( Name, Value ) :-
        Fact =.. [ Name, Value ],
        Fact.


%%------------------------------------------------------------------------------
%% incval( + name ):
%% A naive implementation of Eclipse's incval/2: increment this counter by 1.

incval( Name ) :-
        getval( Name, Value ),
        NewValue is Value + 1,
        setval( Name, NewValue ).




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
%%    )

:- mode mk_ground( ? ).

mk_ground( T ) :-  mk_ground_aux( T, 0, _ ).

%
:- mode mk_ground_aux( ?, +, - ).

mk_ground_aux( V, N, N1 ) :-
        var( V ),
        !,
        name( N, CharsOfN ),
        N1 is N + 1,
        append( " V", CharsOfN, CharsOfVN ),
        name( V, CharsOfVN ).

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
%% is_an_instance( + term, + term ) :
%%    Succeeds iff arg1 is an instance of arg2, but does not instantiate any
%%    variables.

% is_an_instance( T1, T2 ) :-
%         check( (mk_ground( T1 ) , T1 = T2) ).

is_an_instance( T1, T2 ) :-  instance( T1, T2 ). % use the built-in from Eclipse


%%------------------------------------------------------------------------------
%% are_variants( + term, + term ) :
%%    Succeeds only if both arguments are variants of each other.
%%    Does not instantiate any variables.

% are_variants( T1, T2 ) :-
%         check( T1 = T2 ),              % to weed out obvious "misfits" cheaply
%         is_an_instance( T1, T2 ),
%         is_an_instance( T2, T1 ).

are_variants( T1, T2 ) :-  variant( T1, T2 ).    % use the built-in from Eclipse



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

:- mode most_general_instance( + ).

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
        error( [ 'A variable instead of predicate specifications: \", ',
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


%%
predspec_to_pattern( PredSpec, Pattern ) :-
        check_predspec( PredSpec ),
        PredSpec = P / K,
        mk_pattern( P, K, Pattern ).


%%
check_predspec( Var ) :-
        var( Var ),
        !,
        error( [ 'A variable instead of a predicate specification: \", ',
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
%% is_good_clause( + term ):
%% Is this term a reasonable clause?

is_good_clause( T ) :-
        nonvar( T ),
        get_clause_head( T, H ),
        is_good_clause_head( H ).


%%------------------------------------------------------------------------------
%% get_clause_head( + term, - head ):
%% Treat this non-variable term as a clause, get its head.

:-mode get_clause_head( +, - ).

get_clause_head( (H :- _), H ) :-  !.
get_clause_head( H       , H ).


%%------------------------------------------------------------------------------
%% is_good_clause_head( + term ):
%% Is this term a good head for a clause?

is_good_clause_head( Var ) :-
        var( Var ),
        !,
        fail.

is_good_clause_head( Hd ) :-
        atom( Hd ),
        !.

is_good_clause_head( Hd ) :-
        compound( Hd ),
        Hd \= [ _ | _ ].



%%------------------------------------------------------------------------------
%% verify_program( + list of terms ):
%% Given a list of terms that should all be clauses, directives, or queries,
%% raise an error if any of the terms is obviously incorrect.

:- mode verify_program( + ).

verify_program( Terms ) :-
        member( Term, Terms ),
        verify_program_item( Term ),
        fail.

verify_program( _ ).


%%------------------------------------------------------------------------------
%% verify_program_item( + list of terms ):
%% Given a term that should  be a clause, a directive, or a query,
%% raise an error if it is obviously incorrect.

verify_program_item( Var ) :-
        var( Var ),
        !,
        error( [ 'A variable clause: \"', Var, '\"' ] ).

verify_program_item( (:- Var) ) :-
        var( Var ),
        !,
        error( [ 'A variable directive: \"', (:- Var), '\"' ] ).

verify_program_item( (?- Var) ) :-
        var( Var ),
        !,
        error( [ 'A variable query: \"', (?- Var), '\"' ] ).

verify_program_item( Clause ) :-
        \+ is_good_clause( Clause ),
        !,
        error( [ 'Incorrect clause: \"', Clause, '.\"' ] ).

verify_program_item( _ ).



%%------------------------------------------------------------------------------
%% ensure_filename_is_an_atom( + filename ):
%% Verify that the filename is an atom.  If not, produce a fatal error.

ensure_filename_is_an_atom( FileName ) :-
        atom( FileName ),
        !.

ensure_filename_is_an_atom( FileName ) :-
        % \+ atom( FileName ),
        error( [ '*** Illegal file name \"', FileName, '\" (not an atom). ***' ]
             ).


%%------------------------------------------------------------------------------
%% open_file( + root filename chars,
%%            + filename extension chars,
%%            + mode,
%%            - stream
%%          ):
%% Construct the file name, and open the file in this mode.

:- mode open_file( +, +, +, - ).

open_file( RootFileNameChars, ExtensionChars, Mode, Stream ) :-
        once( append( RootFileNameChars, ExtensionChars, FileNameChars ) ),
        name( FileName, FileNameChars ),
        open( FileName, Mode, Stream ).



%%------------------------------------------------------------------------------
%% read_terms( + input stream, - list of terms ):
%% Given an open input stream, produce all the terms that can be read from it.
%%
%% The algorithm uses a d-list.

:- mode read_terms( +, - ).

read_terms( InputStream, Terms ) :-
        read_terms_( InputStream, Terms-Terms ).

%
read_terms_( InputStream, Terms-End ) :-
        read( InputStream, Term ),
        (
            Term == end_of_file
        ->
            End = []
        ;
            End = [ Term | NewEnd ],
            read_terms_( InputStream, Terms - NewEnd )
        ).



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
        write_clause( Clause, OutputStream ),
        fail.

write_clauses( _, _ ).


%%------------------------------------------------------------------------------
%% write_clause( + clauses, + output stream ):
%% Given an open output stream, write the clause onto it.

write_clause( (:- Directive), OutputStream ) :-
        !,
        write( OutputStream, ':- ' ),
        write_term( OutputStream, Directive, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

write_clause( (?- Query), OutputStream ) :-
        !,
        write( OutputStream, '?- ' ),
        write_term( OutputStream, Query, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

write_clause( Clause, OutputStream ) :-
        write_term( OutputStream, Clause, [ indented( true ), quoted( true ) ] 
                  ),
        write( OutputStream, '.' ),
        nl( OutputStream ).



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
                   "\""
                 ]
               ).



%%------------------------------------------------------------------------------
%% getline( - list of character strings ) :
%%    Reads characters from the current input stream upto (and including) the
%%    nearest newline.  The newline is not included in the list of characters
%%    that is returned.

:- mode getline( - ).

getline( Line ) :-  get_char( C ),  getline_( C, Line ).

%
:- mode getline_( +, - ).

getline_( '\n', []          ) :-  !.

getline_( C   , [ C | Cs ] ) :-
        get_char( NC ),
        getline_( NC, Cs ).


%%------------------------------------------------------------------------------
%% putline( + list of character strings ) :
%%    Writes the characters to the current output stream and follows them with
%%    a newline.

:- mode putline( + ).

putline( Cs ) :-  putchars( Cs ),  nl.


%%------------------------------------------------------------------------------
%% putchars( + list of character strings ) :
%%    Writes the characters to the current output stream.

:- mode putchars( + ).

putchars( []         ).
putchars( [ C | Cs ] ) :-  put_char( C ),  putchars( Cs ).


%%------------------------------------------------------------------------------
%% writeln( + stream, + term ):
%% Write the term onto the stream, follow with a newline.

writeln( S, T ) :-  write( S, T ),  nl( S ).


%%------------------------------------------------------------------------------
%% writeln( + term ):
%% Write the term onto standard output, follow with a newline.

writeln( T ) :-  writeln( user_output, T ).


%%------------------------------------------------------------------------------
%% warning( + term ):
%% warning( + list of terms ):
%% Print this term or list of terms as a warning.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

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
        begin_warning,
        write_list( user_output, [ A | B ] ),
        end_warning.

warning( NotAList ) :-
        begin_warning,
        write( user_output, NotAList ),
        end_warning.


%%------------------------------------------------------------------------------
%% begin_warning:
%% Begin a warning printout.

begin_warning :-
        write( user_output, '--- WARNING: ' ).


%%------------------------------------------------------------------------------
%% end_warning:
%% End a warning printout.

end_warning :-
        writeln( user_output, " ---" ).



%%------------------------------------------------------------------------------
%% error( + term ):
%% error( + list of terms ):
%% Print this term or list of terms as a error, then abort the computation.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

error( V ) :-
        var( V ),
        !,
        warning( [ 'Incorrect invocation of error/1: \"',
                   error( V ),
                   '\"'
                 ]
               ).

error( [] ) :-
        !,
        begin_error,
        end_error.

error( [ A | B ] ) :-
        !,
        begin_error,
        write_list( user_error, [ A | B ] ),
        end_error.

error( NotAList ) :-
        begin_error,
        write( user_error, NotAList ),
        end_error.


%%------------------------------------------------------------------------------
%% begin_error:
%% Begin a error printout.

begin_error :-
        write( user_error, '*** ERROR: ' ).


%%------------------------------------------------------------------------------
%% end_error:
%% End a error printout.

end_error :-
        writeln( user_error, '***' ),
        abort.

%%------------------------------------------------------------------------------
