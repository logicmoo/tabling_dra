%%%%  Some generally-useful utilities.  %%%%


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
        number_string( N, CharsOfN ),
        N1 is N + 1,
        append_strings( " V", CharsOfN, CharsOfVN ),
        atom_string( V, CharsOfVN ).

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
%% getline( - list of character strings ) :
%%    Reads characters from the current input stream upto (and including) the
%%    nearest newline.  The newline is not included in the list of characters
%%    that is returned.

:- mode getline( - ).

getline( Line ) :-  get_char( C ),  getline_( C, Line ).

%
:- mode getline_( +, - ).

getline_( "\n", []         ) :-  !.

getline_( C   , [ C | Cs ] ) :-
        % C \= "\n",
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

get_clause_head( H :- _, H ) :-  !.
get_clause_head( H     , H ).


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
        \+ is_list( Hd ).


%%------------------------------------------------------------------------------
%% warning( + term ):
%% warning( + list of terms ):
%% Print this term or list of terms as a warning.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

warning( V ) :-
        var( V ),
        !,
        warning( [ "Incorrect invocation of warning/1: \"",
                   warning( V ),
                   "\""
                 ]
               ).

warning( [] ) :-
        !,
        begin_warning,
        end_warning.

warning( [ A | B ] ) :-
        !,
        begin_warning,
        write_list( warning_output, [ A | B ] ),
        end_warning.

warning( NotAList ) :-
        begin_warning,
        write( warning_output, NotAList ),
        end_warning.


%%------------------------------------------------------------------------------
%% begin_warning:
%% Begin a warning printout.

begin_warning :-
        write( warning_output, "--- WARNING: " ).


%%------------------------------------------------------------------------------
%% end_warning:
%% End a warning printout.

end_warning :-
        writeln( warning_output, " ---" ).


%%------------------------------------------------------------------------------
%% error( + term ):
%% error( + list of terms ):
%% Print this term or list of terms as a error, then abort the computation.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

error( V ) :-
        var( V ),
        !,
        warning( [ "Incorrect invocation of error/1: \"",
                   error( V ),
                   "\""
                 ]
               ).

error( [] ) :-
        !,
        begin_error,
        end_error.

error( [ A | B ] ) :-
        !,
        begin_error,
        write_list( error, [ A | B ] ),
        end_error.

error( NotAList ) :-
        begin_error,
        write( error, NotAList ),
        end_error.


%%------------------------------------------------------------------------------
%% begin_error:
%% Begin a error printout.

begin_error :-
        write( error, "*** ERROR: " ).


%%------------------------------------------------------------------------------
%% end_error:
%% End a error printout.

end_error :-
        writeln( error, " ***" ),
        abort.


%%------------------------------------------------------------------------------
%% write_list( +stream, +list ):
%% Output the items on this list to this stream.
%% There are no spaces between items on the list.
%% Strings are printed without quotes.

write_list( S, V ) :-
        var( V ),
        !,
        warning( [ "Incorrect invocation of write_list/1: \"",
                   write_list( S, V ),
                   "\""
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
        warning( [ "Incorrect invocation of write_list/1: \"",
                   write_list( S, NotAList ),
                   "\""
                 ]
               ).


%%------------------------------------------------------------------------------
%% ensure_filename_is_an_atom( + filename ):
%% Verify that the filename is an atom.  If not, produce a fatal error.

ensure_filename_is_an_atom( FileName ) :-
        atom( FileName ),
        !.

ensure_filename_is_an_atom( FileName ) :-
        % \+ atom( FileName ),
        error( [ "*** Illegal file name \"", FileName, "\" (not an atom). ***" ]
             ).


%%------------------------------------------------------------------------------
%% open_file( + root filename string,
%%            + filename extension string,
%%            + mode,
%%            - stream
%%          ):
%% Construct the file name, and open the file in this mode.

:- mode open_file( +, +, +, - ).

open_file( RootFileNameString, ExtensionString, Mode, Stream ) :-
        concat_strings( RootFileNameString, ExtensionString, FileNameString ),
        open( FileNameString, Mode, Stream ).


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
        writeclause( OutputStream, Clause ),
        fail.

write_clauses( _, _ ).


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

%
verify_program_item( Var ) :-
        var( Var ),
        !,
        error( [ "A variable clause: \"", Var, "\"" ] ).

verify_program_item( (:- Var) ) :-
        var( Var ),
        !,
        error( [ "A variable directive: \"", (:- Var), "\"" ] ).

verify_program_item( (?- Var) ) :-
        var( Var ),
        !,
        error( [ "A variable query: \"", (?- Var), "\"" ] ).

verify_program_item( Clause ) :-
        get_clause_head( Clause, Head ),
        \+ is_good_clause_head( Head ),
        !,
        error( [ "Incorrect head in clause: \"", Clause, ".\"" ] ).

verify_program_item( _ ).

%%------------------------------------------------------------------------------
