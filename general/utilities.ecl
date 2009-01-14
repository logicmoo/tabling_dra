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

is_an_instance( T1, T2 ) :-
        check( (mk_ground( T1 ) , T1 = T2) ).


%%------------------------------------------------------------------------------
%% are_variants( + term, + term ) :
%%    Succeeds only if both arguments are variants of each other.
%%    Does not instantiate any variables.

are_variants( T1, T2 ) :-
        check( T1 = T2 ),                % to weed out obvious "misfits" cheaply
        is_an_instance( T1, T2 ),
        is_an_instance( T2, T1 ).


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

:-mode get_clause_head( + ).

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


