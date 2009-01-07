%%%%  Some generally-useful utilities.  %%%%


%%------------------------------------------------------------------------------
%% check( + goal ) :
%%    Succeeds iff goal succeeds, but without instantiating any variables
%%    (side-effects may appear, though).

check( Goal ) :-  \+ \+ Goal .


%%------------------------------------------------------------------------------
%% mk_ground( +- term ) :
%%    Instantiates all the unbound variables in the term as constants of
%%    the form ' V0', ' V1'. ' V2' etc.
%%    (Note: This is mainly for "safety". If the strange names of the constants
%%           are not needed, consider using:
%%              mk_ground( T ) :- numbervars( T, 0, _ ).
%%    )

mk_ground( T ) :-  mk_ground_aux( T, 0, _ ).

%
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
%% getline( - list of character strings ) :
%%    Reads characters from the current input stream upto (and including) the
%%    nearest newline.  The newline is not included in the list of characters
%%    that is returned.

getline( Name ) :-  get_char( C ),  getline_( C, Name ).

%
getline_( "\n", []         ) :-  !.

getline_( C   , [ C | Cs ] ) :-
        % C \= "\n",
        get_char( NC ),
        getline_( NC, Cs ).


%%------------------------------------------------------------------------------
%% putline( + list of character strings ) :
%%    Writes the characters to the current output stream and follows them with
%%    a newline.

putline( Cs ) :-  putchars( Cs ),  nl.


%%------------------------------------------------------------------------------
%% putchars( + list of character strings ) :
%%    Writes the characters to the current output stream.

putchars( []         ).
putchars( [ C | Cs ] ) :-  put_char( C ),  putchars( Cs ).
