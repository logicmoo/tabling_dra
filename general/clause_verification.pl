%%%  Utilities for verifying well-formedness of clauses.                     %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                       %%%
%%%                                                                          %%%
%%%  Last update: 2 March 2009.                                              %%%
%%%                                                                          %%%

:- ensure_loaded( compatibility_utilities ).
:- ensure_loaded( errors_and_warnings ).


%%------------------------------------------------------------------------------
%% bind_all_variables_to_names( +- term, +- variable dictionary  ):
%% Make sure that all the variables in this term are instantiated to their
%% names. This is not quite the same as bind_variables_to_names/1 (see below),
%% because variables that were originally represented by underscores do not find
%% their way into the variable dictionary.

bind_all_variables_to_names( Term, VarDict ) :-
        bind_variables_to_names( VarDict ),
        ordered_term_variables( Term, AnonymousVars ),
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
%% _A, _B, _C, ... _Z, _V0, _V1 etc. (The underscore is added to avoid spurious
%% messages about singleton variables in case these names are used for output
%% that will subsequently be read by Prolog again.)
%%
%% (All this is done "by hand", since numbervars/3 are not very useful in
%% Eclipse: the writing predicates are not "aware" of '$VAR'(n).)

mk_variable_dictionary( T, VarDict ) :-
        ordered_term_variables( T, Vars ),
        mk_variable_dictionary_( Vars, '_A', VarDict ).

mk_variable_dictionary_( [], _, [] ).

mk_variable_dictionary_( [ V | Vs ], Name, [ [ Name | V ] | VarDict ] ) :-
        mk_next_name( Name, NextName ),
        mk_variable_dictionary_( Vs, NextName, VarDict ).

%
% Once we run out of letters, we will use V0, V1 etc.
%
mk_next_name( '_Z', '_V0' ) :-
        !.

mk_next_name( Name, Next ) :-
        name_chars( Name, [ U, C ] ),      % still in single letters?
        !,
        NC is C + 1,         % good for ASCII, might not work for some encodings
        name_chars( Next, [ U, NC ] ).

mk_next_name( Name, Next ) :-
        name_chars( Name, [ CodeOfUndercore, CodeOfV | DigitChars ] ),
        name_chars( Number, DigitChars ),
        NextNumber is Number + 1,                               % good for ASCII
        name_chars( NextNumber, NewDigitChars ),
        name_chars( Next, [ CodeOfUndercore, CodeOfV | NewDigitChars ] ).


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

verify_program_item( (?- Query), VarDict ) :-       % avoid check for singletons
        !,
        empty_set( Empty ),
        check_for_variable_calls( Query, Empty, ctxt( VarDict, (?- Query) ) ).


verify_program_item( Clause, VarDict ) :-
        \+ is_a_good_clause( Clause, VarDict ),
        !,
        bind_variables_to_names( VarDict ),
        error( [ 'Incorrect clause: \"', Clause, '.\"' ] ).

verify_program_item( _, _ ).



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
        ordered_term_variables( Head, HeadVars ),
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
        ordered_term_variables( A, AVars ),
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
