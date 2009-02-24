%%%  Eclipse-specific predicates that ease compatibility problems.           %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009)                       %%%
%%%                                                                          %%%
%%%  Last update: 23 February 2009.                                          %%%
%%%                                                                          %%%

:- ensure_loaded( higher_order ).


%%------------------------------------------------------------------------------
%% Identify the system.

lp_system( eclipse ).


%%------------------------------------------------------------------------------
%% The standard streams.

std_input_stream(   input          ).
std_output_stream(  output         ).
std_error_stream(   error          ).
std_warning_stream( warning_output ).


%%------------------------------------------------------------------------------
%% getchar( + input stream, - character in the form of an atom ):
%% This is introduced because the built-in get_char/2 returns strings on
%% Eclipse and atoms on Sicstus.

getchar( Stream, Atom ) :-
        get_char( Stream, String ),
        atom_string( Atom, String ).


%%------------------------------------------------------------------------------
%% name_chars( +- atom or number, -+ list of characters that form its name ):
%% Used because Eclipse complains about name/2 being obsolete.

name_chars( Atomic, NameChars ) :-
        (
            var( Atomic )
        ->
            NameChars = [ First | _ ],
            string_list( NameString, NameChars ),
            (
                is_digit_char( First )
            ->
                number_string( Atomic, NameString )
            ;
                atom_string( Atomic, NameString )
            )
        ;
            atom( Atomic )
        ->
            atom_string( Atomic, NameString ),
            string_list( NameString, NameChars )
        ;
            number( Atomic )
        ->
            number_string( Atomic, NameString ),
            string_list( NameString, NameChars )
        ;
            error( [ 'Bad argument type in ', name_chars( Atomic, NameChars ) ]
                 )
        ).

%
is_digit_char( C ) :-
        name_chars( 0, [ ZeroChar ] ),
        ZeroChar =< C,
        name_chars( 9, [ NineChar ] ),
        NineChar >= C.


%%------------------------------------------------------------------------------
%% clause_in_module( + module name, +- clause head, clause body ):
%% Like clause/2, but from the named module.

clause_in_module( ModuleName, Head, Body ) :-
        clause( Head, Body ) @ ModuleName .


%%------------------------------------------------------------------------------
%% current_predicate_in_module( + module name, +- predicate specification ):
%% Like current_predicate/2, but from the named module.

current_predicate_in_module( ModuleName, PredSpec ) :-
        current_predicate( PredSpec ) @ ModuleName .


%%------------------------------------------------------------------------------
%% assertz_in_module( + module name, + clause ):
%% Like assertz/1, but into this module.

assertz_in_module( Module, Clause ) :-
        assertz( Clause ) @ Module .


%%------------------------------------------------------------------------------
%% export_from_module( + module name, + predicate specification ):
%% export/1 factored out here, because Sicstus does not like the "@".

export_from_module( Module, PredSpec ) :-
        export( PredSpec ) @ Module.


%%------------------------------------------------------------------------------
%% dynamic_in_module( + module name, + predicate specification ):
%% dynamic/1 factored out here, because Sicstus does not like the "@".

dynamic_in_module( Module, PredSpec ) :-
        dynamic( PredSpec ) @ Module.


%%------------------------------------------------------------------------------
%% compile_to_module( + module name, + file name ):
%% Compile the program in this file into this module.

compile_to_module( Module, FileName ) :-
        compile( FileName ) @ Module.


%%------------------------------------------------------------------------------
%% write_shallow( + output stream, + term, + maximum depth ):
%% Like write/2, but only to a limited print depth.

write_shallow( OutputStream, Term, MaxDepth ) :-
        write_term( OutputStream, Term, [ depth( MaxDepth ) ] ).


%%------------------------------------------------------------------------------
%% is_built_in( + goal ):
%% Does this goal call a built-in predicate?

is_builtin( Pred ) :-
        functor( Pred, P, K ),
        current_built_in( P/K ).


%%------------------------------------------------------------------------------
%% flush_output( + output stream ):
%% Flush out the buffer of the stream.

flush_output( OutputStream ) :-
        flush( OutputStream ).

%%------------------------------------------------------------------------------
