%%%  Eclipse-specific predicates that ease compatibility problems.           %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009)                       %%%
%%%                                                                          %%%
%%%  Last update: 19 February 2009.                                          %%%
%%%                                                                          %%%


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
%% name_chars( +- atom, -+ list of characters that form its name ):
%% Used because Eclipse complains about name/2 being obsolete.

name_chars( Atom, NameChars ) :-
        (
            var( Atom )
        ->
            string_list( NameString, NameChars ),
            atom_string( Atom, NameString )
        ;
            atom_string( Atom, NameString ),
            string_list( NameString, NameChars )
        ).


%%------------------------------------------------------------------------------
%% clause_in_module( + module name, +- clause head, clause body ):
%% Like clause/2, but from the named module.

clause_in_module( ModuleName, Head, Body ) :-
        clause( Head, Body ) @ ModuleName .


%%------------------------------------------------------------------------------
%% flush_output( + output stream ):
%% Flush out the buffer of the stream.

flush_output( OutputStream ) :-
        flush( OutputStream ).

%%------------------------------------------------------------------------------
