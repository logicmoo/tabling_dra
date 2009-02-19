%%%  Sicstus-specific predicates that ease compatibility problems.           %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009)                       %%%
%%%                                                                          %%%
%%%  Last update: 19 February 2009.                                          %%%
%%%                                                                          %%%


:- ensure_loaded( library( terms ) ). % A Sicstus library, needed for variant/2.


%%------------------------------------------------------------------------------
%% The standard streams.

std_input_stream(   user_input  ).
std_output_stream(  user_output ).
std_error_stream(   user_error  ).
std_warning_stream( user_error  ).


%%------------------------------------------------------------------------------
%% getchar( + input stream, - character in the form of an atom ):
%% This is introduced because the built-in get_char/2 returns strings on
%% Eclipse and atoms on Sicstus.

getchar( Stream, Atom ) :-
        get_char( Stream, Atom ).


%%------------------------------------------------------------------------------
%% name_chars( + atom, - list of characters that form its name ):
%% Used because Eclipse complains about name/2 being obsolete.

name_chars( Atom, NameChars ) :-
        name( Atom, NameChars ).


%%------------------------------------------------------------------------------
%% setval( + name, + value ):
%% A naive implementation of setval/2: set this counter to this value.

setval( Name, Value ) :-
        Pattern =.. [ Name, _ ],
        retractall( Pattern ),
        Fact =.. [ Name, Value ],
        assert( Fact ).


%%------------------------------------------------------------------------------
%% getval( + name, - value ):
%% A naive implementation of getval/2:
%% get the value associated with this counter.

getval( Name, Value ) :-
        Fact =.. [ Name, Value ],
        Fact.


%%------------------------------------------------------------------------------
%% incval( + name ):
%% A naive implementation of incval/2: increment this counter by 1.

incval( Name ) :-
        getval( Name, Value ),
        NewValue is Value + 1,
        setval( Name, NewValue ).


%%------------------------------------------------------------------------------
%% writeclause( + output stream, + clause,  ):
%% Given an open output stream, write the clause onto it.

writeclause( OutputStream, (:- Directive) ) :-
        !,
        write( OutputStream, ':- ' ),
        write_term( OutputStream, Directive, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

writeclause( OutputStream, (?- Query) ) :-
        !,
        write( OutputStream, '?- ' ),
        write_term( OutputStream, Query, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

writeclause( OutputStream, Clause ) :-
        write_term( OutputStream, Clause, [ indented( true ), quoted( true ) ]
                  ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

%%------------------------------------------------------------------------------
