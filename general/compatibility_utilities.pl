%%%  Sicstus-specific predicates that ease compatibility problems.           %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009)                       %%%
%%%                                                                          %%%
%%%  Last update: 20 February 2009.                                          %%%
%%%                                                                          %%%

:- ensure_loaded( higher_order ).
:- ensure_loaded( library( terms ) ). % A Sicstus library, needed for variant/2.


%%------------------------------------------------------------------------------
%% Identify the system.

lp_system( sicstus ).


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
%% clause_in_module( + module name, +- clause head, clause body ):
%% Like clause/2, but from the named module.

clause_in_module( ModuleName, Head, Body ) :-
        clause( ModuleName : Head, Body ).


%%------------------------------------------------------------------------------
%% current_predicate_in_module( + module name, +- predicate specification ):
%% Like current_predicate/2, but from the named module.

current_predicate_in_module( ModuleName, PredSpec ) :-
        current_predicate( ModuleName : PredSpec ).


%%------------------------------------------------------------------------------
%% is_built_in( + goal ):
%% Does this goal call a built-in predicate?

is_builtin( Pred ) :-
        predicate_property( Pred, built_in ).


%%------------------------------------------------------------------------------
%% bind_variables_to_names( +- variable dictionary  ):
%% The variable dictionary is of the format returned by read_term/3 with the
%% option "variable_names", i.e., a list of pairs of the form "name = variable".
%%  Go through the dictionary, binding each variable to the associated name.

bind_variables_to_names( VarDict ) :-
        map( bind_var_to_name, VarDict, _ ).

%
bind_var_to_name( Name = Name, _ ).


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
%% writeclause( + output stream, + clause ):
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
%% writeln( + output stream, + term ):
%% Write the term, followed by a newline.

writeln( OutputStream, Term ) :-
        write( OutputStream, Term ),
        nl(    OutputStream ).


%%------------------------------------------------------------------------------
%% concat_atoms( + atom, + atom, - atom ):
%% Return an atom whose name is the concatenation of the names of the first two
%% atoms.

concat_atoms( A, B, AB ) :-
        name( A, AChars ),
        name( B, BChars ),
        append( AChars, BChars, ABChars ),
        name( AB, ABChars ).

%%------------------------------------------------------------------------------
