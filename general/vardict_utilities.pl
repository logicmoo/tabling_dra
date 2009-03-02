%%%  Utilities that have to do with variable dictionaries.                   %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                       %%%
%%%                                                                          %%%
%%%  Last update: 2 March 2009.                                              %%%
%%%                                                                          %%%


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
