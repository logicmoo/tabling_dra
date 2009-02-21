%%%                                                                          %%%
%%%  Check the consistency of the loaded program.                            %%%
%%%  An auxiliary of top_level.                                              %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                       %%%
%%%                                                                          %%%
%%%  Last update: 20 February 2009.                                          %%%
%%%                                                                          %%%


:- ensure_loaded( utilities ).



%% The following things are being checked:
%%  - is there a call to an undefined predicate?
%%  - is there a defined predicate that is not called?

check_general_consistency :-
        findall( (Pattern :- Body),
                 ( current_predicate_in_module( interpreted, PredSpec )
                 , predspec_to_pattern( PredSpec, Pattern )
                 , \+ builtin( Pattern )
                 , clause_in_module( interpreted, Pattern, Body )
                 ),
                 Clauses
               ),
        true.   %<<<<< TBD
