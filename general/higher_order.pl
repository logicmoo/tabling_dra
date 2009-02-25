%%%  Some "higher order" predicates for Prolog.                              %%%
%%%  This particular version                                                 %%%
%%%   written by Feliks Kluzniak at UTD (February 2009).                     %%%
%%%                                                                          %%%
%%%  Last update: 18 February 2009.                                          %%%
%%%                                                                          %%%

%%% NOTE: Throughout the file "predicate name" will be used either for
%%        the name of a predicate, or for a list representing a partially
%%        applied predicate (see apply/2).


%%------------------------------------------------------------------------------
%% apply( + predicate name, + list of arguments ):
%% apply( + list,           + list of arguments ):
%% For the first form: apply this predicate to the arguments.
%% For the second form: the list represents a partially-applied predicate,
%% i.e., it should consist of a predicate name and the first few actual
%% arguments. The list of arguments should then be just enough for the remaining
%% arguments.
%% For example, if we have
%%     sum( A, B, C ) :-  C is A + B.
%% then
%%     map( [ sum, 5 ], [ 1, 2, 3 ], Result )
%% will bind Result to [ 6, 7, 8 ].

apply( [ PredName | InitialArguments ], RemainingArguments ) :-
        !,
        append( InitialArguments, RemainingArguments, AllArguments ),
        apply( PredName, AllArguments ).

apply( PredName, Arguments ) :-
        Literal =.. [ PredName | Arguments ],
        call( Literal ).


%%------------------------------------------------------------------------------
%% map( + predicate name, + list, - mapped list ):
%% The predicate should implement a unary function, i.e.,
%%   - it should take two arguments, the first of which is an input argument,
%%     and the second of which is an output argument;
%%   - it should always succeed, and the first result should be "what we want".
%% The predicate is applied to input arguments from the list, and the
%% corresponding outputs are returned in the second list.
%%
%% Example:
%%          square( M, N ) :-  N is M * M.
%%
%%          ?- map( square/2, [ 1, 2, 3 ], Ans ).
%%
%%          Ans = [ 1, 4, 9 ].

map( _, [], [] ).

map( PredName, [ H | T ], [ NH | NT ] ) :-
        apply( PredName, [ H, NH ] ),
        map( PredName, T, NT ).


%%------------------------------------------------------------------------------
%% filter( + predicate name, + list, - filtered list ):
%% The predicate should take one argument.
%% The output list will contain only those elements of the input list for which
%% the predicate succeeds.

filter( _, [], [] ).

filter( PredName, [ H | T ], NL ) :-
        (
            apply( PredName, [ H ] )
        ->
            NL = [ H | NT ]
        ;
            NL = NT
        ),
        filter( PredName, T, NT ).


%%------------------------------------------------------------------------------
%% fold( + predicate name,+ initial value, + list, - final value ):
%% The predicate should implement a binary function, i.e.,
%%   - it should take three arguments, the first two of which are input
%%     arguments, and the third of which is an output argument;
%%   - it should always succeed, and the first result should be "what we want".
%% If the list is empty, the initial value is returned; otherwise the predicate
%% is applied to the initial value and ther first member of the list, and then
%% to the result and the third member, and so on.
%% For example, if "sum( A, B, C )" unifies "C" with the sum of "A" and "B",
%% then "fold( sum, 0, [1,2,3], S )" unifies "S" with "6".

fold( _, Initial, [], Initial ).

fold( PredName, Initial, [ H | T ], Result ) :-
        apply( PredName, [Initial, H, R ] ),
        fold( PredName, R, T, Result ).


%%------------------------------------------------------------------------------
