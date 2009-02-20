%%%  Simple, but useful operations on sets.                                  %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009).                      %%%
%%%                                                                          %%%
%%%  Last update: 20 February 2009.                                          %%%
%%%                                                                          %%%
%%%  NOTE: Different Prolog variables are treated as different items: this   %%%
%%%        is done by design!                                                %%%
%%%                                                                          %%%
%%%        This implementation should only be used for smallish sets.        %%%
%%%        The cost of insertion or membership check is proportional to      %%%
%%%        the size of the set, the cost of set operations (union etc.)      %%%
%%%        is quadratic in the size of the sets.                             %%%
%%%                                                                          %%%


%%% Sets are represented as unordered lists.


%%------------------------------------------------------------------------------
%% empty_set( +- set ) :
%% Create an empty set, or check that the given set is empty.

empty_set( [] ).


%%------------------------------------------------------------------------------
%% add_to_set( + item, + set, - new set ):
%% Add the item to the set.

:- mode add_to_set( +, +, - ).

add_to_set( Item, Set, Set ) :-
        is_in_set( Item, Set ),
        !.

add_to_set( Item, Set, [ Item | Set ] ).


%%------------------------------------------------------------------------------
%% is_in_set( + item, + set ):
%% Is the given item a member of the set?

:- mode is_in_set( +, + ).

is_in_set( Item, [ H | _ ] ) :-
        Item == H,
        !.

is_in_set( Item, [ _ | T ] ) :-
        is_in_set( Item, T ).


%%------------------------------------------------------------------------------
%% generate_member_of_set( + set, - item ):
%% Nondeterministically generate members of the set.

:- mode generate_member_of_set( +, - ).

generate_member_of_set( Item, Set ) :-
        member( Item, Set ).


%%------------------------------------------------------------------------------
%% from_set( + set, - item, - new set ):
%% If the set is empty, fail;  otherwise return some element of the set, as well
%% as the set without that element.

from_set( [ H | T ], H, T ).


%%------------------------------------------------------------------------------
%% set_union( + set, + set, - the union ):
%% Compute the union of two sets.

:- mode set_union( +, +, - ).

set_union( [], S, S ) :-
        !.

set_union( S, [], S ) :-
        !.

set_union( [ H | T ], S, NS ) :-
        is_in_set( H, S ),
        !,
        set_union( T, S, NS ).

set_union( [ H | T ], S, [ H | NS ] ) :-
        % \+ is_in_set( H, S ),
        set_union( T, S, NS ).


%%------------------------------------------------------------------------------
%% set_intersection( + set, + set, - the intersection ):
%% Compute the intersection of two sets.

:- mode set_intersection( +, +, - ).

set_intersection( [], _, [] ) :-
        !.

set_intersection( _, [], [] ) :-
        !.

set_intersection( [ H | T ], S, [ H | NS ] ) :-
        is_in_set( H, S ),
        !,
        set_intersection( T, S, NS ).

set_intersection( [ _ | T ], S, NS ) :-
        % \+ is_in_set( H, S ),
        set_intersection( T, S, NS ).


%%------------------------------------------------------------------------------
%% set_difference( + set, + set, - the difference ):
%% Subtract the second set from the first.

:- mode set_difference( +, +, - ).

set_difference( [], _, [] ) :-
        !.

set_difference( S, [], S ) :-
        !.

set_difference( [ H | T ], S, NS ) :-
        is_in_set( H, S ),
        !,
        set_difference( T, S, NS ).

set_difference( [ H | T ], S, [ H | NS ] ) :-
        % \+ is_in_set( H, S ),
        set_difference( T, S, NS ).


%%------------------------------------------------------------------------------
%% symmetric_set_difference( + set, + set, - the symmetric difference ):
%% Compute the symmetric difference of two sets.

:- mode symmetric_set_difference( +, +, - ).

symmetric_set_difference( S1, S2, NS ) :-
        set_difference( S1, S2, Diff12 ),
        set_difference( S2, S1, Diff21 ),
        append( Diff12, Diff21, NS ).


%%------------------------------------------------------------------------------
%% set_to_list( + set, - list ):
%% Create a list that contains all the elements of the set.

:- mode set_to_list( +, - ).

set_to_list( S, S ).


%%------------------------------------------------------------------------------
%% list_to_set( + list, - set ):
%% Create a set that contains all the elements from the list (without
%% duplicates, of course).

:- mode make_set( +, - ).

make_set( L, S ) :-
        empty_set( S0 ),
        make_set_( L, S0, S ).

%
make_set_( [], S, S ).

make_set_( [ H | T ], S, NS ) :-
        add_to_set( H, S, S2 ),
        make_set_( T, S2, NS ).

%%------------------------------------------------------------------------------
