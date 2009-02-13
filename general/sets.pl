%%%  Simple, but useful operations on sets.                                  %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009).                      %%%
%%%                                                                          %%%
%%%  Last update: 12 February 2009.                                          %%%
%%%                                                                          %%%
%%%  NOTE: Some of the code may be Eclipse-specific and may require          %%%
%%%        minor tweaking for other Prolog systems.                          %%%
%%%                                                                          %%%
%%%  NOTE: Different Prolog variables are treated as different items!        %%%


%%% In this version sets are represented just by lists.


%%------------------------------------------------------------------------------
%% make_set( + list, - set ):
%% Create a set that contains all the elements in the list.

:- mode make_set( +, - ).

make_set( L, S ) :-
        make_empty_set( S0 ),
        make_set_( L, S0, S ).

%
make_set_( [], S, S ).

make_set_( [ H | T ], S, NS ) :-
        add_to_set( H, S, S2 ),
        make_set_( T, S2, NS ).


%%------------------------------------------------------------------------------
%% is_empty_set( - set ) :
%% Is this an empty set?

:- mode is_empty_set( + ).

is_empty_set( [] ).


%%------------------------------------------------------------------------------
%% make_empty_set( - set ) :
%% Create an empty set.

:- mode make_empty_set( - ).

make_empty_set( [] ).


%%------------------------------------------------------------------------------
%% is_set_member( + item, + set ):
%% Is the given item a member of the set?

:- mode is_set_member( +, + ).

is_set_member( Item, [ H | _ ] ) :-
        Item == H,
        !.

is_set_member( Item, [ _ | T ] ) :-
        is_set_member( Item, T ).


%%------------------------------------------------------------------------------
%% set_member( + set, - item ):
%% Nondeterministically produce members of the set.

:- mode set_member( +, - ).

set_member( Item, Set ) :-
        member( Item, Set ).


%%------------------------------------------------------------------------------
%% add_to_set( + item, + set, - new set ):
%% Add the item to the set.

:- mode add_to_set( +, +, - ).

add_to_set( Item, Set, Set ) :-
        is_set_member( Item, Set ),
        !.

add_to_set( Item, Set, [ Item | Set ] ).


%%------------------------------------------------------------------------------
%% set_union( + set, + set, - the union ):
%% Compute the union of two sets.

:- mode set_union( +, +, - ).

set_union( [], S, S ) :-
        !.

set_union( S, [], S ) :-
        !.

set_union( [ H | T ], S, NS ) :-
        is_set_member( H, S ),
        !,
        set_union( T, S, NS ).

set_union( [ H | T ], S, [ H | NS ] ) :-
        % \+ is_set_member( H, S ),
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
        is_set_member( H, S ),
        !,
        set_intersection( T, S, NS ).

set_intersection( [ _ | T ], S, NS ) :-
        % \+ is_set_member( H, S ),
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
        is_set_member( H, S ),
        !,
        set_difference( T, S, NS ).

set_difference( [ H | T ], S, [ H | NS ] ) :-
        % \+ is_set_member( H, S ),
        set_difference( T, S, NS ).


%%------------------------------------------------------------------------------
%% symmetric_set_difference( + set, + set, - the symmetric difference ):
%% Compute the symmetric difference of two sets.

:- mode symmetric_set_difference( +, +, - ).

symmetric_set_difference( S1, S2, NS ) :-
        set_union( S1, S2, Union ),
        set_intersection( S1, S2, Intersection ),
        set_difference( Union, Intersection, NS ).

%%------------------------------------------------------------------------------



