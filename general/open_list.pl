%%%  Operations on an "open" list.                                           %%%
%%%                                                                          %%%
%%%  An open list is one whose final element is a variable. One can add      %%%
%%%  new elements at the end without copying the list: the downside is that  %%%
%%%  it is a destructive operation, i.e., the old list "disappears".         %%%
%%%                                                                          %%%
%%%  This particular version                                                 %%%
%%%   written by Feliks Kluzniak at UTD (February 2009).                     %%%
%%%                                                                          %%%
%%%  Last update: 18 February 2009.                                          %%%
%%%                                                                          %%%


%%------------------------------------------------------------------------------
%% empty_olist( +- open list ):
%% Create an empty open list, or check that the provided open list is empty.

empty_olist( OL ) :-  var( OL ).


%%------------------------------------------------------------------------------
%% olist_member( +- item, + open list ):
%% Generate a member of the open list, or check that a given term is a member.

olist_member( M, OL ) :-
        nonvar( OL ),
        OL = [ H | T ],
        (
            M = H
        ;
            olist_member( M, T )
        ).


%%------------------------------------------------------------------------------
%% olist_add( + item, + open list ):
%% Add this item at the end of the open list:

olist_add( M, OL ) :-
        (
            var( OL )
        ->
            OL = [ M | _ ]
        ;
            OL = [ _ | T ],
            olist_add( M, T )
        ).


