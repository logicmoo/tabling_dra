%%%  Operations on "open" lists.                                             %%%
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
%% olist_add( + open list, + item ):
%% Add this item at the end of the open list.

olist_add( OL, M ) :-
        olist_end( OL, End ),
        End = [ M | _ ].

%
% Get the end of an open list.

olist_end( End, End ) :-
        var( End ),
        !.

olist_end( [ _ | T ], End ) :-
        olist_end( T, End ).


%%------------------------------------------------------------------------------
%% olist_conc( + open list, + open list ):
%% The first open list becomes the concatenation of the two.

olist_conc( L1, L2 ) :-
        olist_end( L1, End ),
        olist_conc_( End, L2 ).

%
olist_conc_( _, End ) :-
        var( End )
        !.

olist_conc_( End, [ H | T ] ) :-
        End = [ H | NewEnd ],
        olist_conc( NewEnd, T ).



%%------------------------------------------------------------------------------
%% olist_split( + open list, - open list, - open list ):
%% Splits a list into two parts that, when concatenated, would yield the
%% original list.

olist_split( A, B, C ) :-
        var( A ),
        var( B ),
        var( C ).

olist_split( L, L1, L2 ) :-
        nonvar( L ),
        (
            var( L1 ),
            L2 = L
        ;
            L  = [ H | T ],
            L1 = [ H | T1 ],
            olist_split( T, T1, L2 )
        ).

