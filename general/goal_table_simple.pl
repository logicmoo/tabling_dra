%%%  A goal table implemented by an open list.                               %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February 2009).                      %%%
%%%                                                                          %%%
%%%  Last update: 18 February 2009.                                          %%%
%%%                                                                          %%%

:- ensure_loaded( utilities ).
:- ensure_loaded( olist ).


%%% In this implementation the goal table is just an open list.


%%------------------------------------------------------------------------------
%% empty_goal_table( +- goal table ):
%% Create an empty goal table, or check that the provided table is empty.

empty_goal_table( Table ) :-
        empty_olist( Table ).


%%------------------------------------------------------------------------------
%% goal_table_member( + goal, + goal table ):
%% Check whether any instantiations of the goal are in the table: if there are,
%% unify the goal with the first one (backtracking will unify it with each of
%% them in turn).

goal_table_member( Goal, Table ) :-
        olist_member_reversed( Goal, OList ).


%%------------------------------------------------------------------------------
%% is_a_variant_in_goal_table( + goal, + goal table ):
%% Succeed iff a variant of this goal is present in the table.
%% Do not modify the goal.

is_a_variant_in_goal_table( Goal, Table ) :-
        copy_term( Goal, Copy ),
        olist_member( Copy, Table ),
        are_variants( Copy, Goal ),
        !.


%%------------------------------------------------------------------------------
%% goal_table_add( + goal table, + goal ):
%% Add this goal to the table.

goal_table_add( Table, Goal ) :-
        olist_add( Table, Goal ).
