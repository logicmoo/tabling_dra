%%%  Operations on open binary trees.                                        %%%
%%%                                                                          %%%
%%%  An open tree is one whose leaves are variables.  One can add new        %%%
%%%  elements to the tree without copying branches: the downside is that     %%%
%%%  the operation is destructive, i.e., the old tree is modified.           %%%
%%%                                                                          %%%
%%%  This particular version                                                 %%%
%%%   written by Feliks Kluzniak at UTD (February 2009).                     %%%
%%%                                                                          %%%
%%%  Last update: 18 February 2009.                                          %%%
%%%                                                                          %%%

:- ensure_loaded( higher_order ).


%%%
%%%  The format of a node is:
%%%       t( key, information, left subtree, right subtree )
%%%


%%------------------------------------------------------------------------------
%% empty_otree( +- open tree ):
%% Create an empty open tree, or check that the provided open tree is empty.

empty_otree( OT ) :-  var( OT ).


%%------------------------------------------------------------------------------
%% is_in_otree( + key, + comparison predicate, + open tree, - information ):
%% If the entry for this key is present in the tree, succeed and return the
%% associated information; if it is not, fail.
%% "comparison predicate" is a binary predicate that succeeds if the first
%% argument is smaller than the second argument.  Any predicate that implements
%% a total ordering will do.

is_in_otree( Key, LessPred, Node, Info ) :-
        nonvar( Node ),
        Node = t( K, I, L, R ),
        (
            Key = K
        ->
            Info = I
        ;
            apply( LessPred, [ Key, K ] )
        ->
            is_in_otree( Key, LessPred, L, Info )
        ;
            is_in_otree( Key, LessPred, R, Info )
        ).


%%------------------------------------------------------------------------------
%% otree_add( + open tree,
%%            + key,
%%            + information,
%%            + comparison predicate,
%%            + modification predicate
%%          ):
%% Make sure that the key is associated with this information in the open tree.
%% If the entry for this key is already present, modify the existing
%% information.
%% "less predicate" is a binary predicate that succeeds if the first argument is
%% smaller than the second argument.
%% "modification predicate" is a binary predicate that will add information from
%% its second argument to its first argument (if this is not to be an empty
%% operation, the first argument must be modifiable, e.g., it could be an empty
%% list).
%% NOTE: If the format of the information is such that it cannot be modified,
%%       then always check that the key is not in the tree before invoking this
%%       operation.

otree_add( Node, Key, Info, LessPred, ModifyPred ) :-
        (
            var( Node )
        ->
            Node = t( Key, Info, _, _ )
        ;
            Node = t( K, I, L, R ),
            (
                Key = K
            ->
                apply( ModifyPred, [ I, Info ] )
            ;
                apply( LessPred, [ Key, K ] )
            ->
                otree_add( L, Key, Info, LessPred, ModifyPred )
            ;
                otree_add( R, Key, Info, LessPred, ModifyPred )
            )
        ).
