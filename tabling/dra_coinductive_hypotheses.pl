%%%  The "set of coinductive hypotheses" for the "dra" interpreter.          %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (March 2009)           .              %%%
%%%                                                                          %%%
%%%  Last update: 27 March 2009.                                             %%%
%%%                                                                          %%%

%%% The "set of coinductive hypotheses" contains those ancestors of the current
%%% goal that invoke coinductive predicates. It is used by solve/4, and factored
%%% out as an abstract data type to facilitate changing to a more efficient
%%% representation.
%%%
%%% The requirements are as follows:
%%%
%%%       (a) We must be able to check whether a coinductive goal G can be
%%%           unified with one of its ancestors.
%%%
%%%       (b) Ancestors that might be unifiable with G must be available in
%%%           reverse chronological order (i.e., the nearest ancestor first).
%%%
%%%       NOTE: Before checking for unifiability the goals must be passed
%%%             through essence_hook/2.
%%%
%%%
%%% The operations are:
%%%
%%%    push_coinductive( + goal, + stack, - new stack ):
%%%         Push the coinductive goal onto the stack.
%%%
%%%    unify_with_coinductive_ancestor( + goal, + stack ):
%%%         Fail if there is no unifiable coinductive ancestor on the stack. If
%%%         there is one, succeed after performing the unification with the
%%%         most recent such ancestor.  Upon failure undo the unification and
%%%         unify with the next such ancestor, and so on (in reverse
%%%         chronological order), failing after all unifiable ancestors are
%%%         exhausted.



%%--------------  The minimal implementation:  --------------%%
%%
%% The set of coinductive hypotheses is just a list.

:- mode push_coinductive( +, +, - ).

push_coinductive( Goal, Hyp, [ Goal | Hyp ] ).


:- mode unify_with_coinductive_ancestor( +, + ).

unify_with_coinductive_ancestor( Goal, Hyp ) :-
        essence_hook( Goal, Essence ),
        member( G, Hyp ),
        essence_hook( G, Essence ).

%-------------------------------------------------------------------------------
