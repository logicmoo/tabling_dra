%%%                                                                        %%%
%%%  A meta-interpreter for tabled logic programming: see the description  %%%
%%%  below for more information.                                           %%%
%%%  Written by Feliks Kluzniak at UTD.                                    %%%
%%%                                                                        %%%
%%%  Last update: 28 January 2009.                                         %%%
%%%                                                                        %%%

%%% NOTE:
%%%
%%%    1. See ../general/top_level.ecl for a description of how to load
%%%       and run programs.
%%%
%%%    2. A tabled predicate should be declared as such in the program
%%%       file, e.g.,
%%%           :- tabled ancestor/2 .
%%%
%%%       To include files use the usual Prolog syntax:
%%%           :- [ file1, file2, ... ].
%%%
%%%    2. The program should contain no other directives. It may, however,
%%%       contain queries, which will be executed immediately upon reading.
%%%
%%%    3. If the program invokes a built-in predicate, that predicate must
%%%       be declared in the table builtin/1 below.  Every addition should
%%%       be considered carefully: some might require special treatment by
%%%       the metainterpreter.

%%% LIMITATIONS: - The interpreted program should not contain cuts.
%%%              - Error detection is quite rudimentary.


/*******************************************************************************

General description
   -------------------

   A simple (and very inefficient) metainterpreter that attempts to emulate
   "top-down tabled programming", as described in

     [1] Hai-Feng Guo, Gopal Gupta:
         Tabled Logic Programming with Dynamic Ordering of Alternatives
         (17th ICLP, 2001)

     [2] Neng-Fa Zhou, Taisuke Sato, Yi-Dong Shen:
         Linear Tabling Strategies and Optimizations
         (TPLP 2008 (?))

   The interpreter follows -- somewhat loosely(*) -- the description in the
   latter paper, using the "lazy strategy", but without "semi-naive
   optimization".
   Moreover, "clusters" are detected dynamically, to achieve greater precision
   (a dependency graph among static calls can only be a rough approximation, a
   dependency graph among predicates is rougher still).

   The "lazy strategy" consists in eagerly tabling answers to the subgoals
   encountered during the evaluation of a query.

   [(*) The main difference with respect to the paper is that pioneers that are
        not topmost looping goals are not treated in a special manner, so more
        re-evaluation may occur.  In this program, the term "pioneer" is
        used to denote a "topmost looping pioneer".
   ]


   Nomenclature
   ------------

   Some predicates are "tabled", because the user has declared them to be such
   by using a directive.  E.g.,

       :- tabled p/2 .

   All calls to a tabled predicate that are present in the interpreted program
   are called "tabled calls".  Instances of such calls are called "tabled
   goals".  In general, we will use the term "call" to refer to a static entity
   in the program, and "goal" to refer to an instance of a call.  We will also
   avoid the conventional overloading of the term "goal" in yet another way: we
   will call a sequence (i.e., conjunction) of goals just that (unless we can
   refer to it as a "query" or a "resolvent").


   Limitations
   -----------

   The interpreted program must not contain cuts or "if-then"/"if-then-else"
   constructs.  It also must not contain calls to built-in-predicates, except
   for the handful of predicates listed in builtin/1 below.


   Data structures
   ---------------

   The interpreter uses a number of tables that store information accumulated
   during a computation.  A computation consists in reading a program and
   executing a number of queries.  (A query is a sequence of goals.)

   The tables (implemented as dynamic predicates of Prolog) are:

   -- tabled( generic head )

           Contains an entry for each predicate that has been declared as
           tabled.  For instance, when the interpreter reads
               :- tabled p/2 .
           it stores the fact
               tabled( p( _, _ ) ).

   -- answer( goal, fact )

           Used to store results computed for tabled goals encountered during a
           computation.  Once present, these results are also used during
           further stages of the computation.

           Note that the fact is an instantiation of the goal.  If a tabled
           goal has no solutions, it will have no entry in "answer", even though
           it may have an entry in "completed" (see below).

           In general, a side-effect of each evaluation of a query will be the
           generation -- for each tabled goal encounted during the evaluation
           -- of a set of facts that form the goal's "least fixed point
           interpretation".  (Of course, if this set is not sufficiently small,
           the interpreter will not terminate successfully.)  The facts
           (which need not be ground!) are all entered into the table
           "answered", and the members of different sets are distinguished by
           their association with the appropriate goal: a fact in "answered"
           is a result that is valid only for a variant of the accompanying
           goal.

           The need for annotating a fact with information about the
           corresponding goal might not be immediately obvious.  Consider the
           following example (which is simplistic in that the computation itself
           is trivial):

               program:  p( A, A ).
                         p( a, a ).
                         p( a, b ).

               queries:  ?-  p( U, V ).
                         ?-  p( W, W ).
                         ?-  p( a, X ).
                         ?-  p( Y, b ).

           During "normal" execution of this Prolog program each of the queries
           would generate a different set of results; to wit:

               p( U, V )  would generate  p( U, U ), p( a, a ), p( a, b )
               p( W, W )  ..............  p( W, W ), p( a, a )
               p( a, X )  ..............  p( a, a ) (twice!)
               p( Y, b )  ..............  p( b, b ), p( a, b ).

           In other words, the set of results depends not only on the predicate,
           but also on the form of the goal. In particular, "p( b, b )" is a
           valid answer only for goals whose second argument is "b".

           If "p/2" is tabled, the proper contents of "answer" would be as
           follows (not necessarily in this order):

               answer( p( U, V ), p( U, U ) ).
               answer( p( U, V ), p( a, a ) ).
               answer( p( U, V ), p( a, b ) ).
               answer( p( W, W ), p( W, W ) ).
               answer( p( W, W ), p( a, a ) ).
               answer( p( a, X ), p( a, a ) ).
               answer( p( Y, b ), p( b, b ) ).
               answer( p( Y, b ), p( a, b ) ).

           Please note that the repetition of p( a, a ) for the goal p( a, X )
           will be avoided.  In general, entries in "answer" will not be
           variants of each other.

   -- number_of_answers

           This is a non-logical variable that  records the size of "answer".
           It is used for determining whether new answers have been generated
           during a phase of the computation.

   -- pioneer( goal, index )

           If the current goal is tabled, and its variants have not yet been
           encountered during the evaluation of the current query, the goal
           is called a "pioneer" and recorded in this table.  (An unique index
           is also stored.)
           If a variant goal is encountered subsequently, it will be treated
           as a "follower".
           The table is used to detect whether a tabled goal (when first
           encountered) is a pioneer or a follower.
           If a pioneer is determined not to be the "topmost looping goal" in a
           "cluster" of interdependent goals (see ref. [2]), then it loses the
           status of a pioneer.  This is because a pioneer is expected to
           compute the fixpoint (by tabling answers) for itself and its cluster
           before succeeding: the property is the reason why followers can query
           only "answer", and do not use their clauses.
           Note that no two entries in "pioneer" are variants of each other.
           This table is cleared each time the evaluation of a query terminates.

   -- pioneer_index

           This is a non-logical variable that holds the index to be used for
           the next entry in "pioneer".

   -- loop( pioneer goal, list of goals, index )

           A loop is discovered when the current goal is a variant of its
           ancestor.  If the ancestor is a pioneer, information about the
           pioneer ancestor and the tabled goals between the pioneer and
           the variant is stored in "loop".  (An unique index is also stored.)
           A number of "loop" entries may exist for a given pioneer: together,
           they describe a "cluster" (see ref. [2]).  Before returning any
           answers, a pioneer will compute its own fixpoint as well as
           the fixpoints of the goals in its cluster.
           When a goal loses its pioneer status (because it is determined to
           be a part of a larger loop), the associated entries in "loop" are
           removed.
           This table is cleared each time the evaluation of a query terminates.

   -- loop_index

           This is a non-logical variable that holds the index to be used for
           the next entry in "loop".

   -- completed( goal )

           Indicates that the fixpoint for this tabled goal has been computed,
           and all the possible results for variants of the goal can be found
           in table "answer".

*******************************************************************************/


:- ensure_loaded( [ '../general/top_level',
                    '../general/utilities'
                  ]
                ).




% If a file name has no extension, add ".tlp"

default_extension( ".tlp" ).


%% Initialization of tables:

:- dynamic tabled/1 .
:- dynamic answer/2 .
:- dynamic pioneer/2 .
:- dynamic loop/3 .
:- dynamic completed/1 .

:- setval( number_of_answers, 0 ).
:- setval( pioneer_index,     0 ).
:- setval( loop_index,        0 ).

initialise :-
        retractall( tabled( _ )     ),
        retractall( answer( _, _ )  ),
        retractall( pioneer( _, _ ) ),
        retractall( loop( _, _, _ ) ),
        retractall( completed( _ )  ),
        setval( number_of_answers, 0 ),
        setval( pioneer_index,     0 ),
        setval( loop_index,        0 ).



%%%%%  Built-in predicates  %%%%
%%
%%  NOTE: Just adding "!" won't do the trick, the main metainterpreter would
%%        have to be modified substantially.
%%        Certain other built-ins may also require special treatment.

builtin( true             ).
builtin( false            ).
builtin( fail             ).
builtin( \+( _ )          ).  % there is special treatment for this, see solve/2
builtin( once _           ).  % there is special treatment for this, see solve/2
builtin( (_ -> _ ; _)     ).  % there is special treatment for this, see solve/2
builtin( (_ ; _)          ).  % there is special treatment for this, see solve/2
builtin( (_ , _)          ).  % there is special treatment for this, see solve/2
builtin( _ = _            ).
builtin( _ \= _           ).
builtin( _ > _            ).
builtin( _ >= _           ).
builtin( _ =< _           ).
builtin( _ < _            ).
builtin( _ is _           ).
builtin( atom( _ )        ).
builtin( write( _ )       ).
builtin( writeln( _ )     ).
builtin( nl               ).
builtin( read( _ )        ).
builtin( set_flag( _, _ ) ).
builtin( member( _, _ )   ).




%%%%%  Administration  %%%%%

:- op( 1000, fy, tabled ).    % allow  ":- tabled p/k ."



%% The legal directives (check external form only).

legal_directive( tabled _ ).


%% Check and process the legal directives

execute_directive( tabled P / K ) :-                  % declaration of tabled
        (atom( P ), integer( K ), K >= 0),            %  seems OK
        !,
        mk_pattern( P, K, Pattern ),                  % Pattern = P( _, _, ... )
        assert( tabled( Pattern ) ).

execute_directive( tabled P / K ) :-                  % declaration of tabled
        (\+ atom( P ) ; \+ integer( K ) ; K < 0),     %  obviously wrong
        !,
        warning( [ "Erroneous directive: \"",
                   (:- tabled P / K),
                   "\" ignored! +++"
                 ]
               ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%  The meta-interpreter  %%%%%


%% Execute a query.

:- mode query( + ).

query( Goals ) :-
        solve( Goals, [] ),
        retractall( pioneer( _, _ )  ),
        retractall( loop( _, _ , _ ) ),
        setval( pioneer_index, 0 ),
        setval( loop_index,    0 ).




%% solve( + sequence of goals, + stack ):
%% Solve the sequence of goals, maintaining information about the current chain
%% of ancestors (stack).
%%
%% Please note that the following invariant must be maintained:
%%    A pioneer is
%%    (a) either an active goal (i.e., the current goal, or present in the
%%        chain of ancestors);
%%    (b) or marked as completed.

:- mode solve( +, + ).


% Note that even during the computation of \+/1 a whole set of answers
% may become tabled.

solve( \+ Goal, Stack ) :-
        !,
        \+ solve( Goal, Stack ).


% Note that even during the computation of once/1 a whole set of answers
% may become tabled.

solve( once Goal, Stack ) :-
        !,
        solve( Goal, Stack ),
        !.


% A conditional.

solve( (Cond -> Then ; _Else), Stack ) :-
        solve( Cond, Stack ),
        !,
        solve( Then, Stack ).

solve( (_Cond -> _Then ; Else), Stack ) :-
        !,
        solve( Else, Stack ).


% A disjunction without a conditional.

solve( (Goals ; _), Stack ) :-
        solve( Goals, Stack ).

solve( (_ ; Goals), Stack ) :-
        !,
        solve( Goals, Stack ).


% A conjunction.

solve( (Goals1 , Goals2), Stack ) :-
        !,
        solve( Goals1, Stack ),
        solve( Goals2, Stack ).


% Some other supported built-in.

solve( BuiltIn, _ ) :-
        builtin( BuiltIn ),
        !,
        call( BuiltIn ).


% A "normal" (i.e., not tabled) goal.

solve( Goal, Stack ) :-
        \+ tabled( Goal ),
        !,
        solve_by_rules( Goal, Stack ).


% A tabled goal that has been completed: all the results are in "answer".

solve( Goal, _ ) :-
        is_completed( Goal ),
        !,
        get_answer( Goal ).


% A tabled goal that has a variant among its ancestors.
% See the comment to variant_of_ancestor for a more detailed description of
% the actions taken.
% Only the existing (most likely incomplete) results from "answer" are
% returned before failure.

solve( Goal, Stack ) :-
        variant_of_ancestor( Goal, Stack ),
        !,
        get_answer( Goal ).


% A pioneer goal is solved by rules, producing results that are stored in
% "answer": after this is done, "answer" is used to pass on the results.
%
% Moreover, the goal's answer set is extended to the least fixed point and its
% cluster is marked as complete.
%
% (Note that a pioneer but may cease to be one when some descendant goal finds
%  a variant ancestor that is also an ancestor of the pioneer.
%  See variant_of_ancestor/2.)

solve( Goal, Stack ) :-
        \+ is_a_variant_of_a_pioneer( Goal ),
        !,
        add_pioneer( Goal ),
        store_all_solutions_by_rules( Goal, Stack ),
        (
            is_a_variant_of_a_pioneer( Goal )      % might have lost its status!
        ->
            compute_fixed_point( Goal, Stack ),
            complete_cluster( Goal )
        ;
            true
        ),
        get_answer( Goal ).


% A tabled goal that is not completed, not a pioneer on entry, and has no
% variant among its ancestors.  Something is wrong!

solve( Goal, Stack ) :-
        fatal_error( "IMPOSSIBLE!", [ Goal| Stack ] ).




%% store_all_solutions_by_rules( + goal, + stack ):
%% Invoke solve_by_rules/2 until there are no solutions left, storing
%% the results in "answer".

:- mode store_all_solutions_by_rules( +, + ).

store_all_solutions_by_rules( Goal, Stack ) :-
        copy_term( Goal, OriginalGoal ),
        solve_by_rules( Goal, Stack ),
        memo( OriginalGoal, Goal ),
        fail.

store_all_solutions_by_rules( _, _ ).



%% solve_by_rules( + goal, + stack ):
%% Solves the goal by using rules (i.e., clauses) only.

:- mode solve_by_rules( +, + ).

solve_by_rules( Goal, Stack ) :-
        copy_term( Goal, OriginalGoal ),
        clause( Goal, Body )@interpreted,
        solve( Body, [ OriginalGoal | Stack ] ).




%% compute_fixed_point( + goal, + stack ):
%% Solve the goal by rules until no more answers are produced, then succeed
%% _without_ instantiating the goal.

:- mode compute_fixed_point( +, + ).

compute_fixed_point( Goal, Stack ) :-
        getval( number_of_answers, NAns ),
        compute_fixed_point_( Goal, Stack, NAns ).

%
:- mode compute_fixed_point_( +, +, + ).

compute_fixed_point_( Goal, Stack, _ ) :-
        store_all_solutions_by_rules( Goal, Stack ),            % all solutions
        fail.

compute_fixed_point_( _, _, NAns ) :-
        getval( number_of_answers, NAns ),                      % no new answers
        !.

compute_fixed_point_( Goal, Stack, NAns ) :-
        getval( number_of_answers, NA ),
        NA =\= NAns,                                            % new answers,
        compute_fixed_point_( Goal, Stack, NA ).                %   so iterate



%% variant_of_ancestor( + goal, + list of goals ):
%% Succeeds if the goal is a variant of some member of the list.
%%
%% SIDE EFFECT: If successful, then intermediate pioneer goals will lose their
%%              status as pioneers, and the associated entries in "loop" will
%%              be removed.  Moreover, if the variant ancestor is a pioneer,
%%              the entire prefix of the list upto (and including) the variant
%%              ancestor will be added to the cluster of that ancestor (by
%%              storing it in "loop"), after filtering out goals that are not
%%              tabled.

:- mode variant_of_ancestor( +, + ).

variant_of_ancestor( Goal, List ) :-
        append( Prefix, [ G | _ ], List ),                % i.e., split the list
        are_variants( Goal, G ),
        keep_tabled( Prefix, TabledPrefix ),
        add_loop( G, TabledPrefix ),
        (
            member( M, TabledPrefix ),
            rescind_pioneer_status( M ),
            fail
        ;
            true
        ).


%% keep_tabled( + list of goals, - list of goals ):
%% Filter away goals that are not tabled.

:- mode keep_tabled( +, - ).

keep_tabled( [], [] ).

keep_tabled( [ G | Gs ], [ G | TGs ] ) :-
        tabled( G ),
        !,
        keep_tabled( Gs, TGs ).

keep_tabled( [ _G | Gs ], TGs ) :-
        % \+ tabled( G ),
        keep_tabled( Gs, TGs ).


%% rescind_pioneer_status( + goal ):
%% If the goal is tabled in "pioneer", remove the entry and the associated
%% cluster (i.e., entries in "loop").

:- mode rescind_pioneer_status( + ).

rescind_pioneer_status( Goal ) :-
        is_a_variant_of_a_pioneer( Goal ),
        !,
        remove_pioneer( Goal ),
        remove_loops( Goal ).

rescind_pioneer_status( _ ).


%% complete_cluster( + goal ):
%% If the goal has an associated cluster, make sure all the goals in the cluster
%% are marked as complete.  If there is no associated cluster, just mark the
%% goal as complete.
%% Recall that a cluster may consist of a number of "loops".

:- mode complete_cluster( + ).

complete_cluster( Goal ) :-
        complete_goal( Goal ),
        complete_cluster_if_any( Goal ).

%
:- mode complete_cluster_if_any( + ).

complete_cluster_if_any( Goal ) :-
        loop( G, Gs, _ ),
        are_variants( G, Goal ),
        complete_goals( Gs ),
        fail.

complete_cluster_if_any( _ ).

%
:- mode complete_goals( + ).

complete_goals( Gs ) :-
        member( G, Gs ),
        complete_goal( G ),
        fail.

complete_goals( _ ).





%%-----  The tables: access and modification  -----


%% memo( + goal, + fact ):
%% If the table "answer" does not contain a variant of this fact paired with
%% a variant of this goal, then add the pair to the table, increasing
%% "number_of_answers".

:- mode memo( +, + ).

memo( Goal, Fact ) :-
        answer( G, F ),
        are_variants( F, Fact ),
        are_variants( G, Goal ),
        !.

memo( Goal, Fact ) :-
        % No variant pair in "answer",
        assert( answer( Goal, Fact ) ),
        incval( number_of_answers ).



%% get_answer( +- goal ):
%% Get an instantiation (if any) tabled in "answer" for variants of this goal.
%% Sequence through all such instantiations on backtracking.

:- mode get_answer( ? ).

get_answer( Goal ) :-
        answer( G, Ans ),
        are_variants( Goal, G ),
        Goal = G,                 % make sure that variables are the right ones
        Goal = Ans .              % instantiate



%% complete_goal( + goal ):
%% Make sure the goal is marked as completed.

:- mode complete_goal( + ).

complete_goal( Goal ) :-
        is_completed( Goal ),
        !.

complete_goal( Goal ) :-
        % \+ is_completed( Goal ),
        assert( completed( Goal ) ).



%% is_completed( + goal ):
%% Succeeds iff the goal is a variant of a goal that has been stored in
%% the table "completed".

:- mode is_completed( + ).

is_completed( Goal ) :-
        completed( G ),
        are_variants( Goal, G ).



%% is_a_variant_of_a_pioneer( + goal ):
%% Succeeds if the goal is a variant of another goal that is tabled in
%% "pioneer".

:- mode is_a_variant_of_a_pioneer( + ).

is_a_variant_of_a_pioneer( Goal ) :-
        pioneer( PG, _Index ),
        are_variants( Goal, PG ).



%% make_a_pioneer( + goal ):
%% Add an entry for this goal to "pioneer".

:- mode add_pioneer( + ).

add_pioneer( Goal ) :-
        getval( pioneer_index, NewIndex ),
        incval( pioneer_index ),
        assert( pioneer( Goal, NewIndex ) ).



%% remove_pioneer( + goal ):
%% Remove the pioneer entry for this goal.

:- mode remove_pioneer( + ).

remove_pioneer( Goal ) :-
        pioneer( G, Index ),
        are_variants( G, Goal ),
        once retract( pioneer( _, Index ) ).



%% add_loop( + goal, + list of goals ):
%% Add an entry to "loop".

:- mode add_loop( +, + ).

add_loop( Goal, Goals ) :-
        getval( loop_index, NextIndex ),
        incval( loop_index ),
        assert( loop( Goal, Goals, NextIndex ) ).



%% remove_loops( + goal ):
%% Remove all entries in "loop" that are associated with this goal.

:- mode remove_loops( + ).

remove_loops( Goal ) :-
        loop( G,_, Indx ),
        are_variants( G, Goal ),
        once retract( loop( _, _, Indx ) ),
        fail.

remove_loops( _ ).





%%-----  Custom-tailored utilities  -----


%% fatal_error( + message, + stack ):
%% Display the message and stack, then abort.

:- mode fatal_error( +, + ).

fatal_error( Message, Stack ) :-
        begin_error,
        writeln(    error, Message ),
        writeln(    error, "" ),
        writeln(    error, "*** The current stack:" ),
        show_stack( error, Stack ),
        end_error.

%
show_stack( Stream, Stack ) :-
        member( Call, Stack ),
        writeln( Stream, Call ),
        fail.

show_stack( _ ).
