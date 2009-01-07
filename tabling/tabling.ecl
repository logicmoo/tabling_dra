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

   The interpreter follows -- somewhat loosely -- the description in the latter
   paper but without "semi-naive optimization".  Moreover, "clusters" are
   detected dynamically, to achieve greater precision (a dependency graph among
   static calls can only be a rough approximation, a dependency graph among
   predicates is rougher still).


   Nomenclature
   ------------

   Some predicates are "tabled", because the user has declared them to be such
   by using a directive.  E.g.,
       :- tabled p/2.

   All calls to a tabled predicate that are present in the interpreted program
   are called "tabled calls".  Instances of such calls are called "tabled
   goals".  In general, we will use the term "call" to refer to a static entity
   in the program, and "goal" to refer to an instance of a call.  We will also
   avoid the conventional overloading of the term "goal" in yet another way: we
   will call a sequence (i.e., conjunction) of goals just that (unless we can
   refer to it as a "query" or a "resolvent").


   Limitations
   -----------

   The interpreted program must not contain cuts, disjunctions (i.e.,
   semicolons) or "if-then"/"if-then-else" constructs.  It also must not contain
   calls to built-in-predicates.


   Data structures
   ---------------

   The interpreter uses a number of tables that store information accumulated
   during a computation.  A computation consists in reading a program and
   executing a number of queries.  (A query is a sequence of goals.)

   The tables (implemented as dynamic predicates of Prolog) are:

   -- rule( head, body )

           Used for storing the clauses of the interpreted program.  The body
           must be a non-empty conjuction that ends with 'true' (the conjunction
           operator is the comma, i.e., ",/2" treated as an infix operator).
           Note that the final "true/0" is added automatically to each clause of
           the interpreted program.

   -- tabled( generic head )

           Contains an entry for each predicate that has been declared as
           tabled.  For instance, when the interpreter reads
               :- tabled p/2.
           it stores the fact
               tabled( p( _, _ ) ).

   -- answer( goal, fact )

           Used to store results computed for tabled goals encountered during a
           computation.  Once present, these results are also used during
           further stages of the computation.

           The "fact" 'FAILURE' indicates that a goal fails without producing
           any results.

           In general, a side-effect of each computation will be the generation
           -- for each tabled goal encounted during the computation -- of a set
           of facts that form the goal's "least fixed point interpretation".
           (Of course, if this set is not sufficiently small, the interpreter
           will not terminate successfully.)  The facts (which need not be
           ground!) are all entered into the table "answered", and the members
           of different sets are distinguished by their association with the
           appropriate goal: a fact in "answered" is a result that is valid only
           for a variant of the accompanying goal.

           The need for annotating a fact with information about the
           corresponding goal might not be immediately obvious.  Consider the
           following example (which is simplistic in that the computation itself
           is trivial):

               program:  p( A, A ).
                         p( a, a ).
                         p( a, b ).

               query:    ?-  p( U, V ),  p( W, W ),  p( a, X ),  p( Y, b ).

           During "normal" execution of this Prolog program each goal in the
           query would generate a different set of results; to wit:

               p( U, V )  would generate  p( U, U ), p( a, a ), p( a, b )
               p( W, W )  ..............  p( W, W ), p( a, a )
               p( a, X )  ..............  p( a, a ) (twice!)
               p( Y, b )  ..............  p( b, b ), p( a, b ).

           In other words, the set of results depends not on the predicate, but
           the form of the goal.  If "p/2" is tabled, the proper contents of
           "answer" would be as follows (not necessarily in this order):

               answer( p( U, V ), p( U, U ) ).
               answer( p( U, V ), p( a, a ) ).
               answer( p( U, V ), p( a, b ) ).
               answer( p( W, W ), p( W, W ) ).
               answer( p( W, W ), p( a, a ) ).
               answer( p( a, X ), p( a, a ) ).
               answer( p( Y, b ), p( b, b ) ).
               answer( p( Y, b ), p( a, b ) ).

           Please note that the repetition of p( a, a ) for the goal p( a, X )
           may be avoided.  In general, entries in "answer" will not be variants
           of each other.

   -- number_of_answers( natural number )

           This is a single fact that records the size of "answer".  It is used
           for determining whether new answers have been generated during a
           phase of the computation.

   -- pioneer( goal )

           If a goal is encountered whose variant has not yet been encountered
           during the computation, the goal is called a "pioneer" and recorded
           in this table.  If a variant goal is encountered subsequently, it
           will be treated as a "follower".  The table is used to detect whether
           a goal (when first encountered) is a pioneer or a follower.

   -- not_topmost( goal )

           If a pioneer is determined not to be the "topmost looping goal" in a
           "cluster" of interdependent goals (see ref. [2]), then this is
           recorded in the table.

   -- cluster( goal, list of goals )

           Whenever a "cluster" of interdependent goals is encountered, it is
           entered into this table.  The first argument is the topmost goal in
           the cluster, the list contains the rest.  Please note that clusters
           may be nested, so the topmost goal in a cluster is not necessarily
           the "topmost looping goal" in the sense of ref. [2] (i.e., it may be
           stored in the table "not_topmost").

   -- completed( goal )

           Indicates that the fixpoint for this goal has been computed, and all
           the possible results for variants of the goal can be found in table
           "answer".

*******************************************************************************/



:- [ utilities ].


%% The tables:
:- dynamic rule/2 .
:- dynamic tabled/1 .
:- dynamic answer/2 .
:- dynamic number_of_answers/1 .
:- dynamic pioneer/1 .
:- dynamic not_topmost/1 .
:- dynamic cluster/2 .
:- dynamic completed/1 .

number_of_answers( 0 ).




%%-----  The tables: access and modification  -----

%% memo( + goal, + fact ):
%% If the table "answer" does not contain a variant of this fact paired with
%% a variant of this goal, then add the pair to the table, increasing
%% "number_of_answers".

memo( Goal, Fact ) :-
        answer( G, F ),
        are_variants( F, Fact ),
        are_variants( G, Goal ),
        !.

memo( Goal, Fact ) :-
        % No variant pair in "answer",
        assert( answer( Goal, Fact ) ),
        retract( number_of_answers( N ) ),
        N1 is N + 1,
        assert( number_of_answers( N1 ) ).



%% get_answer( +- goal ):
%% Get an instantiation (if any) tabled in "answer" for variants of this goal.
%% Sequence through all such instantiations on backtracking.

get_answer( Goal ) :-
        answer( G, Ans ),
        are_variants( Goal, G ),
        Goal = Ans .



%% complete_goal( + goal ):
%% Make sure the goal is marked as completed.

complete_goal( Goal ) :-
        is_completed( Goal ),
        !.

complete_goal( Goal ) :-
        % \+ is_completed( Goal ),
        assert( completed( Goal ) ).



%% is_completed( + goal ):
%% Succeeds iff the goal is a variant of a goal that has been stored in
%% the table completed.

is_completed( Goal ) :-
        completed( CG ),
        are_variants( Goal, CG ).



%% complete_cluster( + goal ):
%% If the goal has an associated cluster, make sure all the goals in the cluster
%% are marked as complete.

complete_cluster( Goal ) :-
        cluster( G, Gs ),
        are_variants( G, Goal ),
        !,
        complete_goal( Goal ),
        complete_goals( Gs ).

complete_cluster( _ ).

%
complete_goals( Gs ) :-
        member( G, Gs ),
        complete_goal( G ),
        fail.

complete_goals( _ ).




%% is_a_pioneer( + goal ):
%% Succeeds if the goal is not a variant of another goal that has already been
%% encountered during this computation.
%%
%% SIDE EFFECT: adds the goal to table "pioneer".

is_a_pioneer( Goal ) :-
        \+ ( pioneer( PG ),  are_variants( Goal, PG ) ),
        assert( pioneer( Goal ) ).



%% mk_not_topmost( + goal ):
%% Make sure that the goal is stored in "not_topmost".

mk_not_topmost( Goal ) :-
        \+ is_topmost( Goal ),
        !.

mk_not_topmost( Goal ) :-
        % is_topmost( Goal ),
        assert( not_topmost( Goal ) ).



%% is_topmost( + goal ):
%% Succeeds iff the goal is not a variant of a goal that has been saved in
%% table "not_topmost".

is_topmost( Goal ) :-
        \+ ( not_topmost( G ),  are_variants( Goal, G ) ).





%%-----  Custom-tailored utilities  -----

%% fatal_error( + message, + stack ):
%% Display the message and stack, then abort.

fatal_error( Message, Stack ) :-
        write( "---- FATAL ERROR: " ),
        nl,
        write( "---- " ),
        write( Message ),
        nl,
        nl,
        write( "---- The current stack:" ),
        nl,
        show_stack( Stack ),
        write( "----" ),
        nl,
        abort.

%
show_stack( [] ).
show_stack( [ H | T ] ) :-
        write( H ),
        nl,
        show_stack( T ).



%%%%  The top level  %%%%

go :-   putchars( "What is the name of the program file? " ),
        getline( Name ),
        putchars( "opening " ),  putchars( Name ),
        open( Name, read, ProgStream ),
        putline( "OK!" ).



%%%%  Start the interpreter.  %%%%

% :- go.
