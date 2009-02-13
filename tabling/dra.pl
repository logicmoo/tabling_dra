%%%                                                                          %%%
%%%  An interpreter for tabled logic programming with coinduction:           %%%
%%%  see the description below for more information.                         %%%
%%%  Written by Feliks Kluzniak at UTD (January-February 2009).              %%%
%%%                                                                          %%%
%%%  Last update: 11 February 2009.                                          %%%
%%%                                                                          %%%

%%% NOTE:
%%%
%%%    1. See ../general/top_level.ecl for a description of how to load
%%%       and run programs.
%%%
%%%    2. Tabled and coinductive predicates should be declared as such in
%%%       the program file, e.g.,
%%%           :- tabled      ancestor/2 .
%%%           :- coinductive comember/2 .
%%%
%%%       To include files use the usual Prolog syntax:
%%%           :- [ file1, file2, ... ].
%%%
%%%       To declare predicates used in an interpreted program as dynamic, use
%%%           :- dynamic p/k.
%%%
%%%       To produce a wallpaper trace use the trace directive. For example,
%%%           :- trace p/3, q/0, r/1.
%%%       will trace predicates "p/3", "q/0" and "r/1".  If you want to trace
%%%       everything, use
%%%           :- trace all.
%%%       These directives are cumulative.
%%%
%%%    2. The program should contain no other directives. It may, however,
%%%       contain queries, which will be executed immediately upon reading.
%%%
%%%    3. If the program invokes a built-in predicate, that predicate must
%%%       be declared in the table "builtin/1" below.  Every addition should
%%%       be considered carefully: some might require special treatment by
%%%       the interpreter.
%%%
%%%    4. The program may contain clauses that modify the definition of the
%%%       interpreter's predicate "essence_hook/2" (the clauses will be asserted
%%%       at the front of the predicate, and will thus override the default
%%%       definition for some cases).  The default definition is
%%%
%%%          essence_hook( T, T ).
%%%
%%%       This predicate is invoked _in certain contexts_ when:
%%%          - two terms are about to be compared (either for equality or to
%%%            check whether they are variants of each other);
%%%          - an answer is tabled;
%%%          - an answer is retrieved from the table.
%%%
%%%       The primary intended use is to suppress arguments that carry only
%%%       administrative information and that may differ in two terms that are
%%%       "semantically" equal or variants of each other. (Such, for example, is
%%%       the argument that carries the set of coinductive hypotheses in a
%%%       co-logic program translated into Prolog: see "../coind/translate_clp".
%%%       Mind you, that translation need not be applied to programs executed by
%%%       this interpreter).
%%%
%%%       For example, the presence of
%%%
%%%          essence_hook( p( A, B, _ ),  p( A, B ) ).
%%%
%%%       will result in "p( a, b, c )" and "p( a, b, d )" being treated as
%%%       identical, as each of them will be translated to "p( a, b )" before
%%%       comparison.
%%%
%%%       NOTE: This facility should be used with the utmost caution, as it
%%%             may drastically affect the semantics of the interpreted program
%%%             in a fashion that would be hard to understand for someone who
%%%             does not understand the details of the interpreter.
%%%
%%% LIMITATIONS: - The interpreted program should not contain cuts.
%%%              - Error detection is quite rudimentary.




/*******************************************************************************

   General description
   -------------------

   A simple (and very inefficient) interpreter that attempts to emulate
   "top-down tabled programming", as described in

     [1] Hai-Feng Guo, Gopal Gupta:
         Tabled Logic Programming with Dynamic Ordering of Alternatives
         (17th ICLP, 2001)

   The terminology has been modified under the influence of

     [2] Neng-Fa Zhou, Taisuke Sato, Yi-Dong Shen:
         Linear Tabling Strategies and Optimizations
         (TPLP 2008 (?))

   More specifically, "masters" are called "pioneers" (although in a sense
   somewhat different than in [2]: we use "pioneer" for "topmost looping goal"),
   and "strongly connected components" are called "clusters".

   Note that "clusters" are detected dynamically, to achieve greater precision
   (a dependency graph among static calls can only be a rough approximation, a
   dependency graph among predicates is rougher still).


   Nomenclature
   ------------

   Some predicates are "tabled", because the user has declared them to be such
   by using an appropriate directive, e.g.,

       :- tabled p/2 .

   All calls to a tabled predicate that are present in the interpreted program
   are called "tabled calls".  Instances of such calls are called "tabled
   goals".  In general, we will use the term "call" to refer to a static entity
   in the program, and "goal" to refer to an instance of a call.  We will also
   avoid the conventional overloading of the term "goal" in yet another way: we
   will call a sequence (i.e., conjunction) of goals just that (unless we can
   refer to it as a "query" or a "resolvent").

   Similarly, the user can declare a predicate to be "coinductive", by using
   another kind of directive, e.g.,

       :- coinductive p/2 .

   Calls and goals that refer to a coinductive predicate will also be called
   "coinductive".


   Limitations
   -----------

   The interpreted program must not contain cuts.  It also must not contain
   calls to built-in-predicates, except for the handful of predicates listed in
   builtin/1 below.  (This list can be easily extended as the need arises.  Some
   built-in predicates, however, cannot be added without modifying the
   interpreter, sometimes extensively: "!/0" is a good example.)



   Data structures
   ---------------

   The interpreter uses a number of tables that store information accumulated
   during a computation.  A computation consists in reading a program and
   executing a number of queries.  A query is a sequence (i.e., conjunction) of
   goals.

   The tables (implemented as dynamic predicates of Prolog) are:


   -- coinductive( generic head )

           Contains an entry for each predicate that has been declared as
           coinductive.  For instance, when the interpreter reads
               :- coinductive p/2 .
           it stores the fact
               coinductive( p( _, _ ) ).

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

           Note that the fact is an instantiation of the goal.  If a tabled goal
           has no solutions, it will have no entry in "answer", even though it
           may have an entry in "completed" (see below).

           (NOTE: In the actual implementation each fact in "answer" has the
                  form
                     answer( cgoal, goal, fact )
                  where "cgoal" is a copy of "goal" (no shared variables).
                  This is done to facilitate more effective filtering (via
                  unification) before a check is made for whether "goal" is a
                  variant of the goal for which we are seeking a tabled answer.
           )

           This table is not cleared before the evaluation of a new query.

           Detailed comments:
           ..................
           In general, for each success of a tabled goal encountered during the
           evaluation of a query, the interpreter will make certain that the
           result, i.e., the successful instantiation of that goal (which need
           not be ground!) is stored in the table "answer", accompanied by a
           variant of the original version of the goal (i.e., as it appeared
           when it was first encountered).

           Before a query finally fails (after exhausting all the answers),
           tabled goals encountered during its evaluation will have computed
           their least fixed points, i.e., all the possible results for those
           goals will be stored in "answer".  (Of course, if this set of all
           answers is not sufficiently small, the interpreter will not terminate
           successfully.)

           Results stored in "answer" can be picked up during later evaluation
           but each of them is valid only for a variant of the accompanying
           goal.

           The need for associating a fact with information about the
           corresponding goal might not be immediately obvious.  Consider the
           following example (which is simplistic in that the computation itself
           is trivial):

               program:  :- tabled p/2.
                         p( A, A ).
                         p( a, b ).

               queries:  ?-  p( U, V ).
                         ?-  p( Y, b ).

           During "normal" execution of this Prolog program each of the queries
           would generate a different set of results; to wit:

               p( U, V )  would generate  p( U, U ), p( a, b );
               p( Y, b )  ..............  p( b, b ), p( a, b ).

           In other words, the set of results depends not only on the predicate,
           but also on the form of the goal.

           If these results were tabled without the corresponding goals, then
           the table would be:

               answer( p( U, U ) ).
               answer( p( a, b ) ).
               answer( p( b, b ) ).

           A subsequent invocation of p( U, V ) would then return all three
           results, i.e., also "p( b, b )"!

           The proper contents of "answer" should be as follows (though not
           necessarily in this order):

               answer( p( U, V ), p( U, U ) ).
               answer( p( U, V ), p( a, b ) ).
               answer( p( Y, b ), p( b, b ) ).
               answer( p( Y, b ), p( a, b ) ).

           Please note that two different entries in "answer" will not be
           variants of each other.

   -- number_of_answers

           This is a non-logical variable that records the size of "answer".  It
           is useful for determining whether new answers have been generated
           during a phase of the computation.

           This variable is not cleared before the evaluation of a new query.


   -- pioneer( goal, index )

           If the current goal is tabled, and it is not a variant of any of its
           ancestors, then the goal is called a "pioneer" and obtains an "index"
           (i.e., an unique identifier). Both the goal and its index are
           recorded in this table.

           The role of a pioneer is to compute the fixpoint (by tabling answers)
            for itself and its cluster before failing: this is why the results
           for its variant descendants can be obtained simply by querying
           "answer", without using their clauses (which prevents endless
           recursion).

           If a pioneer is later determined not to be the "topmost looping goal"
           in a "cluster" of interdependent goals (see ref. [2]), then it loses
           the status of a pioneer, and its role will be overtaken by the
           topmost goal in the cluster.  (This can happen if one of the
           descendants of a pioneer turns out to be a variant of one of its
           ancestors.)

           A pioneer also loses its status if its fixpoint has been computed: it
           then becomes a "completed" goal (and all its variants become
           completed).

           A pioneer "G" may also lose its status because another goal "G'",
           encountered after "G" succeeds without yet becoming completed, has
           become completed: if "G'" is a variant of "G", thne "G" becomes
           completed as well.

           When a pioneer loses its status, the associated entries in "pioneer",
           "loop" and "looping_alternative" (see below) are removed.  The
           associated entries in "result" are not removed. The unique index is
           not reused for other goals during the evaluation of the current
           query.

           This table is cleared before the evaluation of a new query.

           (NOTE: In the actual implementation each fact in "pioneer" has the
                  form
                     pioneer( cgoal, goal, index )
                  where "cgoal" is a copy of "goal" (no shared variables).
                  This is done to facilitate more effective filtering (via
                  unification) before a check is made for whether "goal" is a
                  variant of the goal for which we are checking whether it is
                  (still) a pioneer.
           )

   -- unique_index

           This is a non-logical variable that holds the index to be used for
           the next entry in "pioneer".  It is also used to generate unique
           indices for coinductive goals, which might need them to hold their
           own results in "result".

           The variable is cleared before the evaluation of a new query.


   -- result( index, fact )

           A tabled goal "G" that "started out" as a pioneer may have associated
           entries (marked with the unique index of "G") in "result".  This
           table records the instantiations of "G" that were returned as "G"
           succeeded.  By using the table, the interpreter prevents "G" from
           returning the same answer over and over again: in general, each
           tabled goal will not produce two results that are variants of each
           other.

           When a goal loses its pioneer status (because it is determined to be
           a part of a larger loop, or because it has become completed), the
           associated entries in "result" are not removed.  They are removed
           only when the goal finally fails.

           The table is also used by coinductive goals that are not pioneers.

           This table is cleared before the evaluation of a new query.


   -- loop( index, list of goals )

           A loop is discovered when the current tabled goal is a variant of one
           of its ancestors.  If the ancestor is a pioneer, the unique index of
           the pioneer ancestor and a list of the tabled goals between the
           pioneer and the variant are stored in "loop".

           A number of "loop" entries may exist for a given pioneer: together,
           they describe a "cluster" (i.e., a "strongly connected component",
           see ref. [1]).  Before finally failing upon backtracking, a pioneer
           will compute its own fixpoint as well as the fixpoints of the goals
           in its cluster.

           When a goal loses its pioneer status (because it is determined to be
           a part of a larger loop, or because it has become completed), the
           associated entries in "loop" are removed.

           This table is cleared before the evaluation of a new query.


   -- looping_alternative( index, clause )

           When a goal "G" is determined to be a variant descendant of a
           pioneer, the clause that is currently being used by the pioneer
           (i.e., the clause that led to "G") is stored in this table, together
           with the unique index of the pioneer.  "G" will then succeed only
           with answers that have been tabled so far, but the clause will be
           used again as backtracking brings the computation back to the
           pioneer.  (This is the essence of the "dynamic reordering of
           alternatives".)

           When a goal loses its pioneer status (because it is determined to be
           a part of a larger loop, or because it has become completed), the
           associated entries in "looping_alternative" are removed.

           This table is cleared before the evaluation of a new query.


   -- completed( goal )

           Indicates that this tabled goal is completed, i.e., its fixpoint has
           been computed, and all the possible results for variants of the goal
           can be found in table "answer".  Variants of a completed goal are
           completed as well.

           This table is not cleared before the evaluation of a new query.

           (NOTE: In the actual implementation each fact in "completed" has the
                  form
                     completed( cgoal, goal )
                  where "cgoal" is a copy of "goal" (no shared variables).
                  This is done to facilitate more effective filtering (via
                  unification) before a check is made for whether "goal" is a
                  variant of the goal for which we are checking whether it is
                  completed.
           )


   -- tracing( goal )

           A goal that matches something in this table will show up on the
           wallpaper trace.  This table is empty by default, and filled only
           by invocations of "trace" (most often in "trace" directives
           encountered when the interpreted program is being read).

*******************************************************************************/




:- ensure_loaded( [ '../general/top_level',
                    '../general/utilities'
                  ]
                ).



% If a file name has no extension, add ".tlp"

default_extension( ".tlp" ).


%% Initialization of tables:

:- dynamic coinductive/1 .
:- dynamic tabled/1 .
:- dynamic answer/3 .
:- dynamic pioneer/3 .
:- dynamic result/2 .
:- dynamic loop/2 .
:- dynamic looping_alternative/2 .
:- dynamic completed/2 .
:- dynamic tracing/1.

:- setval( number_of_answers, 0 ).
:- setval( unique_index,      0 ).

initialise :-
        retractall( coinductive( _ )            ),
        retractall( tabled( _ )                 ),
        retractall( answer( _, _, _ )           ),
        retractall( pioneer( _, _, _ )          ),
        retractall( result( _, _ )              ),
        retractall( loop( _, _ )                ),
        retractall( looping_alternative( _, _ ) ),
        retractall( completed( _, _ )           ),
        retractall( tracing( _ )                ),
        setval( number_of_answers, 0 ),
        setval( unique_index,      0 ).



%%%%%  Built-in predicates  %%%%
%%
%%  NOTE: Just adding "!" won't do the trick, the main interpreter would
%%        have to be modified substantially.
%%        Certain other built-ins may also require special treatment.

builtin( (_ , _)            ).  % special treatment in solve/3
builtin( (_ -> _ ; _)       ).  % special treatment in solve/3
builtin( (_ ; _)            ).  % special treatment in solve/3
builtin( \+( _ )            ).  % special treatment in solve/3
builtin( _ < _              ).
builtin( _ = _              ).
builtin( _ =< _             ).
builtin( _ > _              ).
builtin( _ >= _             ).
builtin( _ \= _             ).
builtin( _ is _             ).
builtin( assert( _ )        ).  % special treatment in solve/3 (only facts!)
builtin( atom( _ )          ).
builtin( call( _ )          ).
builtin( fail               ).
builtin( false              ).
builtin( member( _, _ )     ).
builtin( nl                 ).
builtin( once( _ )          ).  % special treatment in solve/3
builtin( read( _ )          ).
builtin( retractall( _ )    ).  % special treatment in solve/3 (only facts!)
builtin( set_flag( _, _ )   ).
builtin( true               ).
builtin( var( _ )           ).
builtin( write( _ )         ).
builtin( write_term( _, _ ) ).
builtin( writeln( _ )       ).
builtin( set_print_depth( _, _ )   ).      % not a real built-in, see  top_level



%%%%  Hooks

%% Declarations of hook predicates (for the top level):

hook_predicate( essence_hook( _, _ ) ).


%% The default essence_hook:

:- dynamic essence_hook/2.

essence_hook( T, T ).    % default, may be overridden by the interpreted program




%%%%%  Administration  %%%%%

:- op( 1000, fy, coinductive ).    % allow  ":- coinductive p/k ."
:- op( 1000, fy, tabled      ).    % allow  ":- tabled p/k ."
:- op( 1000, fy, trace       ).    % allow  ":- trace  p/k ."



%% The legal directives (check external form only).  (Used by the top level.)

legal_directive( (coinductive _) ).
legal_directive( (tabled _)      ).
legal_directive( (trace _)       ).
legal_directive( (dynamic _)     ).
legal_directive( (multifile _)   ).


%% Check and process the legal directives

execute_directive( tabled PredSpecs ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        (
            member( Pattern, Patterns ),
            assert( tabled( Pattern ) ),
            fail
        ;
            true
        ).

execute_directive( coinductive PredSpecs ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        (
            member( Pattern, Patterns ),
            assert( coinductive( Pattern ) ),
            fail
        ;
            true
        ).

execute_directive( (trace all) ) :-
        !,
        will_trace( [ _ ] ).

execute_directive( (trace PredSpecs) ) :-
        predspecs_to_patterns( PredSpecs, Patterns ),
        will_trace( Patterns ).

execute_directive( (dynamic _)   ).    % ignore
execute_directive( (multifile _) ).    % ignore


%% will_trace( + list of patterns ):
%% Store the patterns in tracing/1:

will_trace( Patterns ) :-
        member( Pattern, Patterns ),
        assert( tracing( Pattern ) ),
        fail.

will_trace( _ ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%  The interpreter  %%%%%


%% Execute a query.

:- mode query( + ).

query( Goals ) :-
        retractall( pioneer( _, _, _ )          ),
        retractall( result( _, _ )              ),
        retractall( loop( _, _ )                ),
        retractall( looping_alternative( _, _ ) ),

        setval( unique_index, 0 ),

        solve( Goals, [], [], 0 ).




%% solve( + sequence of goals,
%%        + stack,
%%        + coinductive hypotheses,
%%        + level
%%      ):
%% Solve the sequence of goals, maintaining information about the current chain
%% of tabled ancestors (stack) and the chain of coinductive ancestors
%% (coinductive hypotheses).  The level is the level of recursion, and is used
%% only for tracing.
%%
%% Each link in the chain of tabled ancestors is of the form
%%    triple( goal, index, clause )
%% where
%%    goal    is the (current instantiation of the) goal;
%%    index   is the unique index of the goal (every goal that is stacked starts
%%               out as a pioneer!)
%%    clause  is the clause that is currently used by the goal (it has been
%%               instantiated by matching with the goal in its original form,
%%               but does not share variables with the goal).

:- mode solve( +, +, + ).


% A negation.

solve( \+ Goal, Stack, Hyp, Level ) :-
        !,
        NLevel is Level + 1,
        trace_entry( normal, \+ Goal, Level ),
        (
            \+ solve( Goal, Stack, Hyp, NLevel ),
            trace_success( normal, \+ Goal, Level )
        ;
            trace_failure( normal, \+ Goal, Level ),
            fail
        ).


% One solution.

solve( once( Goal ), Stack, Hyp, Level ) :-
        !,
        NLevel is Level + 1,
        trace_entry( normal, once( Goal ), Level ),
        (
            once( solve( Goal, Stack, Hyp, NLevel ) ),
            trace_success( normal, once( Goal ), Level )
        ;
            trace_failure( normal, once( Goal ), Level ),
            fail
        ).


% A conditional.

solve( (Cond -> Then ; _Else), Stack, Hyp, Level ) :-
        solve( Cond, Stack, Hyp, Level ),
        !,
        solve( Then, Stack, Hyp, Level ).

solve( (_Cond -> _Then ; Else), Stack, Hyp, Level ) :-
        !,
        solve( Else, Stack, Hyp, Level ).


% A disjunction without a conditional.

solve( (Goals ; _), Stack, Hyp, Level ) :-
        solve( Goals, Stack, Hyp, Level ).

solve( (_ ; Goals), Stack, Hyp, Level ) :-
        !,
        solve( Goals, Stack, Hyp, Level ).


% A conjunction.

solve( (Goals1 , Goals2), Stack, Hyp, Level ) :-
        !,
        solve( Goals1, Stack, Hyp, Level ),
        solve( Goals2, Stack, Hyp, Level ).


% call/1

solve( call( Goal ), Stack, Hyp, Level ) :-
        (
            var( Goal )
        ->
            error( [ 'A variable meta-call:', call( Goal ) ] )
        ;
            solve( Goal, Stack, Hyp, Level )
        ).


% assert/1
% NOTE: This is good only for asserting facts!


solve( assert( Fact ), _, _ ) :-
        !,
        (
            \+ is_good_clause_head( Fact )
        ->
            error( [ "Bad fact: ", assert( Fact ) ] )
        ;
            true
        ),
        assert( interpreted : Fact ).


% retractall/1

solve( retractall( FactPattern ), _, _ ) :-
        !,
        (
            \+ is_good_clause_head( FactPattern )
        ->
            error( [ "Bad fact: ", retractall( FactPattern ) ] )
        ;
            true
        ),
        retractall( interpreted : FactPattern ).


% Some other supported built-in.

solve( BuiltIn, _, _, _ ) :-
        builtin( BuiltIn ),
        !,
        call( BuiltIn ).


% A "normal" goal (i.e., not tabled, not coinductive).

solve( Goal, Stack, Hyp, Level ) :-
        \+ tabled( Goal ),
        \+ coinductive( Goal ),
        !,
        trace_entry( normal, Goal, Level ),
        (
            NLevel is Level + 1,
            use_clause( Goal, Body ),
            solve( Body, Stack, Hyp, NLevel ),
            trace_success( normal, Goal, Level )
        ;
            trace_failure( normal, Goal, Level ),
            fail
        ).


% A goal that is coinductive, but not tabled.
% Apply the coinductive hypotheses first, then the clauses.

solve( Goal, Stack, Hyp, Level ) :-
        \+ tabled( Goal ),
        coinductive( Goal ),
        !,
        trace_entry( coinductive, Goal, Level ),
        (
            member( Goal, Hyp ),
            trace_success( 'coinductive (hypothesis)', Goal, Level )
        ;
            NLevel is Level + 1,
            use_clause( Goal, Body ),
            solve( Body, Stack, [ Goal | Hyp ], NLevel ),
            trace_success( 'coinductive (clause)', Goal, Level )
        ;
            trace_failure( coinductive, Goal, Level ),
            fail
        ).



% A tabled goal that has been completed: all the results are in "answer".

solve( Goal, _, _, Level ) :-
        is_completed( Goal ),
        !,
        trace_entry( completed, Goal, Level ),
        (
            get_answer( Goal ),
            trace_success( completed, Goal, Level )
        ;
            trace_failure( completed, Goal, Level ),
            fail
        ).


% A tabled goal that has a variant among its ancestors.
% If the goal is not coinductive, only the existing (most likely incomplete)
% results from "answer" are  returned before failure.
% If the goal is also coinductive, return the results that arise from
% coinductive hypotheses, then the remaining results from "answer".

% NOTE: 1. There can be only one variant ancestor, so the question of which one
%          to use does not arise.
%
%       2. Ancestor pioneer goals between this goal and its variant ancestor
%          will lose their status as pioneers (and the associated entries in
%          "loop" and "looping_alternative" will be removed).
%
%       3. If the variant ancestor is a pioneer, then:
%             - the entire prefix of the list of goals upto (but not including)
%               the variant ancestor will be added to the cluster of that
%               ancestor (by storing it in "loop");
%             - a copy of the current clause invoked by the ancestor (which can
%               be found together with the ancestor on the stack) is added to
%               "looping_alternative" entries for that ancestor.
%
%       4. If this goal is coinductive, then we use "result" to avoid
%           duplicating results.


solve( Goal, Stack, Hyp, Level ) :-
        is_variant_of_ancestor( Goal, Stack, AncestorTriple, InterveningGoals ),
        !,
        trace_entry( variant, Goal, Level ),
        (
            % Rescind the status of intervening pioneers:
            member( M, InterveningGoals ),
            is_a_variant_of_a_pioneer( M, Index ),
            optional_trace( 'Removing pioneer: ', M, Index, Level ),
            rescind_pioneer_status( Index ),
            fail
        ;
            true
        ),

        (
            % Create looping alternative if the variant ancestor is a pioneer:
            AncestorTriple = triple( G, Index, Clause ),
            is_a_variant_of_a_pioneer( G, Index )
        ->
            add_loop( Index, InterveningGoals ),
            add_looping_alternative( Index, Clause )
        ;
            true
        ),

        (
            coinductive( Goal )
        ->
            copy_term( Goal, OriginalGoal ),
            get_unique_index( Index ),
            (
                % results from coinductive hypotheses:
                member( Goal, Hyp ),
                trace_success( 'variant (coinductive)', Goal, Level ),
                new_result_or_fail( Index, Goal ),
                memo( OriginalGoal, Goal )
            ;
                % other tabled results
                get_answer( Goal ),
                new_result_or_fail( Index, Goal ),
                trace_success( variant, Goal, Level )
            ;
                % wrap it up
                trace_failure( variant, Goal, Level ),
                retractall( result( Index ) ),
                fail
            )
        ;

            % Not coinductive, just sequence through tabled answers:
            (
                get_answer( Goal ),
                trace_success( variant, Goal, Level )
            ;
                trace_failure( variant, Goal, Level ),
                fail
            )
        ).


% A pioneer goal is solved by program clauses, producing results that are stored
% in "answer".
% The goal succeeds as each answer is produced, and tries to come up with more
% after backtracking.
% When the usual clauses are exhausted, clauses stored in the associated entries
% of "looping_alternative" will be used to produce more answers, until a fixed
% point is reached.  The pioneer (and all the goals in its cluster) will then be
% marked as complete, and will cease to be a pioneer.
%
% (Note that a pioneer but also lose its status when some descendant goal finds
%  a variant ancestor that is also an ancestor of the pioneer.  See the case
%  of "variant of ancestor" above.)

solve( Goal, Stack, Hyp, Level ) :-
        (
            coinductive( Goal )
        ->
            NHyp = [ Goal | Hyp ]  % add this goal to the coinductive hypotheses
        ;
            NHyp = Hyp
        ),
        copy_term( Goal, OriginalGoal ),
        add_pioneer( Goal, Index ),
        optional_trace( 'Entering pioneer: ', Goal, Index, Level ),
        (
            NLevel is Level + 1,
            use_clause( Goal, Body ),
            copy_term( (Goal :- Body), ClauseCopy ),
            solve( Body,
                   [ triple( OriginalGoal, Index, ClauseCopy ) | Stack ],
                   NHyp,
                   NLevel
                 ),
            new_result_or_fail( Index, Goal ),
            memo( OriginalGoal, Goal, Level ),
            trace_success( pioneer, Goal, Level )
        ;

            % All the clauses have been exhausted, for looping alternatives (if
            % any).  However, the goal may have become completed (by a later
            % variant), or it might have lost its pioneer status (because it
            % belongs to a larger loop).

            is_completed( Goal )                      % a variant has completed?
        ->
            optional_trace( 'Removing completed pioneer: ',
                            Goal, Index, Level
                          ),
            rescind_pioneer_status( Index ),
            get_answer( Goal ),
            new_result_or_fail( Index, Goal ),
            trace_success( 'completed now', Goal, Level )
        ;

            is_a_variant_of_a_pioneer( Goal, Index )  % not lost pioneer status?
        ->
            (
                optional_trace( 'Computing fixed point for ',
                                Goal, Index, Level
                              ),
                compute_fixed_point( Goal, Index, Stack, Hyp, Level ),
                trace_success( pioneer, Goal, Level )
            ;
                optional_trace( 'Fixed point computed: ', Goal, Index, Level ),
                complete_goal( Goal, Level ),
                complete_cluster( Index, Level ),
                optional_trace( 'Removing pioneer: ', Goal, Index, Level ),
                rescind_pioneer_status( Index ),
                retractall( result( Index, _ ) ),
                fail
            )
        ;

            trace_failure( 'no longer a pioneer', Goal, Level ),
            retractall( result( Index, _ ) ),
            fail
        ).



%% use_clause( + goal, - body ):
%% Warn and fail if the goal invokes a non-existing predicate.  Otherwise
%% nondeterministically return the appropriately instantiated body of each
%% clause whose head matches the goal.

use_clause( Goal, Body ) :-
        (
            current_predicate( _, interpreted : Goal )
        ->
            clause( interpreted : Goal, Body )
        ;
            warning( [ 'Calling an undefined predicate: \"', Goal, '\"' ] ),
            fail
        ).



%% compute_fixed_point( + pioneer goal, + its index, + stack, + level ):
%% Solve the goal by associated rules from "looping_alternative", succeeding
%% with each new answer (and tabling it).  Fail when all the possible results
%% are exhausted.

:- mode compute_fixed_point( +, +, +, + ).

compute_fixed_point( Goal, Index, Stack, Hyp, Level ) :-
        getval( number_of_answers, NAns ),
        compute_fixed_point_( Goal, Index, Stack, Hyp, Level, NAns ).

%
:- mode compute_fixed_point( +, +, +, +, + ).

compute_fixed_point_( Goal, Index, Stack, Hyp, Level, _ ) :-
        NLevel is Level + 1,
        (
            coinductive( Goal )
        ->
            NHyp = [ Goal | Hyp ]  % add this goal to the coinductive hypotheses
        ;
            NHyp = Hyp
        ),
        copy_term( Goal, OriginalGoal ),

        looping_alternative( Index, (Goal :- Body) ),      % i.e., iterate
        Triple = triple( OriginalGoal, Index, (Goal :- Body) ),
        solve( Body, [ Triple | Stack ], NHyp, NLevel ),
        new_result_or_fail( Index, Goal ),
        memo( OriginalGoal, Goal, Level ).

compute_fixed_point_( Goal, Index, Stack, Hyp, Level, NAns ) :-
        getval( number_of_answers, NAnsNow ),
        NAnsNow \= NAns,                % i.e., fail if there are no new answers
        compute_fixed_point_( Goal, Index, Stack, Hyp, Level, NAnsNow ).



%% is_variant_of_ancestor( + goal,
%%                         + list of triples of goals, indices and clauses,
%%                         - the triple with the variant ancestor,
%%                         - list of goals between this and variant ancestor,
%%                       )
%% Succeeds if the goal is a variant of the goal in some member of the list.
%% If successful, returns the first such member and the list of intervening
%% goals.


:- mode is_variant_of_ancestor( +, +, - ).

is_variant_of_ancestor( Goal, Stack, AncestorTriple, InterveningGoals ) :-
        append( Prefix, [ AncestorTriple | _ ], Stack ),        % split the list
        AncestorTriple = triple( G, _, _ ),
        are_essences_variants( Goal, G ),
        !,
        extract_goals( Prefix, InterveningGoals ).


%% extract_goals( + list of triples of goals, indices and clauses,
%%                - list of goals
%%              ):
%% Filter away the other info in each triple, return list of goals only.

:- mode extract_goals( +, - ).

extract_goals( [], [] ).

extract_goals( [ triple( G, _, _ ) | Ts ], [ G | Gs ] ) :-
        extract_goals( Ts, Gs ).


%% rescind_pioneer_status( + index ):
%% Remove auxiliary table entries for the pioneer with this index.
%% Specifically, clean up "pioneer", "loop" and "looping_alternative".

:- mode rescind_pioneer_status( + ).

rescind_pioneer_status( Index ) :-
        retract(    pioneer( _, _, Index )          ),
        retractall( loop( Index, _ )                ),
        retractall( looping_alternative( Index, _ ) ).


%% complete_cluster( + index of a pioneer goal, + level for tracing ):
%% If the goal has an associated cluster, make sure all the goals in the cluster
%% are marked as completed.
%% Recall that a cluster may consist of a number of "loops".

:- mode complete_cluster( +, + ).

complete_cluster( Index, Level ) :-
        loop( Index, Gs ),                     % iterate over loops
        member( G, Gs ),                       % iterate over members of a loop
        complete_goal( G, Level ),
        fail.

complete_cluster( _ ).





%%-----  The tables: access and modification  -----


%% is_answer_known( + goal, + fact ):
%% Does the table "answer" contain a variant of this fact paired with a variant
%% of this goal?

:- mode is_answer_known( +, + ).

is_answer_known( Goal, Fact ) :-
        copy_term( Goal, Copy ),
        answer( Copy, G, F ),
        are_essences_variants( G, Goal ),
        are_essences_variants( F, Fact ),
        !.


%% memo( + goal, + fact, + level for tracing ):
%% If the table "answer" does not contain a variant of this fact paired with
%% a variant of this goal, then add the pair to the table, increasing
%% "number_of_answers".

:- mode memo( +, +, + ).

memo( Goal, Fact, _ ) :-
        is_answer_known( Goal, Fact ),
        !.

memo( Goal, Fact, Level ) :-
        % \+ is_answer_known( Goal, Fact ),
        optional_trace( 'Storing answer: ', Goal, Fact, Level ),
        copy_term( Goal, Copy ),
        assert( answer( Copy, Goal, Fact ) ),
        incval( number_of_answers ).


%% get_answer( +- goal ):
%% Get an instantiation (if any) tabled in "answer" for variants of this goal.
%% Sequence through all such instantiations on backtracking.

:- mode get_answer( ? ).

get_answer( Goal ) :-
        once( essence_hook( Goal, EssenceOfGoal ) ),
        copy_term( Goal, Copy ),
        answer( Copy, G, Ans ),
        once( essence_hook( G, EssenceOfG ) ),
        are_variants( EssenceOfGoal, EssenceOfG ),
        EssenceOfGoal = EssenceOfG,     % make sure variables are the right ones
        once( essence_hook( Ans, EssenceOfAns ) ),
        EssenceOfGoal = EssenceOfAns .  % instantiate



%% is_completed( + goal ):
%% Succeeds iff the goal is a variant of a goal that has been stored in
%% the table "completed".

:- mode is_completed( + ).

is_completed( Goal ) :-
        copy_term( Goal, Copy ),
        completed( Copy, G ),
        are_essences_variants( Goal, G ).


%% complete_goal( + goal, + index for tracing ):
%% Make sure the goal is marked as completed.

:- mode complete_goal( +, + ).

complete_goal( Goal, _ ) :-
        is_completed( Goal ),
        !.

complete_goal( Goal, Level ) :-
        % \+ is_completed( Goal ),
        copy_term( Goal, Copy ),
        optional_trace( 'Completing: ', Goal, '', Level ),
        assert( completed( Copy, Goal ) ).



%% is_a_variant_of_a_pioneer( + goal, -index ):
%% Succeeds if the goal is a variant of a goal that is tabled in "pioneer";
%% returns the index of the relevant entry in table "pioneer".

:- mode is_a_variant_of_a_pioneer( +, - ).

is_a_variant_of_a_pioneer( Goal, Index ) :-
        copy_term( Goal, Copy ),
        pioneer( Copy, G, Index ),
        are_essences_variants( Goal, G ),
        !.


%% add_pioneer( + goal, - index ):
%% Add an entry for this goal to "pioneer", return the unique index.

:- mode add_pioneer( +, - ).

add_pioneer( Goal, Index ) :-
        copy_term( Goal, Copy ),
        get_unique_index( Index ),
        assert( pioneer( Copy, Goal, Index ) ).



%% get_unique_index( - ):
%% Produce a new unique index.

:- mode get_unique_index( - ).

get_unique_index( Index ) :-
        getval( unique_index, Index ),
        incval( unique_index ).



%% is_result_known( + index, + fact ):
%% Does the table "result" contain a variant of this fact associated with this
%% index?

:- mode is_result_known( +, + ).

is_result_known( Index, Fact ) :-
        result( Index, F ),
        are_essences_variants( F, Fact ),
        !.


%% new_result_or_fail( + index, + fact ):
%% If the table "result" already contains a variant of this fact associated with
%% this index, then fail.  Otherwise record the fact in the table and succeed.

:- mode new_result_or_fail( +, + ).

new_result_or_fail( Index, Fact ) :-
        \+ is_result_known( Index, Fact ),
        assert( result( Index, Fact ) ).



%% add_loop( + index, + list of goals ):
%% Add an entry to "loop".

:- mode add_loop( +, + ).

add_loop( _, [] ) :-                                % empty loops are not stored
        !.

add_loop( Index, Goals ) :-                         % neither are duplicates
        loop( Index, Gs ),
        are_variants( Goals, Gs ),
        !.

add_loop( Index, Goals ) :-
        assert( loop( Index, Goals ) ).



%% add_looping_alternative( + index, + Clause ):
%% Add and entry to "looping_alternative".

:- mode add_looping_alternative( +, + ).

add_looping_alternative( Index, Clause ) :-          % duplicates are not stored
        looping_alternative( Index, C ),
        are_variants( Clause, C ),
        !.

add_looping_alternative( Index, Clause ) :-
        assert( looping_alternative( Index, Clause ) ).





%%-----  Custom-tailored utilities  -----


%% are_essences_variants( + term, + term ):
%% Are both the terms variants of each other after filtering through
%% essence_hook?

:- mode are_essences_variants( +, + ).

are_essences_variants( T1, T2 ) :-
        once( essence_hook( T1, ET1 ) ),
        once( essence_hook( T2, ET2 ) ),
        are_variants( ET1, ET2 ).



%% trace_entry( + label, + goal, + level ):
%% If the goal matches one of the traced patterns, print out a trace line about
%% entering the goal (at this level, with this label).

trace_entry( Label, Goal, Level ) :-
        tracing( Goal ),
        !,
        write_level( Level ),
        write( user_output, 'Entering ' ),
        write_label_and_goal( Label, Goal ),
        nl( user_output ).

trace_entry( _, _, _ ).


%% trace_success( + label, + goal, + level ):
%% If the goal matches one of the traced patterns, print out a trace line about
%% success of the goal (at this level, with this label).  Moreover, just before
%% backtracking gets back to the goal, print out a trace line about retrying the
%% goal.

trace_success( Label, Goal, Level ) :-
        tracing( Goal ),
        !,
        (
            write_level( Level ),
            write( user_output, 'Success ' ),
            write_label_and_goal( Label, Goal ),
            nl( user_output )
        ;
            write_level( Level ),
            write( user_output, 'Retrying ' ),
            write_label_and_goal( Label, Goal ),
            nl( user_output ),
            fail
        ).

trace_success( _, _, _ ).


%% trace_failure( + label, + goal, + level ):
%% If the goal matches one of the traced patterns, print out a trace line about
%% failure of the goal (at this level, with this label).

trace_failure( Label, Goal, Level ) :-
        tracing( Goal ),
        !,
        write_level( Level ),
        write( user_output, 'Failing ' ),
        write_label_and_goal( Label, Goal ),
        nl( user_output ).

trace_failure( _, _, _ ).


%% Auxiliaries for tracing:

write_level( Level ) :-
        write( user_output, '[' ),
        write( user_output, Level ),
        write( user_output, '] ' ).

write_label_and_goal( Label, Goal ) :-
        write( user_output, Label ),
        write( user_output, ': ' ),
        write( user_output, Goal ).



%% optional_trace( + label, + goal, + term, + level ):
%% If the goal matches one of the traced patterns, print out a trace line with
%% this label, the goal and the term.

optional_trace( Label, Goal, Term, Level ) :-
        tracing( Goal ),
        !,
        write_level( Level ),
        write( user_output, Label ),
        write( user_output, Goal ),
        write( user_output, ' : ' ),
        write( user_output, Term ),
        nl(    user_output ).

optional_trace( _, _, _, _ ).



%% fatal_error( + message, + stack ):
%% Display the message and stack, then abort.

:- mode fatal_error( +, + ).

fatal_error( Message, Stack ) :-
        begin_error,
        writeln(    error, Message ),
        writeln(    error, '' ),
        writeln(    error, '*** The current stack:' ),
        show_stack( error, Stack ),
        end_error.

%
show_stack( Stream, Stack ) :-
        member( Call, Stack ),
        writeln( Stream, Call ),
        fail.

show_stack( _ ).
