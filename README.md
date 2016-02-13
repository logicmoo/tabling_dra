# tabling
Port to SWI-Prolog's C @ https://github.com/logicmoo/swipl-devel/   for the "dra" memoizing interpreter 


   General description
   -------------------

   A simple (and very inefficient) interpreter that emulates "top-down tabled
   programming", as described in

     [1] Hai-Feng Guo, Gopal Gupta:
         Tabled Logic Programming with Dynamic Ordering of Alternatives
         (17th ICLP, 2001)

   There are two significant changes with respect to the description in the
   paper:

       - A tabled goal will never produce the same answer twice.

         More specifically: two answers will never be variants of each other.
         Please note that "goal" means a goal instance.

       - By default, new answers for a tabled goal will be produced before
         old answers.  The user can reverse the order by means of an "old_first"
         directive.

         Here, "new answer for a tabled goal" means an answer that has not yet
         been seen (and tabled) for a variant of the goal.

         The default behaviour is intended to help computations converge more
         quickly.  The user is given an option to change it, because some
         predicates may produce a very large (even infinite) set of answers on
         backtracking, and the application might not require those answers.

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

       :- table p/2 .

   All calls to a tabled predicate that are present in the interpreted program
   are called "tabled calls".  Instances of such calls are called "tabled
   goals".  In general, we will use the term "call" to refer to a static entity
   in the program, and "goal" to refer to an instance of a call.  We will also
   avoid the conventional overloading of the term "goal" in yet another way: we
   will call a sequence (i.e., conjunction) of goals just that (unless we can
   refer to it as a "query" or a "resolvent").

   Similarly, the user can declare a predicate to be "coinductive", by using
   another kind of directive, e.g.,

       :- coinductive0  p/2 .
       :- coinductive1 q/3 .

   Calls and goals that refer to a coinductive predicate will also be called
   "coinductive".


   Limitations
   -----------

   The interpreted program must not contain cuts.  It also must not contain
   calls to built-in-predicates, except for the handful of predicates listed in
   is_never_tabled/1 below.  (This list can be easily extended as the need arises.  Some
   built-in predicates, however, cannot be added without modifying the
   interpreter, sometimes extensively: "!/0" is a good example.)



   Data structures
   ---------------

   The interpreter uses a number of tables that store information accumulated
   during a computation.  A computation consists in reading a program and
   executing a number of queries.  A query is a sequence (i.e., conjunction) of
   goals.

   The tables (implemented as dynamic predicates of Prolog) are:


   -- is_coinductive0( generic head )
   -- is_coinductive1( generic head )
   -- is_tabled( generic head )
   -- is_old_first( generic head )

           Each of these tables contains an entry for each predicate that has
           been declared as having the corresponding property (i.e., as
           coinductive, table etc.).  For instance, when the interpreter reads
               :- coinductive0 p/2 .
           it stores the fact
               is_coinductive0( p( _, _ ) ).

           A "coinductive0" declaration is deemed to supersede "coinductive1",
           and information about a predicate that has been so declared is stored
           both in coinductive0/1 and coinductive1/1.

           These tables are cleared only before reading in a new program.


   -- answer( goal, fact )

           Used to store results computed for tabled goals encountered during a
           computation.  Once present, these results are also used during
           further stages of the computation.

           Note that the fact is an instantiation of the goal.  If a tabled goal
           has no solutions, it will have no entry in "answer", even though it
           may have an entry in "completed" (see below).

           (NOTE:
               1. In the actual implementation each fact in "answer" has the
                  form
                     answer( cgoal, goal, fact )
                  where "cgoal" is a copy of "goal" (no shared variables),
                  passed through essence_hook/2.
                  This is done to facilitate more effective filtering (via
                  unification) before a check is made for whether "goal" is a
                  variant of the goal for which we are seeking a tabled answer.

               2. This stuff has been removed to file dra_table_assert.pl
                  or dra_table_record.pl (only one of them is used,
                  depending on the logic programming system: see the main file
                  used to load the program.
           ))

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

               program:  :- table p/2.
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
           for its variant descendants can be obtained simply by dra_calling
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
                  where "cgoal" is a copy of "goal" (no shared variables),
                  passed through essence_hook/2.
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
           alternatives".
            )

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
                  where "cgoal" is a copy of "goal" (no shared variables),
                  passed through essence_hook/2.
                  This is done to facilitate more effective filtering (via
                  unification) before a check is made for whether "goal" is a
                  variant of the goal for which we are checking whether it is
                  completed.
           )


   -- is_tracing( goal )

           A goal that matches something in this table will show up on the
           wallpaper traces.  This table is empty by default, and filled only
           by invocations of "traces" (most often in "traces" directives
           encountered when the interpreted program is being read).

   -- step_counter

           This is a non-logical variable that keeps track of the number of
           goals resolved during the evaluation of each query.  The final value
           is printed after the query terminates.

           The variable is cleared before the evaluation of a new query.

   -- old_table_size

           This is a non-logical variable that is used to store the value of
           "number_of_answers" before the evaluation of a query.  Used to
           produce automatic information about the growth of the table after the
           query terminates.

           The variable is reinitialized before the evaluation of a new query.


  NOTE:
 
    1. See ../general/top_level.ecl for a description of how to load
       and run programs.
       Please note that in Eclipse after loading this interpreter you
       should issue
            :- import dra.
       if you don't want to keep writing
            dra:prog( filename )
       every time.
 
    2. The interpreter supports a number of directives:
 
       a) Tabled and coinductive predicates should be declared as such in
          the program file, e.g.,
              :- table       ancestor/2.
              :- coinductive0  comember/2.
              :- coinductive1 comember/2.
 
          "coinductive1" means that if there are coinductive hypotheses
          with which a goal unifies, then the usual clauses will not be tried
          after the hypotheses are exhausted (this is "new style"
          coinduction).
 
       b) To include files use the usual Prolog syntax:
              :- [ file1, file2, ... ].
 
       c) To declare predicates used in an interpreted program as dynamic,
          use
              :- dynamic p/k.
 
       d) By default, a goal produces new (i.e., heretofore unknown) answers
          before producing old ones.  To reverse this behaviour, use
 
              :- old_first p/k.
          or
              :- old_first all.
 
       e) To produce a wallpaper traces use the traces directive. For example,
 
              :- traces p/3, q/0, r/1.
 
          will traces predicates "p/3", "q/0" and "r/1".  If you want to traces
          everything, use
 
              :- traces all.
 
          These directives are cumulative.
 
       f) To print out subsets of the current answer table, use
 
              :- answers( Goal, Pattern ).
 
          this will print all tabled answers that are associated with a
          variant of Goal and unifiable with Pattern.
          To get a dump of the entire table, use just
 
              :- answers( _, _ ).
 
    2. The program should contain no other directives. It may, however,
       contain queries, which will be executed immediately upon reading.
 
    3. Just before the result of a query is reported, the interpreter
       produces a printout with statistics accummulated since the previous
       printout (or since the beginning, if this is the first printout during
       this session with the interpreted program). The printout looks like
       this:
 
           [K steps, M new answers tabled (N in all)]
 
       where K, M and N are some natural numbers. K is the number of
       evaluated goals, M is the number of new additions to the answer table,
       N is the current size of the answer table.
 
    4. If the program invokes a built-in predicate, that predicate dra_must
       be declared in the table "is_never_tabled/1" (see file "dra_builtins.pl").
       Every addition should be considered carefully: some built-ins might
       require special treatment by the interpreter.
 
    5. The program may contain clauses that modify the definition of the
       interpreter's predicate "essence_hook/2" (the clauses will be asserted
       at the front of the predicate, and will thus override the default
       definition for some cases).  The default definition is
 
          essence_hook( T, T ).
 
       This predicate is invoked _in certain contexts_ when:
          - two terms are about to be compared (either for equality or to
            check whether they are variants of each other);
          - an answer is tabled;
          - an answer is retrieved from the table.
 
       The primary intended use is to suppress arguments that carry only
       administrative information and that may differ in two terms that are
       "semantically" equal or variants of each other. (Such, for example, is
       the argument that carries the set of coinductive hypotheses in a
       co-logic program translated into Prolog: see "../coind/translate_clp".
       Mind you, that translation need not be applied to programs executed by
       this interpreter).
 
       For example, the presence of
 
          essence_hook( p( A, B, _ ),  p( A, B ) ).
 
       will result in "p( a, b, c )" and "p( a, b, d )" being treated as
       identical, as each of them will be translated to "p( a, b )" before
       comparison.
 
       NOTE: This facility should be used with the utmost caution, as it
             may drastically affect the semantics of the interpreted program
             in a fashion that would be hard to understand for someone who
             does not understand the details of the interpreter.

 
     The top level notes "never_tabled" declarations in the table "is_never_tabled".
        For example,
 
            :- never_tabled p/1, q/2.
 
        will be stored as
 
            is_never_tabled( p( _ ) ).
            is_never_tabled( q( _, _ ) ).
 
        The intended meaning is that "never_tabled" predicates do not make use
        (directly or indirectly) of the special features provided by the
        metainterpreter, so their invocations can be handled just by handing
        them over to Prolog (which would presumably speed up the computation).
 
        Please note that the never_tabled predicates (which should be defined in
        files mentioned in ":- load_is_support( filename )." directives) are
        compiled into the module "never_tabled" (unless they are defined within
        other modules).
 
 
     The metainterpreter should provide the following predicates
        ("hooks") that will be called by the top level:
 
           - is_cuts_ok/1:
                  Defines patterns for built-in predicates from the host
                  system that can be invoked by the interpreted program.
                  For example, to allow writeln/2, declare:
                      is_cuts_ok( writeln( _, _ ) ).
 
           - default_extension/1:
                  This predicate is optional.  If present, its argument
                  should be an atom whose name is the extension string to be
                  added to file names that do not already have an extension.
                  (The string should begin with a period!)
                  For example, a metainterpreter for coinductive logic
                  programming might contain the following fact:
                       default_extension( '.clp' ).
 
           - initialise/0:
                  This will be called before loading a new program,
                  giving the metainterpreter an opportunity to
                  (re)initialise its data structures.
 
           - legal_directive/1:
                  Whenever the top level encounters a directive
                  (of the form ":- D."), it will call "legal_directive( D )".
                  If the call succeeds, the interpreter will be given
                  a chance to process the directive (see below), otherwise
                  the directive will be ignored (with a suitable warning).
 
           - process_directive/1:
                  Whenever the top level encounters a legal directive
                  ":- D" (see above), it invokes "process_directive( D )"
                  to give the interpreter a chance to act upon the
                  directive.
 
           - dra_call_interp/1:
                  This would be the main entry point of the metainterpreter.
                  Whenever the top level encounters a query (of the form
                  "?- Q."), it will display the query and then call
                  "dra_call_interp( Q )".  Depending on the result, it will then
                  display "No", or "Yes" (preceded by a display of bindings
                  acquired by the variables occurring in "Q"); in the latter
                  case it will also backtrack to obtain more solutions.
 
 
     The metainterpreter can also define hooks of its own.  A hook
        predicate should be declared in a fact of "hook_predicate/1".
        For example,
 
            hook_predicate( essence_hook( _, _ ) ).
 
        declares that "essence_hook/2" is a metainterpreter hook.  A hook
        predicate (essence_hook/2 in this case) should be dynamic.  When
        the top level encounters a clause whose head matches a hook predicate
        declaration, the clause is asserted at the front (!) of the predicate
        (in the module of the running program, not in "interpreted").
 
        NOTE: If the interpreter does not use hook predicates, it must contain
              the definition
                  hook_predicate( '' ).

