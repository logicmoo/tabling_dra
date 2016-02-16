
:- '$set_source_module'(_,user).
:- 'module'(user).

:- if((fail,exists_source(library(drac)))).
	:- use_module(library(drac)).
:- else.
	:- use_module(library(dra)).
:- endif.

/*
% Simpler example than example12.pl, but the number of predicates involved in mutual recursion will also increase at runtime.

expected_variants([p(3,_),p(2,_),q(2,_),q(3,_),p(_,_)]).
% Note: p(3,_) and q(3,_) are empty tables, but they are there.
expected_answers_for_variant(p(_,_),[p(1,2),p(2,3),p(1,3)]).
expected_answers_for_variant(p(3,_),[]).
expected_answers_for_variant(p(2,_),[p(2,3)]).
expected_answers_for_variant(q(2,_),[q(2,3)]).
expected_answers_for_variant(q(3,_),[]).
*/

:-table((p/2, q/2)).
:-export((p/2, q/2)).


p(X,Y) :-p(X,Z), q(Z,Y).
p(X,Y) :-e(X,Y).
q(X,Y) :-p(X,Y).

e(1,2).
e(2,3).

:- listing(p/2).

:- once(\+ tnot(p(_X,_Y))).

:-print_tables.





/*



% :-pf(('dra/tabling3/examples/XSB/fib.tlp') ).

:-pf(('dra/tabling3/examples/co_t.tlp') ).


:-pf(('dra/tabling3/examples/coind2.tlp') ).
% :-pf(('dra/tabling3/examples/LTL/v.pl') ).
%:-pf(('dra/tabling3/examples/mini_graph.tlp') ).
%:-pf(('dra/tabling3/examples/mini_language.tlp') ).
:-pf(('dra/tabling3/examples/paper_example.tlp') ).



:-pf(('dra/tabling3/Bench/tabling3/run')).
:-pf(('dra/tabling3/Bench/prolog/run')).
:-pf(('dra/tabling3/Bench/clpfd/run')).
:-pf(('dra/tabling3/Bench/aspclp/run')).

t0:-time([('dra/tabling3/examples/XSB/farmer.tlp')]).
tn:-time([('dra/tabling3/examples/tnot1.tlp')]).
t1:-time(process_file(('dra/tabling3/examples/XSB/farmer.tlp') )),!.
t2:-time([('dra/tabling3/examples/XSB/ham.tlp')]).
t2a:-time([('dra/tabling3/examples/XSB/ham_auto.tlp')]).

t2b:-time(pf(('dra/tabling3/examples/XSB/ham.tlp') )).
t3:-[(('dra/tabling3/examples/graph.tlp') )].
t4:-pf(('dra/tabling3/examples/module.tlp') ).
t4:-[(('dra/tabling3/examples/paper_example.tlp') )].
t4:-pf(('dra/tabling3/examples/conditional.clp') ).
t4:-pf(('dra/tabling3/examples/simple1.tlp') ).
t4:-pf(('dra/tabling3/examples/simple1_old_first.tlp') ).
t4:-pf(('dra/tabling3/examples/conditional.clp') ).
t4:-pf(('dra/tabling3/examples/small_comment_example.tlp') ).
t4:-pf(('dra/tabling3/examples/coind_new.tlp') ).
t5:-consult('/devel/LogicmooDeveloperFramework/PrologMUD/packs/MUD_PDDL/prolog/dra/tabling3/Bench/tabling/tcl.pl').

*/



