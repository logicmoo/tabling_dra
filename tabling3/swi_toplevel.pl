#! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/
:- shell(cls).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory,H),absolute_file_name('../../../..',A,[file_type(directory),relative_to(H)]),asserta(user:file_search_path(pack,A)).

:- attach_packs.
:- initialization(attach_packs).

% :- multifile sandbox:safe_primitive/1.
% :-asserta((sandbox:safe_primitive(Z):-wdmsg(Z))).

%%% ON :- initialization( profiler(_,walltime) ).
%%% ON :- initialization(user:use_module(library(swi/pce_profile))).

:- user:ensure_loaded(library(ape/get_ape_results)).

:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).

:- use_module(library(coinduction),
  	  [ (coinductive)/1,
  	    op(1150, fx, (coinductive))
  	  ]).

process_file_test(F):-    abolish( interpreted:demo/0  ),abolish( demo/0  ),
   retractall( interpreted:top  ),retractall( top  ),
   retractall( interpreted:test  ),retractall( test  ),
   time(must(once(process_file(F)))),!.  % ,once(ignore((run_curent_test,sleep(2)))))).

run_curent_test:- show_call_failure(if_defined(go,if_defined(test,if_defined(top)))),!.
run_curent_test:- top(_),!,forall(top(I),time((ignore(show_call((nonvar(I),query(I))))))),!.


:- ensure_loaded( utilities ).
:- ensure_loaded( program_consistency ).
:- ensure_loaded( output_equation ).



%% load( + file name ):
%% Initialise, then load a program from this file, processing directives and
%% queries.  After this is done, enter interactive mode.


:-export(load/1).
load( FileName ) :-
     must_det_l((
        setup,
        initialise,                              % provided by a metainterpreter
        process_file( FileName ),!,
        check_general_consistency,
        program_loaded)),!.                          % provided by a metainterpreter


cputime(X):- statistics(cputime,X).
table(X):-execute_directive(table(X)).

%% process_file( + file name ):
%% Load a program from this file, processing directives and queries.

% :- mode process_file( + ).

do_process_file( FileName ) :-    
        open_the_file( FileName, ProgStream ),
        process_input( ProgStream ),!,
        
        sanity(at_end_of_stream(ProgStream)),        
   % atom_to_memory_file('',Null_stream),
   % file_directory_name(FileName,D),
   stream_property(ProgStream,file_name(FN)),
   load_files(FN,[derived_from(FileName),register(true),stream(ProgStream)]),
        close( ProgStream ),!.

%

current_dirs(D):- prolog_load_context(directory,D).
current_dirs(D):- working_directory(D,D).
current_dirs(D):- current_stream(_,read,Y), stream_property(Y,file_name(FN)), file_directory_name(FN,D).
current_dirs(D):- stream_property(_,file_name(FN)), file_directory_name(FN,D).
current_dirs(D):- source_file_property(FN, modified(_)), file_directory_name(FN,D).
current_dirs('./.').

to_the_file( FileName, FileName ) :- atomic(FileName),exists_file(FileName),!.
to_the_file( FileName, AFN ) :- 
 member(TF,[false,true]), 
  must(( default_extension( Ext ),no_repeats(current_dirs(D)),
        absolute_file_name(FileName,AFN,[solutions(all),expand(TF),access(read),relative_to(D),file_errors(fail),extensions([Ext,'.pl','.tlp',''])]),
        exists_file(AFN))),!.


%% top:
%% Interactive mode.  Each term that is not a directive or a query is treated
%% as an abbreviated query.  After displaying the results of each query read
%% characters upto the nearest newline: if the first character is ";",
%% backtrack to find alternative solutions.
%% Exit upon encountering end of file.
%% NOTE: When running on Sicstus, each term must come on a separate line: after
%%       reading the term the rest of the line is ignored, to facilitate
%%       interaction with the user when asking whether more answers are needed.




:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-fail.


:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).
user:expand_answer(_Goal, _Expanded):-fail.



		/********************************
		*           EXECUTION		*
		********************************/
:-meta_predicate user:dra_execute(0, ?).
:-meta_predicate user:user:residue_vars(0, ?).
:-meta_predicate user:user:dra_execute_goal22(0, ?).

user:dra_execute(Var, _) :-
	var(Var), !,
	print_message(informational, var_query(Var)),
	fail.
user:dra_execute(end_of_file, _) :- !,
	print_message(query, query(eof)).
user:dra_execute(Goal, Bindings) :-
	'$module'(TypeIn, TypeIn),
	'$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected), !,
	setup_call_cleanup('$set_source_module'(M0, TypeIn),
			   expand_goal(Corrected, Expanded),
			   '$set_source_module'(_, M0)),
	print_message(silent, toplevel_goal(Expanded, Bindings)),
	user:dra_execute_goal22(Expanded, Bindings).
user:dra_execute(_, _) :-
	notrace,
	print_message(query, query(no)),
	fail.

user:dra_execute_goal22(Goal, Bindings) :-
   	'$toplevel':restore_debug,
	user:residue_vars(Goal, Vars),
	deterministic(Det),
	(   '$toplevel':save_debug
	;   '$toplevel':restore_debug, fail
	),
	flush_output(user_output),
	'$toplevel':call_expand_answer(Bindings, NewBindings),
	(    \+ \+ '$toplevel':write_bindings(NewBindings, Vars, Det)
	->   !, fail
	).
user:dra_execute_goal22(_, _) :-
	'$toplevel':save_debug,
	print_message(query, query(no)),
	fail.

user:residue_vars(Goal, Vars) :-
	current_prolog_flag(toplevel_residue_vars, true), !,
	call_residue_vars(query(Goal), Vars).
user:residue_vars(Goal, []) :-
	query(Goal).


user:dra_prompt(Module, BrekLev, Prompt) :-
	current_prolog_flag(toplevel_prompt, PAtom),
	atom_codes(PAtom, P0),
	(    Module \== user
	->   '$toplevel':'$substitute'('~m', [Module, ': '], P0, P1)
	;    '$toplevel':'$substitute'('~m', [], P0, P1)
	),
	(    BrekLev > 0
	->   '$toplevel':'$substitute'('~l', ['[', BrekLev, '] '], P1, P2)
	;    '$toplevel':'$substitute'('~l', [], P1, P2)
	),
	current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
	(    Tracing == true
	->   '$toplevel':'$substitute'('~d', ['[traced] '], P2, P3)
	;    Debugging == true
	->   '$toplevel':'$substitute'('~d', ['[debug] '], P2, P3)
	;    '$toplevel':'$substitute'('~d', [], P2, P3)
	),
	atom_chars(Prompt, P3).

%:- call(user:rl_add_history(ls)).
%:- call(user:rl_add_history('traced,go')).
:- '$toplevel':setup_history.

user:listing_mpred_hook(What):- debugOnError(dra_listing(What)).

dra_listing(What):-get_pi(What,PI),PI\=@=What,!,dra_listing(PI).
dra_listing(Matches):- ignore(dra_listing_0(Matches)),!.

dra_listing_0(MatchesIn):- 
 forall(property_pred(DECLF,DBF),
  (ignore(( DB=..[DBF,Matches],
  clause(DB,true),
  get_functor(Matches,PI0),
  get_functor(MatchesIn,PI1),!,
  PI0==PI1,
  functor(Matches,F,A),
  Decl=..[DECLF,F/A],
  format('~N:- ~q.~n',[Decl]))))).



%legal_directive(M:P):-atom(M),M:legal_directive(P).

%legal_directive(P):-compound(P),functor(P,F,1),property_pred(F).

property_pred(table,is_tabled).
property_pred(builtin,is_builtin).
property_pred(old_first,is_old_first).
property_pred(coinductive0,is_coinductive0).
property_pred(coinductive1,is_coinductive1).
property_pred(topl,is_topl).
property_pred(support,is_support).
%property_pred(traces,is_traced).
%property_pred(hilog,is_hilog).


%:-use_module(boot('$toplevel'),[]).
% '$query_loop'/0 
(tprolog) :-   \+ lp_system( eclipse ),!,
    user:with_assertions(op(0,fy,(traced)),   
	(((   current_prolog_flag(break_level, BreakLev)
	->  true
	;   BreakLev = -1
	),
	repeat,
	    (   '$module'(TypeIn, TypeIn),
	       '$toplevel':(((   (stream_property(user_input, tty(true)),write('tprolog '))
		->  user:dra_prompt(TypeIn, BreakLev, Prompt),
		    prompt(Old, '|    ')
		;   Prompt = '', prompt(Old, '') ),
		trim_stacks,
	        '$toplevel':read_query(Prompt, Query, Bindings),
		prompt(_, Old),
		call_expand_query(Query, ExpandedQuery,
				  Bindings, ExpandedBindings)
	    ->  expand_goal(ExpandedQuery, Goal))),
	        (user:dra_execute(Goal, ExpandedBindings),fail) )))), !.



initialize_table:-must(initialise).
print_table_statistics:-print_statistics.
%load(P):-must(prog0(P)),!.

:- user:ensure_loaded(library(dra/tabling3/dra_table_assert)).
%:- user:ensure_loaded(library(dra/tabling3/dra_table_record)).
:- user:ensure_loaded(library(dra/tabling3/compatibility_utilities_swi)).
:- user:ensure_loaded(library(dra/tabling3/top_level)).
:- user:ensure_loaded(library(dra/tabling3/dra_common_wcuts)).  

% c + r = 7.949 seconds

/*
% :- process_file_test(library('dra/tabling3/examples/XSB/fib.tlp') ).

:- process_file_test(library('dra/tabling3/examples/co_t.tlp') ).


:- process_file_test(library('dra/tabling3/examples/coind2.tlp') ).
% :- process_file_test(library('dra/tabling3/examples/LTL/v.pl') ).
%:- process_file_test(library('dra/tabling3/examples/mini_graph.tlp') ).
%:- process_file_test(library('dra/tabling3/examples/mini_language.tlp') ).
:- process_file_test(library('dra/tabling3/examples/paper_example.tlp') ).



:- process_file_test(library('dra/tabling3/Bench/tabling3/run')).
:- process_file_test(library('dra/tabling3/Bench/prolog/run')).
:- process_file_test(library('dra/tabling3/Bench/clpfd/run')).
:- process_file_test(library('dra/tabling3/Bench/aspclp/run')).
*/
:- time(process_file_test(library('dra/tabling3/examples/XSB/farmer.tlp') )),!.
:- time(process_file_test(library('dra/tabling3/examples/XSB/ham.tlp') )).

% :- process_file_test('/devel/LogicmooDeveloperFramework/PrologMUD/packs/MUD_PDDL/prolog/dra/tabling3/Bench/tabling/tcl.pl').

:-prolog.

:- repeat,logOnErrorIgnore(tprolog),fail.

