/*
 * Driver file for subsumption tests.
 */

:- import numbervars/3 from num_vars.


backtrack_and_print(Query) :-
	call(Query), numbervars(Query,0,_), writeq(Query), nl, fail.
backtrack_and_print(_).



lrtc(QueryCode,DataFile) :-
	['Programs/traversal',DataFile], lrtc(QueryCode).
lrtc('XY') :-
	backtrack_and_print( anc_l(_,_) ).
lrtc('1Y') :-
	backtrack_and_print( anc_l(1,_) ).
lrtc('X8') :-
	backtrack_and_print( anc_l(_,8) ).
lrtc(multidown) :-
	backtrack_and_print( anc_l(_,_) ),
	backtrack_and_print( anc_l(X,X) ),
	backtrack_and_print( anc_l(f(F),f(F)) ),
	backtrack_and_print( anc_l([L],[L]) ).
lrtc(multiup) :-
	backtrack_and_print( anc_l([L],[L]) ),
	backtrack_and_print( anc_l(f(F),f(F)) ),
	backtrack_and_print( anc_l(X,X) ),
	backtrack_and_print( anc_l(_,_) ).


rrtc(QueryCode,DataFile) :-
	['Programs/traversal',DataFile], rrtc(QueryCode).
rrtc('XY') :-
	backtrack_and_print( anc_r(_,_) ).
rrtc('1Y') :-
	backtrack_and_print( anc_r(1,_) ).
rrtc('X8') :-
	backtrack_and_print( anc_r(_,8) ).
rrtc(multidown) :-
	backtrack_and_print( anc_r(_,_) ),
	backtrack_and_print( anc_r(X,X) ),
	backtrack_and_print( anc_r(f(F),f(F)) ),
	backtrack_and_print( anc_r([L],[L]) ).
rrtc(multiup) :-
	backtrack_and_print( anc_r([L],[L]) ),
	backtrack_and_print( anc_r(f(F),f(F)) ),
	backtrack_and_print( anc_r(X,X) ),
	backtrack_and_print( anc_r(_,_) ).


drtc(QueryCode,DataFile) :-
	['Programs/traversal',DataFile], drtc(QueryCode).
drtc('XY') :-
	backtrack_and_print( anc_d(_,_) ).
drtc('1Y') :-
	backtrack_and_print( anc_d(1,_) ).
drtc('X8') :-
	backtrack_and_print( anc_d(_,8) ).
drtc(multidown) :-
	backtrack_and_print( anc_d(_,_) ),
	backtrack_and_print( anc_d(X,X) ),
	backtrack_and_print( anc_d(f(F),f(F)) ),
	backtrack_and_print( anc_d([L],[L]) ).
drtc(multiup) :-
	backtrack_and_print( anc_d([L],[L]) ),
	backtrack_and_print( anc_d(f(F),f(F)) ),
	backtrack_and_print( anc_d(X,X) ),
	backtrack_and_print( anc_d(_,_) ).


sg(QueryCode,DataFile) :-
	['Programs/traversal',DataFile], sg(QueryCode).
sg('XY') :-
	backtrack_and_print( same_gen(_,_) ).
sg('1Y') :-
	backtrack_and_print( same_gen(1,_) ).
sg('X8') :-
	backtrack_and_print( same_gen(_,8) ).
sg(multidown) :-
	backtrack_and_print( same_gen(_,_) ),
	backtrack_and_print( same_gen(X,X) ),
	backtrack_and_print( same_gen(f(F),f(F)) ),
	backtrack_and_print( same_gen([L],[L]) ).
sg(multiup) :-
	backtrack_and_print( same_gen([L],[L]) ),
	backtrack_and_print( same_gen(f(F),f(F)) ),
	backtrack_and_print( same_gen(X,X) ),
	backtrack_and_print( same_gen(_,_) ).


genome(DataFile) :-
	['Programs/traversal',DataFile],
	backtrack_and_print( genome_l(_) ).


decker :-
	['Programs/decker_rules'], load_dync('Data/decker_facts'),
	backtrack_and_print(p_isa_(_,f_Researcher)).


flora :-
	['Programs/flora'],
	backtrack_and_print('_$_$_flora_isa_rhs'(_,direct)).
