:- import 
	abolish_table_info/0,
	buff_alloc/3, buff_cell/3, buff_dealloc/4, buff_set_word/3, buff_word/3,
	call_hash/5, conpsc/2, cut_if_leader/3, slg_not/1,
	is_incomplete/5, get_osp_breg/2, get_ptcp/1, get_subgoal_ptr/2,
	print_predicate_table/1,
        psc_tabled/2,
	stat_flag/2, stat_set_flag/3, 
	term_type/2, tip_prop/4, get_emu_dependent_const/2,
	conpsc/2, buff_byte/3, buff_set_byte/3, 
	zero_out_profile/0,write_out_profile/0
	from machine.

:- import ground/1, copy_term/2, member/2 from basics.

:- import tcall/1 from standard.

:- import get_residual/2 from tables.
:- import 't not'/1 from tables.

cycle_win_bench:- 
	bigbench(win2k(1),[win,cycle(2048),its(100)],100),
	bigbench(unsafe_win(1),[unsafe_win,cycle(2048),its(100)],100).

chain_win_bench:- 
	bigbench(win2k(1),[win,chain(2048),its(100)],100),
	bigbench(unsafe_win(1),[unsafe_win,chain(2048),its(100)],100),
	bigbench(sldnf1_win(1),[sldnf1_win,chain(2048),its(100)],100),
	bigbench(sldnf2_win(1),[sldnf2_win,chain(2048),its(100)],100).

:- table win2k/1.
win2k(X):- move2k(X,Y),tnot(win2k(Y)).

:- table unsafe_win/1.
unsafe_win(X):- move2k(X,Y),'t not'(unsafe_win(Y)).

sldnf1_win(X):- move2k(X,Y),my_not(sldnf1_win(Y)).

sldnf2_win(X):- move2k(X,Y),\+ sldnf2_win(Y).

:- table dual_win/1,neg_win/1.
dual_win(X):- move2k(X,Y),neg_win(Y).

neg_win(X):- \+ move2k(X,Y).
neg_win(X):- move2k(X,Y),dual_win(Y).

my_not(Term):- (tcall(Term), !, fail) ; true.

:- table my_win/1.
my_win(X):- move2k(X,Y),tnot(my_win(Y)).

fake_win(X):- tnot(pwin(X)).

:- table pwin/1.
pwin(X):- move2k(X,Y),pwin(Y).

:- table p1win/1.
p1win(X):- move2k(X,Y),neg_call(p1win(Y)).

neg_call(X) :-
	get_ptcp(PTCP), get_subgoal_ptr(X, SubgPtr),
	( SubgPtr == 0 ->  tcall(X), fail
	; is_incomplete(X, SubgPtr, PTCP, 1, Subg), true
	).


my_tnot(X):-
	( ground(X) -> my_tnot1(X)
	; functor(X,F,A),
	  abort(('FLOUNDERING: tnot/1 used with a non-ground subgoal of predicate ', F/A))
	).

my_tnot1(X) :-
	get_ptcp(PTCP), get_subgoal_ptr(X, SubgPtr),
	( SubgPtr == 0 ->  tcall(X), fail
	; is_incomplete(X, SubgPtr, PTCP, 1, Subg), slg_not(Subg)
	).


:- table pnwin/1.
pnwin(X):- move2k(X,Y),'t not'(pnwin(Y)),fail.

:- table swin/1.
swin(X):- move2k(X,Y),'t not'(swin(Y)).

:- table rwin/1.
rwin(X):- get_residual(win(X),[tnot(win(Y))]),
	  tnot(rwin(Y)).

:- table simp_win/1, myfail/1.
simp_win(X):- move2k(X,Y),tnot(simp_win(Y)),myfail(X).

:- table spec_simp_win/1.
spec_simp_win(X):- move2k(X,Y),tnot(spec_simp_win(Y)),\+ pmyfail(X).

myfail(X):- X \== 1.
pmyfail(1).

:- table win_t/1.
:- table  win_tu/1.
:- table  neg/1.
win_t(X):- move2k(X,Y),tnot(win_tu(Y)).
win_tu(X):- move2k(X,Y),tnot(win_t(Y)),tnot(neg(win_tu(X))).

neg(win(1)).

end_of_file.
