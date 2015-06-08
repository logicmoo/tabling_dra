chain_ancestor_bench:- 
	bigbench(rancestor1k(1,_),[tabled_right_anc,1024,chain],1),
	bigbench(ancestor1k(1,_),[tabled_left_anc,1024,chain],500),
	bigbench(ancestor2k(1,_),[tabled_left_anc,2048,chain],500).

cycle_ancestor_bench:- 
	bigbench(rancestor1k(1,_),[tabled_right_anc,1024,cycle],1),
	bigbench(ancestor1k(1,_),[tabled_left_anc,1024,cycle],500),
	bigbench(ancestor2k(1,_),[tabled_left_tanc,2048,cycle],500).

chain_win_bench:- 
	bigbench(win(1),[win,2048,chain],200).

cycle_win_bench:- 
	bigbench(win(1),[win,2048,cycle],200).
	
:- table ancestor2k/2.
ancestor2k(X,Y):- ancestor2k(X,Z), move2k(Z,Y).
ancestor2k(X,Y):- move2k(X,Y).

:- table ancestor1k/2.
ancestor1k(X,Y):- ancestor1k(X,Z), move1k(Z,Y).
ancestor1k(X,Y):- move1k(X,Y).

:- table rancestor1k/2.
rancestor1k(X,Y):- move1k(X,Y).
rancestor1k(X,Y):- move1k(X,Z),rancestor1k(Z,Y).

pancestor2k(X,Y):- move2k(X,Z),pancestor2k(Z,Y).
pancestor2k(X,Y):- move2k(X,Y).

pancestor1k(X,Y):- move1k(X,Y).
pancestor1k(X,Y):- move1k(X,Z),pancestor1k(Z,Y). 

:- table win/1.
win(X):- move2k(X,Y),tnot(win(Y)).
