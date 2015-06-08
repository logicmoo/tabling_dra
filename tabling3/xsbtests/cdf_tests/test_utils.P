:- import prepare/1, gensym/2 from gensym.
:- import numbervars/1 from num_vars.

:- prepare(0).

test_display(Goal):- 
	copy_term(Goal,Goala),
	numbervars(Goala),
	gensym(test,F),
	write(F),write(' ------------- (goal) -------------'),writeln(Goala),
	call(Goal),
	numbervars(Goal),
	write(F),write(': '),writeln(Goal),
	fail.
test_display(_).

