
:- import trie_create/2, trie_intern/2, incr_trie_intern/2, incr_trie_uninternall/2,
	trie_interned/2 from intern.

:- table pt/3 as incremental.
pt(T,X,Y) :- 
%	writeln(executing(pt(T,X,Y))),
	trie_interned(qt(X,Y),T),Y =< 5.

:- import incr_table_update/0 from increval.

test_seq:- test_trie.

test_trie:- 
	trie_create(T,[incremental]),
	trie_intern(qt(a,1),T),	trie_intern(qt(b,3),T),
	trie_intern(qt(c,5),T),trie_intern(qt(d,7),T),
	pt(T,X,Y),incr_writeln(first_pt(T,X,Y)),
	fail.
test_trie:- incr_trie_intern(1,qt(d,4)),fail.
test_trie:- pt(1,X,Y),incr_writeln(second_pt(1,X,Y)),fail.
test_trie:- incr_trie_uninternall(1,qt(a,_X)),fail.
test_trie:- pt(1,X,Y),incr_writeln(third_pt(1,X,Y)),fail.
test_trie:- incr_trie_uninternall(1,qt(b,_X)),fail.
test_trie:- get_residual(pt(1,X,Y),_),incr_writeln(fourth_p(X,Y)),fail.
test_trie:- pt(1,_X,_Y),fail.
test_trie:- get_residual(pt(1,X,Y),_),incr_writeln(fifth_p(X,Y)),fail.
test_trie:- incr_trie_intern(1,qt(e,2)),fail.
test_trie:- get_residual(pt(1,X,Y),_),incr_writeln(sixth_p(X,Y)),fail.
test_trie:- pt(1,_X,_Y),fail.
test_trie:- get_residual(pt(1,X,Y),_),incr_writeln(seventh_p(X,Y)),fail.
test_trie.

incr_writeln(Term):- write(incr(Term)),writeln('.').

end_of_file.
