
:- import trie_interned/4, trie_intern/5, new_trie/1 from intern.
:- import conget/2, conset/2 from gensym.

eql(c,b1).
eql(g,h).
eql(aa,bbb).
eql(cc,bb).
eql(ff,hh).

:- new_trie(Handle), conset(aaa,Handle).
eql1(X,Y) :-
	conget(aaa, Handle),
	trie_interned(eql(X,Y),Handle,_,_),
	write('   '), writeln(eql(X,Y)).
eql_true(X,Y) :-
	conget(aaa, Handle),
	eql(X,Y), trie_intern(eql(Y,X),Handle,_,_Z,_),
	write('   '), writeln(eql(X,Y)).

%% Query goes into infinite loop, if we replace eql1 with eql everywhere.
%% First it interns the above facts (after switching X,Y)
%% Then it checks what was interned. It goes into a loop before
%% reaching the last interned fact, eql(b1,c)
?- writeln('Asserting:'), eql_true(X,Y), fail.
?- writeln('Results:'), eql1(X,Y), fail.
