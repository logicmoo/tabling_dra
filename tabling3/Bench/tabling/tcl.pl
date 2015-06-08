
:- topl go/0, main/0.
:-table reach/2.

go:-
    must(cputime(Start)),
    top,
    cputime(End),
    T is End-Start,
    write('TIME:'),write(T),nl.

main:-top.

top:-
    reach(X,Y),
   % write(X),
    fail.
top.

reach(X,Y):-edge(X,Y).
reach(X,Y):-reach(X,Z),edge(Z,Y).

:-['sg_edge.pl'].
