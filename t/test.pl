
:- use_module(library(dra)).

:- table((p/2, q/2)).


p(X,Y) :- p(X,Z), q(Z,Y).
p(X,Y) :- e(X,Y).
q(X,Y) :- p(X,Y).

e(1,2).
e(2,3).

