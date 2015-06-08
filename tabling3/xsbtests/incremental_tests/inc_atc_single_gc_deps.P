:- import find_components/1, find_forward_dependencies/1, find_backward_dependencies/1, 
	find_answers/1
from tables.
%:- import start_forest_view/1 from tables.
%?- start_forest_view(userout).
/*
test:- p1(_,_,_),p1(a,b,_),s,abolish_table_call(undef),fail.
test:- get_residual(p1(A,B,C),D),writeln(get_residual(p1(A,B,C),D)),fail.
test:- get_residual(s,F),writeln(get_residual(s,F)),fail.
%test:- p1(_,_,_),p1(_,_,_),s,writeln(atc1),abolish_table_call(undef),fail.
*/
test:- p1(_,_,_),p1(_,_,_),s,abolish_table_call(undef),fail.
test:- get_residual(p1(A,B,C),D),writeln(get_residual(p1(A,B,C),D)),fail.
test:- get_residual(s,F),writeln(get_residual(s,F)),fail.
test:- p1(_,_,_),s,abolish_table_call(undef),abolish_table_call(p1(_,_,_)),fail.
test:- get_residual(p1(A,B,C),D),writeln(get_residual(p1(A,B,C),D)),fail.
/*
test:- get_residual(s,F),writeln(get_residual(s,F)),fail.
%test:- p1(_,_,_),p1(_,_,_),s,abolish_table_call(undef,[abolish_tables_singly]),fail.
%test:- get_residual(p1(A,B,C),D),writeln(get_residual(p1(A,B,C),D)),fail.
%test:- get_residual(s,F),writeln(get_residual(s,F)),fail.
test:- p2(_,_,_),abolish_table_call(r2(_,_,_)),abolish_table_call(r3(_,_,_)),fail.
test:- get_residual(p2(A,B,C),F),writeln(get_residual(p2(A,B,C),F)),fail.
test:- get_residual(r2(A,B,C),F),writeln(get_residual(r2(A,B,C),F)),fail.
test:- get_residual(r3(A,B,C),F),writeln(get_residual(r3(A,B,C),F)),fail.
test:- writeln('cascading abolish test completed').
*/

t1:- p1(_,_,_),abolish_table_call(undef),fail.

t2:- p1(A,B,C),writeln(p1(A,B,C)),p1(_,_,_),abolish_table_call(undef),fail.

:- table p1/3, r1/3,undef/0 as incremental.
%:- table undef/0 as opaque.
p1(X,Y,Z):- r1(X,Y,Z).

r1(a,b,c):- undef.
r1(a,b,d):- undef.
r1(a,c,c):- undef.
r1(a,c,d):- undef.
r1(b,c,d):- undef.
r1(b,c,e):- undef.
r1(c,c,e):- undef.
r1(d,c,e):- undef.
r1(3,c,e):- undef.
r1(f,c,e):- undef.
r1(g,c,e):- undef.
r1(h,c,e):- undef.
r1(i,c,e):- undef.

%undef :- tnot(undef).
undef.


:- table s/0,t/0.
s:- tnot(t).

t:- tnot(undef).

:- table p2/3,  r2/3,  r3/3.
p2(X,Y,Z):- r2(X,Y,Z).
p2(X,Y,Z):- r3(X,Y,Z).

r2(a,b,c):- undef.
r2(a,b,d):- undef.
r2(a,c,c):- undef.
r2(a,c,d):- undef.
r2(b,c,d):- undef.
r2(b,c,e):- undef.
r2(c,c,e):- undef.

r3(d,c,e):- undef.
r3(3,c,e):- undef.
r3(f,c,e):- undef.
r3(g,c,e):- undef.
r3(h,c,e):- undef.
r3(i,c,e):- undef.
end_of_file.

%----------------
t2_ni:- p1_ni(A,B,C),writeln(p1_ni(A,B,C)),p1_ni(_,_,_),abolish_table_call(undef_ni),fail.

t3_ni:- p1(A,B,C),writeln(p1(A,B,C)),p1(_,_,_),fail.

:- table p1_ni/3, r1_ni/3,undef_ni/0.

p1_ni(X,Y,Z):- r1_ni(X,Y,Z).

r1_ni(a,b,c):- undef_ni.
r1_ni(a,b,d):- undef_ni.
r1_ni(a,c,c):- undef_ni.
r1_ni(a,c,d):- undef_ni.
r1_ni(b,c,d):- undef_ni.
r1_ni(b,c,e):- undef_ni.
r1_ni(c,c,e):- undef_ni.
r1_ni(d,c,e):- undef_ni.
r1_ni(3,c,e):- undef_ni.
r1_ni(f,c,e):- undef_ni.
r1_ni(g,c,e):- undef_ni.
r1_ni(h,c,e):- undef_ni.
r1_ni(i,c,e):- undef_ni.

%undef_ni :- tnot(undef_ni).
undef_ni.

