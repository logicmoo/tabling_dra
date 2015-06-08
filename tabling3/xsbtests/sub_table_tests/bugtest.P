
:- import mycall/2 from bug.

test1:- mycall(po(X,c),V),writeln(p3(V)),fail.
test2:- mycall(po(X),V),writeln(p2(V)),fail.

po(a,X,p(f(X),g(X))).
po(a,b,p(f(1),g(2))).
po(a,b,p(f(1),g(_))).
po(a,c,p(f(2),g(2))).
po(b,c,p(f(_),g(2))).


:- hilog po.
po(false,top).
po(true,top).
po(bot,top).
po(bot,true).
po(bot,false).


