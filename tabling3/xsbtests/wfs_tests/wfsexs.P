%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the original code for testing programs from David S. Warren
%
% To use this, first load the file testall, and then load this file.  Then 
% 	:- test_all.
% will run all the test examples.
%%%%%%%%%%%%%%%%%%%%%%%% cut here %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% query(+Pgm_num,+Subgoals,+Query,[+True_instances],[+Undef_instances]).

/* Example 11: */
query(p11,win(_),[win(a),win(b),win(c),win(d)],[win(b),win(c)],[]).
pgm(p11,rule(win(X),[m(X,Y),\+win(Y)])).
pgm(p11,rule(m(a,b),[])).
pgm(p11,rule(m(b,c),[])).
pgm(p11,rule(m(c,d),[])).
pgm(p11,rule(m(b,d),[])).

/* Example 12: */
query(p12,win(_),[win(a),win(b),win(c)],[win(b)],[]).
pgm(p12,rule(win(X),[m(X,Y),\+win(Y)])).
pgm(p12,rule(m(a,b),[])).
pgm(p12,rule(m(b,a),[])).
pgm(p12,rule(m(b,c),[])).

/* Example 13: */
query(p13,win(_),[win(a),win(b),win(c),win(d)],[win(c)],[win(a),win(b)]).
pgm(p13,rule(win(X),[m(X,Y),\+win(Y)])).
pgm(p13,rule(m(a,b),[])).
pgm(p13,rule(m(b,a),[])).
pgm(p13,rule(m(b,c),[])).
pgm(p13,rule(m(c,d),[])).

/* Example 14: */
query(p14,p,[p],[],[p]).
pgm(p14,rule(p,[\+p])).

/* Example 15: */
query(p15,s,[s,p,q,r],[s],[]).
pgm(p15,rule(s,[\+p,\+q,\+r])). 
pgm(p15,rule(p,[\+s,\+r,q])).
pgm(p15,rule(q,[\+p,r])).
pgm(p15,rule(r,[\+q,p])). 

/* Example 16: */
query(p16,s,[s,p,q,r],[s],[]).
pgm(p16,rule(s,[\+p,\+q,\+r])). 
pgm(p16,rule(p,[q,\+s,\+r])).
pgm(p16,rule(q,[r,\+p])).
pgm(p16,rule(r,[p,\+q])).

/* Example 17: nested multiple dependency */
query(p17,s,[s,p,q,r,s1,p1,q1,r1,s2,p2,q2,r2,s3,p3,q3,r3,
	     s4,p4,q4,r4,s5,p5,q5,r5],[s,s1,s2,s3,s4,s5],[]).
pgm(p17,rule(s,[\+p,\+q,\+r,s1])). 
pgm(p17,rule(p,[\+r,\+s,s1,q])).
pgm(p17,rule(q,[\+p,s1,r])).
pgm(p17,rule(r,[\+q,s1,p])). 
pgm(p17,rule(s1,[\+p1,\+q1,\+r1,s2])). 
pgm(p17,rule(p1,[\+r1,\+s1,s2,q1])).
pgm(p17,rule(q1,[\+p1,s2,r1])).
pgm(p17,rule(r1,[\+q1,s2,p1])). 
pgm(p17,rule(s2,[\+p2,\+q2,\+r2,s3])). 
pgm(p17,rule(p2,[\+r2,\+s2,s3,q2])).
pgm(p17,rule(q2,[\+p2,s3,r2])).
pgm(p17,rule(r2,[\+q2,s3,p2])). 
pgm(p17,rule(s3,[\+p3,\+q3,\+r3,s4])). 
pgm(p17,rule(p3,[\+r3,\+s3,s4,q3])).
pgm(p17,rule(q3,[\+p3,s4,r3])).
pgm(p17,rule(r3,[\+q3,s4,p3])). 
pgm(p17,rule(s4,[\+p4,\+q4,\+r4,s5])). 
pgm(p17,rule(p4,[\+r4,\+s4,s5,q4])).
pgm(p17,rule(q4,[\+p4,s5,r4])).
pgm(p17,rule(r4,[\+q4,s5,p4])). 
pgm(p17,rule(s5,[\+p5,\+q5,\+r5])). 
pgm(p17,rule(p5,[\+s5,\+r5,q5])).
pgm(p17,rule(q5,[\+p5,r5])).
pgm(p17,rule(r5,[\+q5,p5])). 

/* Example 18: */
query(p18,p,[p,q,r],[p,q,r],[]).
pgm(p18,rule(p,[q,r])).
pgm(p18,rule(p,[r])).
pgm(p18,rule(q,[])).
pgm(p18,rule(r,[])).

/* Example 19: */
query(p19,r(X,Y),[r(a,b),r(a,c),r(a,d),r(b,a),r(c,a),r(d,a),
		  p(a,b),p(a,c),p(a,d),p(b,a),p(c,a),p(d,a)],
		 [r(a,b),r(a,c),r(a,d),p(a,b),p(a,c),p(a,d)],[]).
pgm(p19,rule(r(X,Y),[p(X,Y),\+p(Y,X)])).
pgm(p19,rule(p(X,Y),[q(X,Y)])).
pgm(p19,rule(p(X,Y),[q(X,Z),p(Z,Y)])).
pgm(p19,rule(q(a,b),[])).
pgm(p19,rule(q(b,c),[])).
pgm(p19,rule(q(c,d),[])).
pgm(p19,rule(q(d,b),[])).


% Przymusinski's example 9.1 (PODS'89)
% a should fail.
query(p20,a,[a,b,c],[c],[]).
pgm(p20,rule(a,[\+b,\+c])).
pgm(p20,rule(b,[a])).
pgm(p20,rule(c,[])).


% Przymusinski's examples
% T = {c} U = {a,b}
query(p21,a,[a,b,c],[c],[a,b]).
pgm(p21,rule(a,[\+b,c])).
pgm(p21,rule(b,[\+a])).
pgm(p21,rule(c,[])).

% Przymusinski's example 4.1 (PODS'89)
% ans: T={b}, F={a,c}, U={p,q}
query(p22,p,[p,q,a,b,c],[b],[p,q]).
pgm(p22,rule(a,[fail])).
pgm(p22,rule(b,[\+a])).
pgm(p22,rule(c,[\+b])).
pgm(p22,rule(c,[a,\+p])).
pgm(p22,rule(p,[\+q])).
pgm(p22,rule(q,[\+p,b])).

% van Gelder's example 5.1 (PODS'89)
% AFP = {c,\+p,\+q,\+r,\+s,\+t}
query(p23,(d,p),[p,q,r,s,t,a,b,c,d],[c],[a,b,d]).
pgm(p23,rule(a,[\+b,c])).
pgm(p23,rule(b,[\+a])).
pgm(p23,rule(c,[])).
pgm(p23,rule(d,[a,c,a])).
pgm(p23,rule(p,[\+r,q])).
pgm(p23,rule(p,[\+s,r])).
pgm(p23,rule(p,[t])).
pgm(p23,rule(q,[p])).
pgm(p23,rule(r,[q])).
pgm(p23,rule(r,[\+c])).
pgm(p23,rule(s,[fail])).
pgm(p23,rule(t,[fail])).

% Ross's example 3.2 (from Przymusinsk{a,i}) (PODS'89)
% (X=p;X=q;X=r;X=s),que(X,p10).
query(p24,p,[p,q,r,s],[s],[]).
pgm(p24,rule(p,[\+s,q,\+r])).
pgm(p24,rule(q,[r,\+p])).
pgm(p24,rule(r,[p,\+q])).
pgm(p24,rule(s,[\+p,\+q,\+r])).

% Ross's example 3.2 (from Przymusinsk{a,i}) (PODS'89)
% WFM = {s}
query(p25,s,[p,q,r,s],[s],[]).
pgm(p25,rule(p,[q,\+s,\+r])).
pgm(p25,rule(q,[r,\+p])).
pgm(p25,rule(r,[p,\+q])).
pgm(p25,rule(s,[\+p,\+q,\+r])).

% Ross's example 3.2 (from Przymusinsk{a,i}) (PODS'89)
% WFM = {s}
query(p26,l,[l,m,n,p,q,r,s],[l,n,s],[]).
pgm(p26,rule(l,[\+p,s])).
pgm(p26,rule(m,[\+s])).
pgm(p26,rule(n,[\+p])).
pgm(p26,rule(p,[\+s,\+r,q])).
pgm(p26,rule(q,[\+p,r])).
pgm(p26,rule(r,[\+q,p])).
pgm(p26,rule(s,[\+p,\+q,\+r])).

% Ross's example 3.2 (from Przymusinsk{a,i}) (PODS'89)
% WFM = {s}
query(p27,s,[p,q,r,s,ns],[s],[]).
pgm(p27,rule(p,[\+r,ns,q])).
pgm(p27,rule(ns,[\+s])).
pgm(p27,rule(q,[r,\+p])).
pgm(p27,rule(r,[p,\+q])).
pgm(p27,rule(s,[\+p,\+q,\+r])).

% Ross's example 3. (from vanGelder) (PODS'89)
% xwam goes into an infinite loop on w(0).
query(p29,w(0),[w(0),u(0)],[w(0)],[]).
pgm(p29,rule(e(s(0),s(s(0))),[])).
pgm(p29,rule(e(s(0),0),[])).
pgm(p29,rule(e(s(X),s(s(X))),[e(X,s(X))])).
pgm(p29,rule(e(s(X),0),[e(X,0)])).
pgm(p29,rule(w(X),[\+u(X)])).
pgm(p29,rule(w(X),[e(Y,X),\+w(Y)])).
pgm(p29,rule(u(0),[fail])).

% Subrahmanian's example showing difference between Stable and WF.
% WFM: p is undef; Stable: p is true
query(p30,p,[p,q,r],[],[p,q,r]).
pgm(p30,rule(p,[q])).
pgm(p30,rule(p,[\+r])).
pgm(p30,rule(r,[\+q])).
pgm(p30,rule(q,[\+r])).

% Example to show that undef's must be propagated (through Prolog predicates)
query(p31,(q(_),q(b)),[q(a),q(b),r],[],[q(b),r]).
pgm(p31,rule(q(X),[p(X),eq(X,b)])).
pgm(p31,rule(eq(Y,Y),[])).
pgm(p31,rule(p(a),[])).
pgm(p31,rule(p(_X),[r])).
pgm(p31,rule(r,[\+r])).

% Example 3.1 VanG,Ross,Schlipf PODS'88
% {\+p,\+q,\+r,\+s,c}
query(p32,(a,p),[a,b,c,p,q,r,s],[c],[a,b]).
pgm(p32,rule(a,[c,\+b])).
pgm(p32,rule(b,[\+a])).
pgm(p32,rule(c,[])).
pgm(p32,rule(p,[q,\+r])).
pgm(p32,rule(p,[r,\+s])).
pgm(p32,rule(q,[p])).
pgm(p32,rule(r,[q])).
pgm(p32,rule(s,[fail])).

% Example 5.1 VanG,Ross,Schlipf PODS'88
% {}
query(p33,a,[a,b],[],[a,b]).
pgm(p33,rule(a,[\+b])).
pgm(p33,rule(b,[\+a])).

% Example 6.1 VanG,Ross,Schlipf PODS'88
% {}
query(p34,p,[a,b,p],[],[a,b,p]).
pgm(p34,rule(a,[\+b])).
pgm(p34,rule(b,[\+a])).
pgm(p34,rule(p,[\+p])).
pgm(p34,rule(p,[\+b])).

% Example 6.2 VanG,Ross,Schlipf PODS'88
% {}
query(p35,a,[a,b,c],[],[a,b,c]).
pgm(p35,rule(a,[\+b])).
pgm(p35,rule(b,[\+a])).
pgm(p35,rule(c,[a,b])).
pgm(p35,rule(a,[\+c])).

% Example 7.1 VanG,Ross,Schlipf PODS'88
% {loaded(0),\+shoots(0),succ(0,1),loaded(1),shoots(1),noise(1),\+noise(0)}
query(p36,noise(1),[noise(0),loaded(0),shoots(0),noise(1),loaded(1),shoots(1)],
	[loaded(0),noise(1),loaded(1),shoots(1)],[]).
pgm(p36,rule(noise(1),[loaded(1),shoots(1)])).
pgm(p36,rule(loaded(1),[succ(0,1),loaded(0),\+shoots(0)])).
pgm(p36,rule(succ(0,1),[])).
pgm(p36,rule(loaded(0),[])).
pgm(p36,rule(shoots(1),[])).

% Example 7.2 VanG,Ross,Schlipf PODS'88
% call a(X,Y)
query(p37,a(_,_),[a(2,3),a(3,2),a(3,3),p(3,3),e(3,3)],
		 [a(2,3),a(3,2),a(3,3),e(3,3)],[]).
pgm(p37,rule(p(X,Y),[b(X,Y)])).
pgm(p37,rule(p(X,Y),[b(X,U),p(U,Y)])).
pgm(p37,rule(e(X,Y),[g(X,Y)])).
pgm(p37,rule(e(X,Y),[g(X,U),e(U,Y)])).
pgm(p37,rule(a(X,Y),[e(X,Y),\+p(X,Y)])).
pgm(p37,rule(b(1,2),[])).
pgm(p37,rule(b(2,1),[])).
pgm(p37,rule(g(2,3),[])).
pgm(p37,rule(g(3,2),[])).

% These 3 are from Teodor:

query(p39,(a(_X),a(s),b(s)),[a(s),b(s)],[],[a(s),b(s)]).
pgm(p39,rule(b(s),[\+b(s)])).
pgm(p39,rule(a(A),[b(A)])).

% ???
query(p40,a(_),[a(s),a(t),b(s)],[a(s),a(t)],[b(s)]).
pgm(p40,rule(b(s),[\+b(s)])).
pgm(p40,rule(a(A),[b(A)])).
pgm(p40,rule(a(_A),[])).

% takes >3 mins CPU and >1.7 meg of mem!
% answers(r(_X,_Y),p42,[tv(r(a,b),yes),tv(r(a,c),yes),tv(r(a,d),yes),tv(r(a,e),yes),tv(r(a,f),yes),tv(r(a,g),yes),tv(r(a,h),yes),tv(r(a,i),yes),tv(r(a,j),yes),tv(r(a,k),yes),tv(r(a,l),yes)]))..

query(p42,r(_X,_Y),[p(a,b),r(a,b),r(a,c),r(a,d),r(a,e),r(a,f),
		    r(a,g),r(a,h),r(a,i),r(a,j),r(a,k),r(a,l)],
		   [p(a,b),r(a,b),r(a,c),r(a,d),r(a,e),r(a,f),
		    r(a,g),r(a,h),r(a,i),r(a,j),r(a,k),r(a,l)],[]).

pgm(p42,rule(r(X,Y), [p(X,Y),\+(p(Y,X))])).

pgm(p42,rule(p(X,Y),[q(X,Y)])).
pgm(p42,rule(p(X,Y),[q(X,Z),p(Z,Y)])).
pgm(p42,rule(q(a,b),[])).
pgm(p42,rule(q(b,c),[])).
pgm(p42,rule(q(c,d),[])).
pgm(p42,rule(q(d,e),[])).
pgm(p42,rule(q(e,f),[])).
pgm(p42,rule(q(f,g),[])).
pgm(p42,rule(q(g,h),[])).
pgm(p42,rule(q(h,i),[])).
pgm(p42,rule(q(i,j),[])).
pgm(p42,rule(q(j,k),[])).
pgm(p42,rule(q(k,l),[])).
pgm(p42,rule(q(l,b),[])).

% vanGelder (alternating fix point) ex 5.2.a
% answers(w(_X),p43,[tv(w(b),yes),tv(w(e),yes),tv(w(g),yes)])).
query(p43,w(_),[w(a),w(b),w(c),w(d),w(e),w(f),w(g)],[w(b),w(e),w(g)],[]).
pgm(p43,rule(w(X), [m(X,Y),\+(w(Y))])).
pgm(p43,rule(m(a,b),[])).
pgm(p43,rule(m(a,e),[])).
pgm(p43,rule(m(b,c),[])).
pgm(p43,rule(m(b,d),[])).
pgm(p43,rule(m(e,f),[])).
pgm(p43,rule(m(e,g),[])).
pgm(p43,rule(m(g,h),[])).
pgm(p43,rule(m(g,i),[])).

% vanGelder (alternating fix point) ex 5.2.b
% answers(w(_X),p44,[tv(w(a),maybe),tv(w(b),maybe),tv(w(c),yes)])).
query(p44,w(_),[w(a),w(b),w(c),w(d)],[w(c)],[w(a),w(b)]).
pgm(p44,rule(w(X), [m(X,Y),\+w(Y)])).
pgm(p44,rule(m(a,b),[])).
pgm(p44,rule(m(b,a),[])).
pgm(p44,rule(m(b,c),[])).
pgm(p44,rule(m(c,d),[])).

% vanGelder (alternating fix point) ex 5.2.c
% answers(w(_X),p45,[tv(w(b),yes)])).
query(p45,w(_),[w(a),w(b),w(c)],[w(b)],[]).
pgm(p45,rule(w(X), [m(X,Y),\+w(Y)])).
pgm(p45,rule(m(a,b),[])).
pgm(p45,rule(m(b,a),[])).
pgm(p45,rule(m(b,c),[])).


% answers(q,p46,[])).
% answers(p(_X),p46,[tv(p(a),maybe),tv(p(b),yes)])).
query(p46,q,[p(a),p(b),q,r],[p(b),r],[p(a)]).
pgm(p46,rule(q, [\+r])).
pgm(p46,rule(r, [p(_)])).
pgm(p46,rule(p(a), [\+p(a)])).
pgm(p46,rule(p(b), [])).

/* Example 47: */
query(p47,p,[p,q,l],[p],[]).
pgm(p47,rule(l, [\+p])).
pgm(p47,rule(p, [\+q])).
pgm(p47,rule(p, [])).
pgm(p47,rule(q, [\+p])).

/* Example 48: */
query(p48,p,[p,q,r,s],[p],[]).
pgm(p48,rule(p,[\+q])).
pgm(p48,rule(q,[\+p])).
pgm(p48,rule(p,[\+r])).
pgm(p48,rule(r,[\+p])).
pgm(p48,rule(p,[\+s])).
pgm(p48,rule(s,[fail])).

/* Example 49: */
query(p49,q(a),[p(a),q(a),r(a)],[q(a),r(a)],[]).
pgm(p49,rule(q(X),[\+p(X)])).
pgm(p49,rule(p(Y),[\+p(Y), \+r(Y)])).
pgm(p49,rule(r(a),[])).

/* Example 50: */
query(p50,q(_X),[q(a),q(b),p(a),p(b),r(a),r(b)],[q(b),p(a),r(a)],[]).
pgm(p50,rule(q(a),[\+p(a)])).
pgm(p50,rule(q(b),[\+p(b)])).
pgm(p50,rule(p(a),[\+p(a), \+r(a)])).
pgm(p50,rule(p(a),[\+p(b), \+r(b)])).
pgm(p50,rule(r(a),[])).


%
% Tests where the results are mostly undefined...
%

query(p51,p0,[p0,p1,p2],[],[p0,p1,p2]).
pgm(p51,rule(p0,[\+p0,\+p1,\+p2])).
pgm(p51,rule(p1,[\+p1,\+p2,\+p0])).
pgm(p51,rule(p2,[\+p2,\+p0,\+p1])).

query(p52,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]).
pgm(p52,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).

query(p52a,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10],
	      [],
	      [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]).
pgm(p52a,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p52a,rule(p1,[\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0])).
pgm(p52a,rule(p2,[\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1])).
pgm(p52a,rule(p3,[\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2])).
pgm(p52a,rule(p4,[\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3])).
pgm(p52a,rule(p5,[\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4])).
pgm(p52a,rule(p6,[\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5])).
pgm(p52a,rule(p7,[\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6])).
pgm(p52a,rule(p8,[\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7])).
pgm(p52a,rule(p9,[\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8])).
pgm(p52a,rule(p10,[\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9])).

query(p53,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]).
pgm(p53,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10])).
pgm(p53,rule(p0,[\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0])).
pgm(p53,rule(p1,[\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1])).
pgm(p53,rule(p2,[\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2])).
pgm(p53,rule(p3,[\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3])).
pgm(p53,rule(p4,[\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4])).
pgm(p53,rule(p5,[\+p6,\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5])).
pgm(p53,rule(p6,[\+p7,\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6])).
pgm(p53,rule(p7,[\+p8,\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7])).
pgm(p53,rule(p8,[\+p9,\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8])).
pgm(p53,rule(p9,[\+p10,\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9])).

query(p54,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12]).
pgm(p54,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).
pgm(p54,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,
		 \+p7,\+p8,\+p9,\+p10,\+p11,\+p12])).

query(p55,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14]).
pgm(p55,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		 \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		  \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		  \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		  \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p13,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		  \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).
pgm(p55,rule(p14,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,
		  \+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14])).

query(p56,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16]).
pgm(p56,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p13,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p14,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p15,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).
pgm(p56,rule(p16,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16])).

query(p57,p0,
	[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18],
	[],
	[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18]).
pgm(p57,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,
		 \+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,
		 \+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,
		 \+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,
		 \+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,
		 \+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		 \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p13,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p14,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p15,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p16,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p17,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).
pgm(p57,rule(p18,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,
		  \+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18])).

query(p58,p0,
   [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20],
   [],
   [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20]).
pgm(p58,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p13,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p14,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p15,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p16,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p17,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p18,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p19,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).
pgm(p58,rule(p20,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20])).

query(p59,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
	      p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22],
	     [],
	     [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
	      p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22]).
pgm(p59,rule(p0,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p1,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p2,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p3,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p4,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p5,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p6,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p7,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p8,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p9,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p10,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p11,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p12,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p13,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p14,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p15,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p16,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p17,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p18,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p19,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p20,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p21,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).
pgm(p59,rule(p22,[\+p0,\+p1,\+p2,\+p3,\+p4,\+p5,\+p6,\+p7,\+p8,\+p9,\+p10,\+p11,\+p12,\+p13,\+p14,\+p15,\+p16,\+p17,\+p18,\+p19,\+p20,\+p21,\+p22])).


query(p60,(p(X),q(X)),
	  [p(1),p(2),p(3),p(4),q(1),q(2),q(3),q(4),s(1),s(2),s(3),s(4)],
	  [p(2),p(3),p(4),q(3),q(4),s(2)],[]).
pgm(p60,rule(q(X),[u(X), \+s(X)])).
pgm(p60,rule(q(X),[q(Y), t(X,Y)])).
%pgm(p60,rule(q(X),[t(X,Y),q(Y)])).
pgm(p60,rule(p(X),[u(X), \+q(X)])).
pgm(p60,rule(p(X),[p(Y), t(X,Y)])).
%pgm(p60,rule(p(X),[t(X,Y),p(Y)])).
pgm(p60,rule(u(2),[])).
pgm(p60,rule(u(3),[])).
pgm(p60,rule(s(2),[])).
pgm(p60,rule(t(2,1),[])).
pgm(p60,rule(t(3,2),[])).
pgm(p60,rule(t(4,3),[])).


query(p62,l,[l,p,q,s,r,t],[],[l,p,q,s,r,t]).
pgm(p62,rule(l,[\+p])).
pgm(p62,rule(p,[\+q,\+r])).
pgm(p62,rule(q,[\+s])).
pgm(p62,rule(s,[\+q])).
pgm(p62,rule(r,[\+t])).
pgm(p62,rule(t,[\+r])).

query(p63,t,[p,q,s,r,t],[],[p,q,s,r,t]).
pgm(p63,rule(t,[\+s])).
pgm(p63,rule(s,[\+p])).
pgm(p63,rule(p,[q])).
pgm(p63,rule(p,[r])).
pgm(p63,rule(r,[\+q])).
pgm(p63,rule(q,[\+r])).

query(p64,p,[p,q,s,r,t,u,v],[],[p,q,s,r,t,u,v]).
pgm(p64,rule(p,[\+q,\+r])).
pgm(p64,rule(p,[\+u])).
pgm(p64,rule(q,[\+s])).
pgm(p64,rule(q,[\+u])).
pgm(p64,rule(s,[\+q])).
pgm(p64,rule(r,[\+t])).
pgm(p64,rule(t,[\+r])).
pgm(p64,rule(u,[\+v])).
pgm(p64,rule(v,[\+u])).


query(p65,p,[p,q],[],[p,q]).
pgm(p65,rule(p,[q])).
pgm(p65,rule(q,[\+q])).


query(p66,l,[l,p,q,r,s,t],[],[l,p,q,r,s,t]).
pgm(p66,rule(l,[\+p])).
pgm(p66,rule(p,[\+q])).
pgm(p66,rule(p,[q])).
pgm(p66,rule(p,[\+r])).
pgm(p66,rule(q,[\+s])).
pgm(p66,rule(s,[\+q])).
pgm(p66,rule(r,[\+t])).
pgm(p66,rule(t,[\+r])).

query(p67,p,[p,q],[],[p]).
pgm(p67,rule(p,[\+p])).
pgm(p67,rule(p,[q])).
pgm(p67,rule(q,[q])).

%----------------------------------------------------------------------------
/* Example p77: CURRENTLY GIVES WRONG RESULTS */
query(p77,p,[p,q,r,s,t],[],[p,q,r,s,t]).
pgm(p77,rule(p,[q])).
pgm(p77,rule(q,[r])).
pgm(p77,rule(q,[\+t])).
pgm(p77,rule(r,[s])).
pgm(p77,rule(s,[p])).
pgm(p77,rule(t,[r])).
%----------------------------------------------------------------------------

query(p78,s,[p,q,s],[s],[]).
pgm(p78,rule(s,[\+p,\+q])).
pgm(p78,rule(p,[\+s,q])).
pgm(p78,rule(q,[\+s,p])).

% example of stratified program that needs delaying
query(p79,p(a),[p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c)],
	       [p(a),q(a),q(b),r(b),s(b),s(c)],[]).
pgm(p79,rule(p(X),[q(X),\+r(X)])).
pgm(p79,rule(q(a),[])).
pgm(p79,rule(q(b),[])).
pgm(p79,rule(r(X),[q(X),s(X)])).
pgm(p79,rule(s(b),[])).
pgm(p79,rule(s(c),[])).

query(p80,m,[m,p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c)],
	    [m,p(b),q(a),q(b),r(a),s(a),s(c)],[]).
pgm(p80,rule(m,[q(X),p(X)])).
pgm(p80,rule(p(X),[q(X),\+r(X)])).
pgm(p80,rule(q(a),[])).
pgm(p80,rule(q(b),[])).
pgm(p80,rule(r(X),[q(X),s(X)])).
pgm(p80,rule(s(a),[])).
pgm(p80,rule(s(c),[])).

query(p81,m(a),
   [m(a),m(b),m(c),p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c)],
   [m(a),m(b),p(a),p(b),q(a),q(b),r(c),s(a),s(c)],[]).
pgm(p81,rule(m(X),[q(X),p(X)])).
pgm(p81,rule(p(X),[q(X),\+r(X)])).
pgm(p81,rule(q(a),[])).
pgm(p81,rule(q(b),[])).
pgm(p81,rule(r(X),[s(X),\+q(X)])).
pgm(p81,rule(s(a),[])).
pgm(p81,rule(s(c),[])).

query(p82,m(a),
   [m(a),m(b),m(c),n(a),n(b),n(c),p(a),p(b),p(c),
    q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c),t(a),t(b),t(c)],
   [m(b),p(b),q(a),q(b),r(a),r(c),s(a),s(c)],[]).
pgm(p82,rule(m(X),[n(X)])).
pgm(p82,rule(m(X),[p(X)])).
pgm(p82,rule(n(X),[q(X),t(X)])).
pgm(p82,rule(p(X),[q(X),\+r(X)])).
pgm(p82,rule(q(a),[])).
pgm(p82,rule(q(b),[])).
pgm(p82,rule(r(X),[s(X),\+n(X)])).
pgm(p82,rule(s(a),[])).
pgm(p82,rule(s(c),[])).
pgm(p82,rule(t(_),[fail])).

query(p83,m,[m,a,b,c,d],[m,a,c],[]).
pgm(p83,rule(m,[c,a])).
pgm(p83,rule(c,[b])).
pgm(p83,rule(c,[])).
pgm(p83,rule(b,[c,d])).
pgm(p83,rule(a,[\+b])).
pgm(p83,rule(d,[fail])).

/* example that leads to unnecessary floundering */
query(p84,m,[m,a,b,c,d,r(a)],[a,c],[]).
pgm(p84,rule(m,[c,\+a,\+r(X)])).
pgm(p84,rule(c,[b])).
pgm(p84,rule(c,[])).
pgm(p84,rule(b,[c,d])).
pgm(p84,rule(a,[\+b])).
pgm(p84,rule(d,[fail])).
pgm(p84,rule(r(X),[fail])).

/* example that shows incorrect completion */
query(p85,p(X),[a,b,c,d,p(1),p(2),p(3),q(1),q(2),q(3)],
	       [a,b,p(1),p(2),q(1),q(2)],[]).
pgm(p85,rule(p(X),[a,\+c,q(X)])).
pgm(p85,rule(p(1),[])).
pgm(p85,rule(a,[b])).
pgm(p85,rule(b,[])).
pgm(p85,rule(c,[b,d])).
pgm(p85,rule(d,[fail])).
pgm(p85,rule(q(X),[p(X)])).
pgm(p85,rule(q(2),[])).

/* example from Stuckey */
query(p86,p(X),[p(a),
		p(b1),p(b2),p(b3),p(b4),p(b5),p(b6),p(b7),p(b8),p(b9),p(b10),
		p(c1),p(c2),p(c3),p(c4),p(c5),p(c6),p(c7),p(c8),p(c9)],
	       [p(b1),p(b3),p(b5),p(b7),p(b9),p(c2)],[]).
pgm(p86,rule(p(X),[t(X,Y,Z),\+p(Y),\+p(Z)])).
pgm(p86,rule(p(X),[p0(X)])).
pgm(p86,rule(p0(c2),[])).
pgm(p86,rule(t(a,a,b1),[])).
pgm(p86,rule(t(b1,c1,b2),[])).
pgm(p86,rule(t(b2,c2,b3),[])).
pgm(p86,rule(t(b3,c3,b4),[])).
pgm(p86,rule(t(b4,c4,b5),[])).
pgm(p86,rule(t(b5,c5,b6),[])).
pgm(p86,rule(t(b6,c6,b7),[])).
pgm(p86,rule(t(b7,c7,b8),[])).
pgm(p86,rule(t(b8,c8,b9),[])).
pgm(p86,rule(t(b9,c9,b10),[])).

%----------------------------------------------------------------------------
/* Example 87: it should flounder -- NOT TESTED */
query(p87,f(X),[f(a),f(b),f(c)],[f(c)],[]).
pgm(p87,rule(f(X),[move(_,X),founded(X)])).
pgm(p87,rule(founded(X),[\+move(X,Y),founded(Y)])).
pgm(p87,rule(move(a,b),[])).
pgm(p87,rule(move(b,a),[])).
pgm(p87,rule(move(b,c),[])).

/* Example 88: it should flounder -- NOT TESTED */
query(p88,col(X),[col(a),col(b)],[]).
pgm(p88,rule(col(X),[edge(_,X),color(X)])).
pgm(p88,rule(color(X),all([\+edge(X,Y),\+color(Y)]))).
pgm(p88,rule(edge(a,c),[])).
pgm(p88,rule(edge(a,d),[])).
pgm(p88,rule(edge(c,a),[])).
pgm(p88,rule(edge(c,b),[])).
pgm(p88,rule(edge(d,b),[])).
%----------------------------------------------------------------------------

query(p89,p,[p,q,r,s],[p,r],[]).
pgm(p89,rule(p,[\+q,r])).
pgm(p89,rule(q,[\+p,s])).
pgm(p89,rule(r,[])).
pgm(p89,rule(s,[fail])).

query(p90,p,[p,q,s],[s],[p,q]).
pgm(p90,rule(p,[\+q])).
pgm(p90,rule(q,[\+p,s])).
pgm(p90,rule(s,[])).

query(p91,p,[p,q,r],[p,r],[]).
pgm(p91,rule(p,[\+q,r])).
pgm(p91,rule(q,[\+r,p])).
pgm(p91,rule(r,[\+p,q])).
pgm(p91,rule(r,[])).

