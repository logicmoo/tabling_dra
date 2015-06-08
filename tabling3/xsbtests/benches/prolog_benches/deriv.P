% generated: 7 March 1990
% option(s): 
%
%   (deriv) times10
%
%   David H. D. Warren
%
%   symbolic derivative of ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x
%
%   scaled a little for modern CPUs by creating terms of varying length --- TLS

%:- option(analyze).

bench_deriv(Type,N,Length):- 
	create_log(Length,Log),
	create_times(Length,Times),
	create_div(Length,Div),
	create_ops(Length,Ops),
	cputime(T1),
	main1(N,Log,Times,Div,Ops),
	cputime(T2),T is T2-T1,
	writeq(datum(Type,T)),writeln('.'),nl.


main1(N,Log,Times,Div,Ops):- 
	repeat(N),
	d(Log,x,_),
	d(Times,x,_),
	d(Div,x,_),
	d(Ops,x,_),
	fail.
main1(_,_,_,_,_).


main1(N) :- repeat(N), one, fail.
main1(_).

repeat(N) :- N>=1.
repeat(N) :- N>1, N1 is N-1, repeat(N1).

create_times(2,x*x):-!.
create_times(N,(T)*x):- N > 2,
	N1 is N - 1, 
	create_times(N1,T).

create_log(1,x):-!.
create_log(N,log(T)):- N > 1,
	N1 is N - 1, 
	create_log(N1,T).

create_div(2,(x/x)):-!.
create_div(N,T1):- 
	N > 2,
	T1 = (T/x),
	N1 is N - 1, 
	create_div(N1,T).

create_ops(N,T):-
	N1 is N - 1,
	create_ops1(N1,(^(x,N)+N),T).

create_ops1(1,T,(x+1)*T):- !.
create_ops1(N,T,T1):- 
	N > 1,
	N1 is N - 1, 
	create_ops1(N1,((^(x,N)+N) * T) ,T1).

d(U+V,X,DU+DV) :- !, 
    d(U,X,DU),
    d(V,X,DV).
d(U-V,X,DU-DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U/V,X,(DU*V-U*DV) / (^(V,2))) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(^(U,N),X,DU*N * (^(U,N1))) :- !, 
    integer(N),
    N1 is N-1,
    d(U,X,DU).
d(-U,X,-DU) :- !,
    d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !,
    d(U,X,DU).
d(log(U),X,DU/U) :- !,
    d(U,X,DU).
d(X,X,1) :- !.
d(_,_,0).

end_of_file.

one :- ops8, divide10, log10, times10.

ops8 :- d((x+1) * ((^(x,2)+2) * (^(x,3)+3)),x,_).

divide10 :- d(((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x,x,_).

times10 :- d(((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x,x,_).

log10 :- d(log(log(log(log(log(log(log(log(log(log(x)))))))))),x,_).

