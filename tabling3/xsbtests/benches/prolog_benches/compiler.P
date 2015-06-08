
% compile twice to factor out gc, etc.

bench_compiler(Type):- 
	compile(top_down),
	cputime(T1),
	compile(top_down),
	cputime(T2),T is T2-T1,
	writeq(datum(Type,T)),writeln('.'),nl.
