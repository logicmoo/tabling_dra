
:- import reverse/2 from basics.

get_variance:-
	setof(Time,datum(Type,Time),[First|Times]),
	reverse(Times,[Last|_Rtimes]),
	Variance is Last/First,
	write(variance(Type,Variance)),tab(5),writeln(min(Type,First)),
	fail.
get_variance.

	