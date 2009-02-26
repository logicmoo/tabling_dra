%:- tabled r/2.

r( X, Y ) :-  p( X, Z ),  r( Z, Y ).
r( X, Y ) :-  p( X, Y ).

p( a, b ).
p( b, a ).

%?- r( a, Y ).    % expected: r( a, a ), r( a, b )

%?- r( b, Y ).    % expected: r( b, a ), r( b, b )
