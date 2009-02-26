r( X, Y ) :-  r( X, Z ),  r( Z, Y ).
r( X, Y ) :-  p( X, Y ),  q( Y ).

p( a, b ).
p( a, d ).
p( b, c ).

q( b ).
q( c ).

% ?- r( a, Y ).    % expected: r( a, b ), r( a, c )
%
% ?- r( b, Y ).    % expected: r( b, c )

% ?- r( c, Y ).    % expected: failure
