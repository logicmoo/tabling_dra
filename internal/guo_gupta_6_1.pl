p( 1, 1 ).
p( X, Y ) :-  p( X, S ),  p( X, T ),  q( S, T, Y ).
p( 1, 2 ).

q( 1, 2, 3 ).

%?- p( 1, X ).    % expected: p( 1, 1 ), p( 1, 2 ), p( 1, 3 )
