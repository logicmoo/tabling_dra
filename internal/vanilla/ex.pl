p( a ) :-  b( a ).
p( X ) :-  q( X, Y ),  r( Y ).

b( _ ).

q( X, X ).

r( b ).
