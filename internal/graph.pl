
%%
%%              A <---> B
%%             ^ ^     /
%%            /   \   /
%%           /     v v
%%          D       C
%%          |
%%          |
%%          v
%%          E
%%

edge( a, b ).
edge( a, c ).

edge( b, a ).
edge( b, c ).

edge( c, a ).

edge( d, e ).
edge( d, a ).


path( A, B ) :-  edge( A, B ).

path( A, B ) :-  path( A, C ), path( C, B ).

q1 :-  path( a, a ).      % expected:  yes
q2 :-  path( a, d ).      % expected:  no
q3 :-  path( c, c ).      % expected:  yes
q4 :-  path( c, b ).      % expected:  yes
q5 :-  path( c, e ).      % expected:  no
q6 :-  path( _X, _Y ),    % expected:  see the picture
       writeln( _X -> _Y ).
