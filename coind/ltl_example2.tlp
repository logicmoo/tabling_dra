%% Get the translated interpreter:

:- [ 'ltl_interpreter.pl' ].


%--- An example: some states, some queries... Note the two loops.



%   S3: q <--------- S2: p, r
%     \             /  ^
%      \           /   |
%       \         /    |
%        \       /     |
%         \     /      |
%          \   /       |
%           v v        |
%         S0: p ----> S1: p, q
%


proposition( p ).
proposition( q ).
proposition( s ).


state( s0 ).
state( s1 ).
state( s2 ).
state( s3 ).

trans_all( s0, [ s1     ] ).
trans_all( s1, [ s2     ] ).
trans_all( s2, [ s0, s3 ] ).
trans_all( s3, [ s0     ] ).

holds( s0, p ).

holds( s1, p ).
holds( s1, q ).

holds( s2, p ).
holds( s2, s ).

holds( s3, q ).


q1  :-  check( s0, f p ).                        % YES !

q2  :-  check( s3, f p ).                        % YES !

q3  :-  check( s0, f( p ^ q ) ).                 % YES !

q4  :-  check( s3, f( p ^ q ) ).                 % YES !

q5  :-  check( s0, g p ).                        % NO  !

q6  :-  check( s0, g( p v q ) ).                 % YES !

