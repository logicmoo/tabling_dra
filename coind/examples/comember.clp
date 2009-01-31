%% This is example2.clp from Luke Simon's thesis, with some tweaks.

:- bottom num/1, drop/3 .
:- top comember/2.

% A number.
num( 0 ).
num( s( N ) ) :-  num( N ).


% A stream of numbers.
:- coinductive stream/1 .

stream( [ H | T ] ) :-  num( H ),  stream( T ).



% Similar to append/3.
:- coinductive append1/3 .

append1( [], X, X ).
append1( [ H | T ], Y, [ H | Z ] ) :-  append1( T, Y, Z ).


% Similar to member/2.
:- coinductive member1/2 .

member1( X, [ X | _ ] ).
member1( X, [ _ | T ] ) :-  member1( X, T ).


% Ditto, but not coinductive
member2( X, [ X | _ ] ).
member2( X, [ _ | T ] ) :-  member2( X, T ).


% Drop some "occurrence" of arg1 from arg2, yielding arg3.
% ("Occurrence" of X = something unifiable with X.)
drop( H, [ H | T ] , T  ).
drop( H, [ _ | T ] , T1 ) :-  drop( H, T, T1 ).


% Are there infinitely many "occurrences" of arg1 in arg2?
:- coinductive comember/2 .

comember( X, L ) :-  drop( X, L, L1 ),  comember( X, L1 ).


% Example queries:
?-  write( 'Query1' ),  nl,
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    member2( s( 0 ), X ),
    write( 'Yes1 !' ),  nl.

?-  write( 'Query2'),  nl,
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    member1( s( 0 ), X ),
    write( 'Yes2 !' ),  nl.

?-  write( 'Query3'),  nl,
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    comember( s( 0 ), X ),
    write( 'WHAT? SHOULD HAVE FAILED !' ),  nl.

?-  write( 'Query4'),  nl,
    X = [ 0, s( 0 ), s( s( 0 ) ) | X ],
    once( comember( s( 0 ), X ) ),
    write( 'Yes4 !' ),  nl.