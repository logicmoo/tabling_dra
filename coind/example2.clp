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
?-  writeln( "Query1" ),
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    member2( s( 0 ), X ),
    writeln( "Yes1 !" ).

?-  writeln( "Query2"),
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    member1( s( 0 ), X ),
    writeln( "Yes2 !" ).

?-  writeln( "Query3"),
    X = [ 0, s( 0 ), s( s( 0 ) ) ],
    comember( s( 0 ), X ),
    writeln( "Yes3 !" ).

?-  writeln( "Query4"),
    X = [ 0, s( 0 ), s( s( 0 ) ) | X ],
    once comember( s( 0 ), X ),
    writeln( "Yes4 !" ).
