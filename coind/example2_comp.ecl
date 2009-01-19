%% This is example2.clp from Luke Simon's thesis, with some tweaks.
%% "Compiled" by hand.


% A number.

num( 0, _Hyp ).

num( s( N ), Hyp ) :-
        num( N, Hyp ).


% A stream of numbers.
% :- coinductive stream/1 .

stream( S, Hyp ) :-
        member( stream( S ), Hyp ).

stream( [ H | T ], Hyp ) :-
        NHyp = [ stream( [ H | T ] ) | Hyp ],
        num( H, H, NHyp ),
        stream( T, NHyp ).



% Similar to append/3.
% :- coinductive append1/3 .

append1( L1, L2, L12, Hyp ) :-
        member( append1( L1, L2, L12), Hyp ).

append1( [], X, X, _ ).

append1( [ H | T ], Y , [ H | Z ], Hyp ) :-
        NHyp = [ append( [ H | T ], Y, [ H | Z ] ) | Hyp ],
        append1( T, Y, Z, NHyp ).


% Similar to member/2.
% :- coinductive member1/2 .

member1( E, L , Hyp ) :-
        member( member1( E, L ), Hyp ).

member1( H, [ H | _ ], _ ).

member1( X, [ H | T ], Hyp ) :-
        NHyp = [ member1( X, [ H | T ] ) | Hyp ],
        member1( X, T, NHyp ).


% Ditto, but not coinductive

member2( X, [ X | _ ], _ ).

member2( X, [ _ | T ], Hyp ) :-
        member2( X, T, Hyp ).


% Drop some "occurrence" of arg1 from arg2, yielding arg3.
% ("Occurrence" of X = something unifiable with X.)

drop( H, [ H | T ], T, _ ).

drop( H, [ _ | T ], T1, Hyp ) :-
        drop( H, T, T1, Hyp ).


% Are there infinitely many "occurrences" of arg1 in arg2?
% :- coinductive comember/2 .

comember( X, L, Hyp ) :-
        member( comember( X, L ), Hyp ).

comember( X, L, Hyp ) :-
        NHyp = [ comember( X, L ) | Hyp ],
        drop( X, L, L1, NHyp ),
        comember( X, L1, NHyp ).


% Example queries:
?-  X = [ 0, s( 0 ), s( s( 0 ) ) ],
    (
        member2( s( 0 ), X, [] ),
        write( "YES: " ),
        writeln( member2( s( 0 ), X ) ),
        fail
    ;
        writeln( "NO" )
    ).

?-  X = [ 0, s( 0 ), s( s( 0 ) ) ],
    (
        member1( s( 0 ), X, [] ),
        write( "YES: " ),
        writeln( member1( s( 0 ), X ) ),
        fail
    ;
        writeln( "NO" )
    ).


?-  X = [ 0, s( 0 ), s( s( 0 ) ) ],
    (
        comember( s( 0 ), X, [] ),
        write( "YES: " ),
        writeln( comember( s( 0 ), X ) ),
        fail
    ;
        writeln( "NO" )
    ).

?-  X = [ 0, s( 0 ), s( s( 0 ) ) | X ],
    comember( s( 0 ), X, [] ),
    write( "YES: " ),
    writeln( comember( s( 0 ), X ) ).
