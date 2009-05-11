   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%                                                                          %%%
%%%  The program transformatio phase for dra2.                               %%%
%%%  Written by Feliks Kluzniak at UTD (April-May 2009).                     %%%
%%%                                                                          %%%
%%%  Last update: 11 May 2009.                                               %%%




%------- Transformation -------

%% transform:
%% Transform the program clauses and repack them into rules, as described in the
%% comment at the top of this file.

transform :-
        check_no_support,
        findall( Proc, defined( Proc ), Procs ),
        transform_procedures( Procs, 0 ),
        show.
%
show :- index(A,B,C),  writeln(index(A,B,C)), fail.
show :- rule(A,B,C), writeln(rule(A,B,C)), fail.
show.


%% check_no_support:
%% Produce a fatal error if "support" has been used in this program.

check_no_support :-
        support( _ ),
        error( 'Programs with "support" cannot be executed by this version' ).

check_no_support.


%% transform_procedures( + list of procedure patterns, + number of next rule ):
%% Transform the clauses of these procedures.

transform_procedures( [], _ ).

transform_procedures( [ Proc | Procs ], FreeRuleNumber ) :-
        findall( (Proc :- Body)
               , clause_in_module( interpreted, Proc, Body )
               , Clauses
               ),
        transform_clauses( Clauses, FreeRuleNumber, NextFreeRuleNumber ),
        assertz( index( Proc, FreeRuleNumber, NextFreeRuleNumber ) ),
        transform_procedures( Procs, NextFreeRuleNumber ).


%% transform_clauses( + list of original clauses,
%%                    + number of next rule,
%%                    - number of next rule after
%%                  ):
%% Transform the clauses of a procedure, storing them in rules/2.

transform_clauses( [], FreeRuleNumber, FreeRuleNumber ).

transform_clauses( [ Clause | Clauses ], FreeRuleNumber, NextFreeRuleNumber ) :-
        transform_clause( Clause, FreeRuleNumber, NewFreeRuleNumber ),
        transform_clauses( Clauses, NewFreeRuleNumber, NextFreeRuleNumber ).

%
transform_clause( (Head :- Body), FreeRuleNumber, NewFreeRuleNumber ) :-
        transform_body( Body, 1, NewBody, _ ),
        assertz( rule( FreeRuleNumber, Head, NewBody ) ),
        NewFreeRuleNumber is FreeRuleNumber + 1.


%% transform_body( + body,              + next free goal number,
%%                  - transformed body, - number of next free goal after
%%               ):

transform_body( \+ A, N, \+ NA, NN ) :-
        !,
        transform_body( A, N, NA, NN ).

transform_body( once( A ), N, once( NA ), NN ) :-
        !,
        transform_body( A, N, NA, NN ).

transform_body( (A -> B ; C), N, NewBody, NN ) :-
        !,
        transform_body( A, N , NA, N2 ),
        transform_body( B, N2, NB, N3 ),
        transform_body( C, N3, NC, NN ),
        NewBody = (NA -> NB ; NC).

transform_body( (A ; B), N, NewBody, NN ) :-
        !,
        transform_body( A, N , NA, N2 ),
        transform_body( B, N2, NB, NN ),
        NewBody = (NA ; NB).

transform_body( (A , B), N, NewBody, NN ) :-
        !,
        transform_body( A, N , NA, N2 ),
        transform_body( B, N2, NB, NN ),
        NewBody = (NA , NB).

transform_body( (A -> B), N, NewBody, NN ) :-
        !,
        transform_body( A, N , NA, N2 ),
        transform_body( B, N2, NB, NN ),
        NewBody = (NA -> NB).

transform_body( Call, N, goal( N, Call ), NN ) :-
        NN is N + 1.

%------- End of transformation -------
