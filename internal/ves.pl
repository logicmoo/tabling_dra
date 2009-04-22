

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
%%%  A vanilla interpreter for Prolog, with explicit state.                  %%%
%%%  Written by Feliks Kluzniak at UTD (April 2009).                         %%%
%%%                                                                          %%%
%%%  Last update: 22 April 2009.                                             %%%
%%%                                                                          %%%

%%% This interpreter maintains an explicit stack of all calls that have not been
%%% bactracked from and an explicit resolvent.  The purpose is to make it a good
%%% starting point for experiments with various control regimes.
%%%
%%% The stack is a list of goals with associated information (to be described
%%% below).
%%%
%%% The resolvent is a list of pairs (formed with pair/2).
%%% The first element of a pair is a goal with associated information (see
%%% below), the second is a list of ancestors (of that goal): the list is
%%% ordered, and the immediate ancestor is the first item on the list.  Data
%%% about each ancestor is, again, a goal with associated information.
%%%
%%% A "goal with associated information" is a pair (formed with goal/2).
%%% The first element of the pair is the goal, the second is the list of clauses
%%% that have not been completed for the goal (the clauses are in a transformed
%%% form, described below).
%%%
%%% The clauses of a program are transformed ("pre-compiled") so that the body
%%% of each clause is a d-list of items that can easily be pre-pended to the
%%% beginning of a resolvent.  When the head of the clause is matched against
%%% the goal and the current list of ancestors, the new fragment of the
%%% resolvent gets instantiated to its desired form.  Moreover, the clauses of a
%%% procedure are packed into lists, so that it is easy to pick up the
%%% "associated information" about a goal.
%%%
%%% NOTE: These data structures are cyclic terms!
%%%
%%% The details are best explained by an example.  Consider the following
%%% procedure:
%%%
%%%     p( a ) :-  b( a ).
%%%     p( X ) :-  q( X, Y ),  r( Y ).
%%%
%%% This is transformed, and stored in the following form:
%%%
%%%     procedure( pair( p( H ), Anc ), Proc ) :-
%%%         NAnc = [ goal( p( H ), L ) | Anc ]
%%%         Proc = [ match( H, a ) : ( [ pair( b( a ), NAnc ) | E1 ] - E1),
%%%                  match( H, X ) :
%%%                      ( [ pair( q( X, Y ), NAnc ),
%%%                          pair( r( Y ),    NAnc ] ) | E2 ] - E2)
%%%                ].
%%%
%%% The procedure is picked up by invoking
%%%
%%%     procedure( pair( p( Argument ), AncestorList ), Proc )
%%%
%%% This unifies H with Argument and Anc with AncestorList.  The instantiation
%%% of Proc will be the list of modified clauses, with the correct list of
%%% ancestors already in the second element of each pair in their bodies.  As a
%%% clause is activated, the "match" is invoked to take care of unification
%%% with the head ("match" just unifies its arguments).  If the match succeeds,
%%% the representation of the body (by now correctly instantiated) is prepended
%%% to the current resolvent: the cost of this is O(1), since the body is a
%%% difference list.
%%%
%%%
%%% CURRENT LIMITATIONS:
%%%   - There is no support for:
%%%       !/0,  ;/2, ->/2 .




:- ensure_loaded( '../general/top_level.pl' ).
:- ensure_loaded( '../general/compatibility_utilities' ).


%%%%% Interface with the top level.  %%%%

default_extension( '.pl' ).                      % default file name extension


%Built-in predicates:

builtin( Goal ) :-  is_builtin( Goal ), Goal != '!'.

initialise.

program_loaded.

query( _ ).



%%%%% End of interface with the top level.  %%%%
