%%%  Output cyclic terms as equations.                                       %%%
%%%                                                                          %%%
%%%  Written by Ronald de Haan at UT Dresden (January, April 20011).         %%%
%%%                                                                          %%%
%%%  Reformatted by FK.  Some modifications by FK.                           %%%
%%%                                                                          %%%


:- module( output_equation,
	  [
	    cyclic/2,
	    get_term_equation/3,
	    get_printable_term_equation/3
	  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Output cyclic terms as equations                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                            %%%
%%% Example of use:                                            %%%
%%%                                                            %%%
%%% ?- X = f(Y), Y = g(Y), get_term_equation(X,Eq,InitVar).    %%%
%%% X = f(g(**)),                                              %%%
%%% Y = g(**),                                                 %%%
%%% Eq = [InitVar=f(_G805), _G805=g(_G805)] .                  %%%
%%%                                                            %%%
%%%                                                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%------------------------------------------------------------------------------
%% cyclic( + Term, + Max ) :
%% Succeeds iff the term Term is cyclic within a depth of Max.

cyclic( Term, Max ) :-
        cyclic_term( Term ),  % if the term is not cyclic at all, don't even try
        list_subterms_up_to_depth( Term, Max, Subterms ),
        check_list_for_duplicates( Subterms ).


%% list_subterms_up_to_depth( + Term, + MaxDepth, - Subterms ) :
%% Produces a list of all subterms of Term upto the given depth.

%% The following is the original version:
%
% list_subterms_up_to_depth( Term, MaxDepth, Subterms ) :-
%         list_subterms_up_to_depth( [ (0 , Term) ], MaxDepth, [], Subterms ).
%
%
% %% list_subterms_up_to_depth( + Terms, + MaxDepth, + Acc, - Answer ) :
% %%   - Terms is the list of pairs of numbers/terms to be handled,
% %%     where each number is the depth of the accompanying subterm within the
% %%     original term;
% %%   - MaxDepth is the depth up to which subterms are to be listed;
% %%   - Acc is an accumulator;
% %%   - Answer returns the list of subterms.
%
% list_subterms_up_to_depth( [], _, Acc, Acc ).
%
% list_subterms_up_to_depth( [ (CurDepth , _) | Terms ], MaxDepth, Acc, Ans ) :-
%         CurDepth > MaxDepth,
%         !,
%         list_subterms_up_to_depth( Terms, MaxDepth, Acc, Ans ).
%
% list_subterms_up_to_depth( [ (_ , CurTerm) | Terms ], MaxDepth, Acc, Ans ) :-
%         var( CurTerm ),
%         !,
%         list_subterms_up_to_depth( Terms, MaxDepth, Acc, Ans ).
%
% list_subterms_up_to_depth( [ (CurDepth , CurTerm) | Terms ],
%                            MaxDepth, Acc, Ans
%                          ) :-
%         CurTerm =.. [ _ | Args ],
%         NewCurDepth is CurDepth + 1,
%         number_with( NewCurDepth, Args, NumberedArgs ),
%         append( Terms, NumberedArgs, NewTerms ),
%         list_subterms_up_to_depth( NewTerms, MaxDepth, [ CurTerm | Acc ],
%                                    Ans
%                                   ).
%
% I have rewritten it to the following form, which is not always tail-recursive,
% but does note use append/3 to extend the list of (depth , term) pairs that are
% to be processed.  We probably have enough memory for the execution stack,
% but the use of append may slow the program down in extreme cases.

list_subterms_up_to_depth( Term, MaxDepth, Subterms ) :-
        aux_subterms_up_to_depth( [ (0 , Term) ], MaxDepth, Subterms, [] ).


% aux_subterms_up_to_depth( + Pairs, + MaxDepth, - Subterms, - End ) :
%    - Pairs is a list of (depth , subterm) pairs: the first item is the depth
%      of this subterm in the original term;
%    - MaxDepth is the maximum depth of interesting terms;
%    - Subterms is an _open_ list of interesting subterms;
%    - End is the end of the open list.
% This is an auxiliary of list_subterms_up_to_depth/3.

aux_subterms_up_to_depth( [], _MaxDepth, End, End ).

aux_subterms_up_to_depth( [ Pair | Pairs ], MaxDepth, Subterms, End ) :-
        Pair = (CurDepth , CurTerm),
        (
            CurDepth > MaxDepth
        ->                                         % CurTerm is too deep
            Subterms = RestOfSubterms
        ;
            \+ compound( CurTerm )
        ->                                         % CurTerm OK, has no subterms
            Subterms = [ CurTerm | RestOfSubterms ]
        ;
                                                   % CurTerm OK, do its subterms
            Subterms = [ CurTerm | ArgSubterms ],
            CurTerm =.. [ _ | Args ],
            NewCurDepth is CurDepth + 1,
            number_with( NewCurDepth, Args, NumberedArgs ),
            aux_subterms_up_to_depth( NumberedArgs, MaxDepth,
                                      ArgSubterms,  RestOfSubterms
                                    )
        ),
        aux_subterms_up_to_depth( Pairs, MaxDepth, RestOfSubterms, End ).


% Convert the list in arg2 to arg3, by pairing each element with arg1.

number_with( _, [], [] ).

number_with( N, [ H | T ], [ (N , H) | Ans ] ) :-
        number_with( N, T, Ans ).


%% check_list_for_duplicates( + List ) :
%% Checks whether the list contains duplicates, i.e., at least two identical
%% terms ("identical" as opposed to "unifiable").
%
% Ronald's original version was most elegant:
%
%    check_list_for_duplicates( List ) :-
%            setof( X, member( X, List ), Set ),
%            length( List, N ),
%            length( Set, M ),
%            N \= M.
%
% I have replaced it with the following in the interest of "efficiency": if
% a duplicate is found early, there is no need to go through the entire list.
% The worst-case cost should be about the same, i.e., quadratic in the length of
% the list (in the original version this is hidden within setof/3).

check_list_for_duplicates( List ) :-
        append( _Prefix, [ Term | Postfix ], List ),
        identical_member( Term, Postfix ),
        !.



%%------------------------------------------------------------------------------
%% get_printable_term_equation( Term, Head, List ) :
%% Returns the equation of a term as a list of strings.
%% Head is a string containing the initial variable of the equation.
%% List is a list of strings containing parts of the equation.

get_printable_term_equation( Term, Head, List ) :-
        get_term_equation( Term, Eq, H ),
        swritef( Head,  '%w\n',  [ H ] ),
        get_printable_list( Eq, List ).


% Convert a list of equations (arg1) to a list of their string forms.

get_printable_list( [], [] ).

get_printable_list( [ ( A=B ) | T ], [ String | Rest ] ) :-
        swritef( String,  '%w = %w\n',  [ A, B ] ),
        get_printable_list( T, Rest ).


%% get_term_equation( Term,  EquationList,  HeadVar ) obtains a list of
%% equations EquationList corresponding to the cyclic term Term,  in which
%% HeadVar is the variable corresponding to Term.

get_term_equation( Term, EquationList, HeadVar ) :-
        get_equation( Term, Equation ),
        clean_equation( Equation, CleanEquation ),
        get_equation_with_variables( CleanEquation, EquationList, HeadVar ).



%%------------------------------------------------------------------------------
%% get_equation( Term,  Equation )  gets the equation corresponding to a term in
%% the form of a list of equalities in which the cyclic points are marked with
%% x/1 markers
%%
%% example:
%% ?-  X = [ a | Y ],  Y = [ b | Y ],  get_equation( X, E ).
%% X = [ a,  b | ** ],
%% Y = [ b | ** ],
%% E = [ (0 , [ a | x( 1 ) ]), (1 , [ b | x( 1 ) ]) ].

get_equation( Term, Equation ) :-
        obtain_all_cyclic_subterms( Term, List ),
        number_list_starting_at( List, 1, NumberedList ),
        replace_loop( [ (0 , Term) ],  NumberedList, [],
                      EquationWithDuplicates
                    ),
        setof( X,  member( X, EquationWithDuplicates ),  Equation ).


%% obtain_all_cyclic_subterms( Term,  List )
%% Create a list of all cyclic subterms of this term.  Each element of the list
%% is different from the others.

obtain_all_cyclic_subterms( Term, List ) :-
        obtain_all_cyclic_subterms( [ Term ], [ Term ], 0, [], List ).


%% obtain_all_cyclic_subterms( Terms, SeenBefore, N, Acc, Ans )
%%  - Terms are the terms that still have to be handled;
%%  - SeenBefore is the list of terms that have already been seen;
%%  - N is a counter used to keep track of whether we're at the root of the term
%%    or not;
%%  - Acc is an accumulator;
%%  - Ans will contain the list of different subterms.

%%%  ???????????  FK ????????????

obtain_all_cyclic_subterms( [], _, _, Acc, Acc ) :- !.

obtain_all_cyclic_subterms( [ T | TS ], SeenBefore, N, Acc, List ) :-
        identical_member( T, SeenBefore ),
        N \= 0,  % makes sure the root of the whole term is handled as an
                 % exception
        !,
        obtain_all_cyclic_subterms( TS, SeenBefore, N, [ T | Acc ], List ).

obtain_all_cyclic_subterms( [ T | TS ], SeenBefore, N, Acc, List ) :-
        % Remember only cyclic subterms:
        (
            cyclic_term( T )
        ->
            NewSeenBefore = [ T | SeenBefore ]
        ;
            NewSeenBefore = SeenBefore
         ),
        (
            compound( T )
        ->
            T =.. [ _ | SubtermList ],
            append( TS, SubtermList, NTS )
        ;
            NTS = TS
        ),
        M is N + 1,
        obtain_all_cyclic_subterms( NTS, NewSeenBefore, M, Acc, List ).


%% number_list_starting_at( List, InitialNr, NumberedList )
%%  - List is the list to be numbered;
%%  - InitialNr is the number to start numbering with;
%%  - NumberedList is the result of numbering elements of List from InitialNr
%%    on.

number_list_starting_at( [], _, [] ).

number_list_starting_at( [ H | T ], N, [ (N , H) | A ] ) :-
        M is N + 1,
        number_list_starting_at( T, M, A ).


%% replace_loop( Agenda,  SubtermList,  DoneBefore,  Results )
%% Replaces all subterms at cyclic positions with x/1 markers
%%  - Agenda is a list of pairs of numbers and terms that still have to be
%%    handled
%%  - SubtermList

replace_loop( [],  _,  _,  [] ).

replace_loop( [ (I , Term) | RestAgenda ],  SubtermList,  DoneBefore,
              [ (I , Result1) | Results ]
            ) :-
        replace_term_proper( Term,  SubtermList,  NewAgenda1,  Result1 ),
        findall( ( N, AgendaItem ),
                ( member( (N , AgendaItem), NewAgenda1 ),
                 \+ member( AgendaItem, DoneBefore ) ),
                RealNewAgenda
                ),
        append( RestAgenda, RealNewAgenda, NewAgenda ),
        replace_loop( NewAgenda,  SubtermList,  [ Term | DoneBefore ],
                      Results
                    ).


%% replace_term( Term,  SubtermList,  NewAgenda,  Result )
%% Replaces all subterms of a term with cycle markers x/1.
%% Also returns all replaced subterms in NewAgenda.

replace_term( Term,  SubtermList,  [ (N , Term) ],  x( N ) ) :-
        member( (N , Term), SubtermList ),
        !.

replace_term( Term,  SubtermList,  NewAgenda,  Result ) :-
        replace_term_proper( Term,  SubtermList,  NewAgenda,  Result ).


%% replace_term_proper( Term,  SubtermList,  NewAgenda,  Result )
%% Acts like replace_term/4 but skips any replacements in the root of the term.

replace_term_proper( Term,  SubtermList,  NewAgenda,  Result ) :-
        Term =.. [ H | T ],
        replace_term_list( T,  SubtermList,  NewAgenda,  Results ),
        Result =.. [ H | Results ].


%% replace_term_list( Terms,  SubtermList,  NewAgenda,  Results )
%% Straightforwardly extends replace_term/4 to act on lists of terms instead of
%% on single terms.

replace_term_list( [],  _,  [],  [] ).

replace_term_list( [ Term | List ],  SubtermList,  NewAgenda,  Results ) :-
        replace_term( Term,  SubtermList,  NewAgenda1,  Result1 ),
        replace_term_list( List,  SubtermList,  NewAgenda2,  Results2 ),
        append( NewAgenda1,  NewAgenda2,  NewAgenda ),
        Results = [ Result1 | Results2 ].


%%------------------------------------------------------------------------------
%% get_equation_with_variables( Equation,  EquationList,  HeadVar )
%% turns an equation with x/1 markers into an equation with variables for the
%% cyclic points
%%
%% example:
%% ?- X = [ a | Y ],  Y = [ b | Y ],  get_equation( X, E ),
%%    get_equation_with_variables( E, EL, HV ).
%% X = [ a,  b | ** ],
%% Y = [ b | ** ],
%% E = [  ( 0,  [ a | x( 1 ) ] ),  ( 1,  [ b | x( 1 ) ] ) ],
%% EL = [ HV=[ a | _G930 ],  _G930=[ b | _G930 ] ] .

get_equation_with_variables( Equation, EquationList, HeadVar ) :-
        variable_list( Equation, VarList ),
        member( ( 0, HeadVar ), VarList ),
        loop_through_list( Equation, VarList, EquationList ).

% loop_through_list( Equation,  VarList,  EquationList )
loop_through_list( [], _, [] ).
loop_through_list( [ ( N, T ) | Rest ], VarList, [ Eq | RestAns ] ) :-
        replace_marker_by_variable( T, VarList, L ),
        member( ( N, V ), VarList ),
        Eq = ( V=L ),
        loop_through_list( Rest, VarList, RestAns ).

%% variable_list( Equation,  variable_list )
%% gets a list of numbered variables for every term in the list of equations
variable_list( [], [] ).
variable_list( [ ( N, _ ) | T ], [ ( N, _ ) | R ] ) :-
        variable_list( T, R ).

%% replace_marker_by_variable( Term,  VarList,  Loop )
%% replaces cyclic positions,  marked with x/1,  with corresponding variable
%% from a numbered list of variables
replace_marker_by_variable( x( N ), VL, S ) :- !,
        member( ( N, S ), VL ).
replace_marker_by_variable( T, VL, S ) :-
        T =.. [ H | R ],
        replace_marker_by_variable_list( R, VL, SS ),
        S =.. [ H | SS ].

% replace_marker_by_variable_list( +Terms,  +VarList,  -Loops )
replace_marker_by_variable_list( [], _, [] ).
replace_marker_by_variable_list( [ H | T ], VL, [ S | SS ] ) :-
        replace_marker_by_variable( H, VL, S ),
        replace_marker_by_variable_list( T, VL, SS ).

%%------------------------------------------------------------------------------
%% the following predicates are used to remove redundancies from the equation
%%
%% the following exemplifies what kind of redundancies can occur:
%% ?- X = [ a | X ],  get_equation( X, E ),
%%   get_equation_with_variables( E, EL, HV ).
%% X = [ a | ** ],
%% E = [  ( 0,  [ a | x( 1 ) ] ),  ( 1,  [ a | x( 1 ) ] ) ],
%% EL = [ HV=[ a | _G735 ],  _G735=[ a | _G735 ] ] .
%%
%% ?- X = [ a | X ],  get_equation( X, E ),  clean_equation( E, F ),
%%   get_equation_with_variables( F, EL, HV ).
%% X = [ a | ** ],
%% E = [  ( 0,  [ a | x( 1 ) ] ),  ( 1,  [ a | x( 1 ) ] ) ],
%% F = [  ( 0,  [ a | x( 0 ) ] ) ],
%% EL = [ HV=[ a | HV ] ] .

%% clean_equation( +Equation,  -Result )
%% checks if there is a redundancy,  and if so throws it out
clean_equation( Equation,  Result ) :-
        member( ( 0, Init ), Equation ),
        find_duplicate_eq( Equation, Init, N ),
        !,
        doclean_equation( Equation, N, Result ).
clean_equation( Equation, Equation ).

%% doclean_equation( Equation,  SuperfluousVar,  Result )
%% actually throws out the redundancy
doclean_equation( [], _, [] ).
doclean_equation( [ ( N, _ ) | Rest ], N, Ans ) :-
        !,
        doclean_equation( Rest, N, Ans ).
doclean_equation( [ ( N, Term ) | Rest ], M, [ ( N, NewTerm ) | Ans ] ) :-
        replace_markers_with_initial( Term, M, NewTerm ),
        doclean_equation( Rest, M, Ans ).

%% replace_markers_with_initial( Term,  SuperfluousVar,  Result )
%% replaces markers of the superfluous variable with markers for the initial
%% variable
replace_markers_with_initial( x( N ), N, x( 0 ) ) :- !.
replace_markers_with_initial( x( K ), N, x( K ) ) :- K \= N,  !.
replace_markers_with_initial( Term, N, Result ) :-
        Term =.. [ H | TS ],
        replace_markers_with_initial_list( TS, N, RS ),
        Result =.. [ H | RS ].

%% replace_markers_with_initial_list( Terms,  SuperfluousVar,  Results )
%% straightforwardly extends replace_markers_with_initial/3 to act on lists
replace_markers_with_initial_list( [], _, [] ).
replace_markers_with_initial_list( [ H | T ], N, [ R | RS ] ) :-
        replace_markers_with_initial( H, N, R ),
        replace_markers_with_initial_list( T, N, RS ).

%% find_duplicate_eq( Equation,  InitialTerm,  SuperfluousVar )
%% throws out duplicate information
find_duplicate_eq( [ ( 0, Init ) | T ], Init, M ) :- !,
        find_duplicate_eq( T, Init, M ).
find_duplicate_eq( [ ( M, Init ) | _ ], Init, M ) :- !.
find_duplicate_eq( [ _ | T ], Init, M ) :-
        find_duplicate_eq( T, Init, M ).


%%------------------------------------------------------------------------------
%% identical_member( + term, + list ):
%% Succeed if the list contains this term (as opposed to something that is
%% unifiable with this term).

identical_member( X, [ Item | Items ] ) :-
        (
            X == Item
        ->
            true
        ;
            identical_member( X, Items )
        ).

%%==============================================================================
