
% Syntax of Processes:
% -------------------
% 
% Prefix:		P = a.Q		ProcPref(P, a, Q)
% Choice:		P = Q + R	ProcChoice(P, Q, R)
% Parallel:	P = Q | R	ProcPar(P, Q, R)
% Restriction:	P = Q \ L	ProcRest(P, Q, L)
% Relabelling:	P = Q [f]	ProcRelab(P, Q, f)
% constant Def: P = Q		def(P,Q).
% 
% 
% Auxiliary Constructs :
% --------------------
% 
% Membership:	a in L	mem(a, L)
% Relabellig fn:	b = f(a)	map(f, a, b)
% Coactions:	a.b=b.a=tau	compAct(a, b)
% 



% Semantics:
% ---------
% 

%:- table trans_rec/3.
trans_rec(P, Act_a, Q) :- 
	newdef(P, R), trans(R, Act_a, Q).

trans(code(C), nil, nil) :- call(C).
trans(in(X), in(X), nil).
trans(out(X), out(X), nil).

% Conditional
trans(cond(C, P1, P2), Act, Q) :-
	(call(C) -> trans(P1, Act, Q)
		  ; trans(P2, Act, Q)
	).


% Prefix:
trans(action(Act), Act, nil).

%% trans(pref(P1, P2), Act, Q) :-
%% 	(trans(P1, Act_b, Q1),
%% 	 Act_b == nil % if  Act is nil, then Q1 is also nil
%% 		->	trans(P2, Act, Q)
%% 		;	Act = Act_b,
%% 			((Q1 == nil) -> Q = P2 ; Q = pref(Q1, P2))
%% 	).

trans(pref(P1, P2), Act, Q) :-
	trans(P1, Act_b, Q1),
	((Act_b == nil % if  Act is nil, then Q1 is also nil
		->	trans(P2, Act, Q));
	(Act = Act_b, (( Q1 == nil -> Q = P2) ; Q = pref(Q1, P2)))).

% Choice:
trans(choice(P, Q), Act_a, R)	:- trans(P, Act_a, R) ; trans(Q, Act_a, R).
%trans(choice(_P, Q), Act_a, Q1)	:-	trans(Q, Act_a, Q1).

% Parallel:
/*
trans(par(P, Q), Act_a, par(P1, Q))	:-	trans(P, Act_a, P1).
trans(par(P, Q), Act_a, par(P, Q1))	:-	trans(Q, Act_a, Q1).
trans(par(P, Q), tau, par(P1, Q1))	:-	trans(P, Act_a, P1), 
						compAct(Act_a, Act_b),
						trans(Q, Act_b, Q1).
trans(par(P, Q), tau, par(P1, Q1))	:-	trans(P, Act_a, P1), 
						compAct(Act_b, Act_a),
						trans(Q, Act_b, Q1).
*/

/*
trans(par(P, Q), Act_a, par(P1, Q1)):-	trans_par(P,Q, Act_a, P1,Q1).


trans_par(P, Q, tau, P1, Q1) :- trans_join(P, Q, P1, Q1).
trans_par(P, Q, Act_a, P1, Q) :- trans(P, Act_a, P1).
trans_par(P, Q, Act_a, P, Q1) :- trans(Q, Act_a, Q1).

%:- table trans_join/4.
trans_join(P, Q, P1, Q1) :- 
				trans(P, Act_a, P1),
				compAct(Act_a, Act_b),
				trans(Q, Act_b, Q1).
*/

trans(par(P, Q), Act_a, par(P1, Q1)):-	trans_par(P,Q, Act_a, P1,Q1).


trans_par(P, Q, tau, P1, Q1) :- 
				trans(P, Act_a, P1),
				compAct(Act_a, Act_b),
				trans(Q, Act_b, Q1).
trans_par(P, Q, Act_a, P1, Q) :- trans(P, Act_a, P1).
trans_par(P, Q, Act_a, P, Q1) :- trans(Q, Act_a, Q1).


% axioms for comp
compAct(in(X), out(X)).
compAct(out(X), in(X)).

% Restriction:
trans(rest(P,L), Act_a, rest(P1,L)) :-	
	(nonvar(Act_a) -> notRest(Act_a, L), trans(P, Act_a, P1)
			; trans(P, Act_a, P1), notRest(Act_a, L)).


% axioms for Rest.
notRest(tau, _L) :- !.			% can't ever restrict tau's
notRest(nil, _L) :- !.			% .. or nil.
notRest(in(V), L) :- !,
	\+ mem(L, V).
notRest(out(V), L) :- !,
	\+ mem(L, V).
notRest(Act_a, L) :- atomic(Act_a), \+ mem(L, Act_a).

% Relabelling:
trans(relab(P, F), Act_b, relab(P1, F))	:-
	(nonvar(Act_b) -> map(F, Act_a, Act_b), trans(P, Act_a, P1)
			; trans(P, Act_a, P1), map(F, Act_a, Act_b)).
						
						

% axioms for map
map(F, out(V1), out(V2)) :- !,
         defmap(F, V1, V2).
map(F, in(V1), in(V2)) :- !,
         defmap(F, V1, V2).
map(F, X, Y) :- defmap(F, X, Y).


%map(F, out(Chan1(X)), out(Chan2(X))) :-	map(F, Chan1, Chan2).

% recursion:
trans(proc(P), Act_a, Q) :- trans_rec(P, Act_a, Q).

% dummy stub for dom
dom(_,_).

