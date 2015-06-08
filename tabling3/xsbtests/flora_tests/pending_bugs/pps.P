

:- table fd/3.
:- table mvd/3.
:- table ifd/3.
:- table imvd/3.
:- table isa/2.
:- table sub/2.
:- table fs/3.
:- table mvs/3.

:- table fd/3.
:- table mvd/3.
:- table ifd/3.
:- table imvd/3.

:- table derived_fd/3.
:- table derived_mvd/3.
:- table derived_ifd/3.
:- table derived_imvd/3.
:- table defined_fd/2.
:- table defined_mvd/2.
:- table defined_ifd/2.
:- table defined_imvd/2.

:- table overwritten_fd/3.
:- table overwritten_mvd/3.
:- table overwritten_ifd/3.
:- table overwritten_imvd/3.
:- table conflict_fd/3.
:- table conflict_ifd/3.
:- table conflict_mvd/3.
:- table conflict_imvd/3.


:- import member/2, ground/1 from basics.
:- import sk_not/1 from tables.


tnot_all(Test) :- sk_not(Test).


isa(0,stage1).


base_mvd(pps,'flora_@'(produces,0),golfC).

base_mvd(pps,'flora_@'(produces,0),golfCL).

base_mvd(pps,'flora_@'(produces,0),passatC).

base_mvd(pps,'flora_@'(produces,0),passatCL).

base_mvd(pps,'flora_@'(produces,0),motor14).

base_mvd(pps,'flora_@'(produces,0),motor18).

base_mvd(pps,'flora_@'(produces,0),wheel145).

base_mvd(pps,'flora_@'(produces,0),part1).

base_mvd(pps,'flora_@'(produces,0),part2).

base_mvd(pps,'flora_@'(produces,0),part3).


%%% golfC[hasConfig@(0)->newConfig(golfC,0)].
base_fd(golfC,'flora_@'(hasConfig,0),newConfig(golfC,0)).


%%% golfCL[hasConfig@(0)->newConfig(golfCL,0)].
base_fd(golfCL,'flora_@'(hasConfig,0),newConfig(golfCL,0)).


%%% passatC[hasConfig@(0)->newConfig(passatC,0)].
base_fd(passatC,'flora_@'(hasConfig,0),newConfig(passatC,0)).


%%% passatCL[hasConfig@(0)->newConfig(passatCL,0)].
base_fd(passatCL,'flora_@'(hasConfig,0),newConfig(passatCL,0)).


%%% motor14[hasConfig@(0)->newConfig(motor14,0)].
base_fd(motor14,'flora_@'(hasConfig,0),newConfig(motor14,0)).


%%% newConfig(golfC,0)[needsPart->>{motor14,wheel145}].
base_mvd(newConfig(golfC,0),'flora_@'(needsPart),motor14).

base_mvd(newConfig(golfC,0),'flora_@'(needsPart),wheel145).


%%% newConfig(golfCL,0)[needsPart->>{motor18,wheel145}].
base_mvd(newConfig(golfCL,0),'flora_@'(needsPart),motor18).

base_mvd(newConfig(golfCL,0),'flora_@'(needsPart),wheel145).


%%% newConfig(passatC,0)[needsPart->>{motor14,wheel145}].
base_mvd(newConfig(passatC,0),'flora_@'(needsPart),motor14).

base_mvd(newConfig(passatC,0),'flora_@'(needsPart),wheel145).


%%% newConfig(passatCL,0)[needsPart->>{motor18,wheel145}].
base_mvd(newConfig(passatCL,0),'flora_@'(needsPart),motor18).

base_mvd(newConfig(passatCL,0),'flora_@'(needsPart),wheel145).


%%% newConfig(motor14,0)[needsPart->>{part1,part2,part3}].
base_mvd(newConfig(motor14,0),'flora_@'(needsPart),part1).

base_mvd(newConfig(motor14,0),'flora_@'(needsPart),part2).

base_mvd(newConfig(motor14,0),'flora_@'(needsPart),part3).


%%%  removeFrom(passatC,motor14,0).
removeFrom(passatC,motor14,0).


%%% addTo(passatC,motor18,0).
addTo(passatC,motor18,0).


%%% start(wheel155,0).
start(wheel155,0).


%%% 0.ready_user[].
base_fd(0,'flora_@'(ready_user),flora_objref_fd(0,'flora_@'(ready_user))).


%%% stop(part1,1).
stop(part1,1).


%%% removeFrom(golfCL,wheel145,1).
removeFrom(golfCL,wheel145,1).


%%% addTo(golfCL,wheel155,1).
addTo(golfCL,wheel155,1).


%%% 1.ready_user[].
base_fd(1,'flora_@'(ready_user),flora_objref_fd(1,'flora_@'(ready_user))).


%%%  :- table change_meth/3.
:- table
        change_meth/3.

derived_fd(X,Y,Z) :- base_fd(X,Y,Z).
derived_ifd(X,Y,Z) :- base_ifd(X,Y,Z).
derived_mvd(X,Y,Z) :- base_mvd(X,Y,Z).
derived_imvd(X,Y,Z) :- base_imvd(X,Y,Z).

fd(X,Y,Z) :- derived_fd(X,Y,Z).
ifd(X,Y,Z) :- derived_ifd(X,Y,Z).
mvd(X,Y,Z) :- derived_mvd(X,Y,Z).
imvd(X,Y,Z) :- derived_imvd(X,Y,Z).

defined_fd(X,Y) :- derived_fd(X,Y,_).
defined_ifd(X,Y) :- derived_ifd(X,Y,_).
defined_mvd(X,Y) :- derived_mvd(X,Y,_).
defined_imvd(X,Y) :- derived_imvd(X,Y,_).

%%% change_meth(O,S,M) :- O.change@(S,M)[].
change_meth(_h10661,_h10675,_h10689) :- 
        fd_rhs(_h10661,'flora_@'(change,_h10675,_h10689),_h10794).


derived_fd(_h11836,'flora_@'(_h11854,_h11870),_h11894) :- 
        isa_rhs(_h11870,stage1),
        isa_rhs(apply(_h11836,_h11854),edbscalar),
        fd_rhs(_h11836,'flora_@'(_h11854,_h12034),_h11894),
        is(_h12298,+(_h12034,1)),
        =(_h11870,_h12298),
        tnot(change_meth(_h11836,_h12034,_h11854)).


derived_fd(_h13348,'flora_@'(_h13366,_h13382),_h13406) :- 
        isa_rhs(_h13382,stage1),
        isa_rhs(apply(_h13348,_h13366),edbscalar),
        fd_rhs(_h13348,'flora_@'(change,_h13542,_h13366),_h13406),
        is(_h13761,+(_h13542,1)),
        =(_h13382,_h13761).


derived_mvd(O,'flora_@'(M,T),Q) :- 
        isa_rhs(T,stage1),
        isa_rhs(apply(O,M),edbmultivalued),
        mvd_rhs(O,'flora_@'(M,S),Q),
        is(T1,+(S,1)),
        =(T,T1),
        tnot(delete(O,M,Q,S)).
	%%not_deleted(O,M,Q,S).

:- table not_deleted/4.
not_deleted(O,M,Q,S) :- tnot(delete(O,M,Q,S)).
 ?- derived_mvd(pps,'flora_@'(produces,2),passatC).
 ?- (derived_mvd(pps,'flora_@'(produces,2),passatC); get_residual(derived_mvd(pps,'flora_@'(produces,2),passatC),[])).
 ?- (derived_mvd(O,'flora_@'(M,S),Q), fail; get_residual(derived_mvd(O,'flora_@'(M,S),Q),[])), writeln(get_residual(derived_mvd(O,'flora_@'(M,S),Q),[])), fail.
 ?- mvd_rhs(pps,'flora_@'(produces,1),passatC).
 ?- tnot(delete(pps,produces,passatC,1)).
 ?- not_deleted(pps,produces,passatC,1).
 ?- (not_deleted(pps,produces,passatC,1); get_residual(not_deleted(pps,produces,passatC,1))).
 ?- (delete(O,M,Q,S), fail; get_residual(delete(O,M,Q,S),[]), writeln(delete(O,M,Q,S)),fail).
 ?- (delete(pps,produces,passatC,1), fail; get_residual(delete(pps,produces,passatC,1),[]), writeln(delete(pps,produces,passatC,1)),fail).


derived_mvd(_h16393,'flora_@'(_h16411,_h16427),_h16449) :- 
        isa_rhs(_h16427,stage1),
        isa_rhs(apply(_h16393,_h16411),edbmultivalued),
        insert(_h16393,_h16411,_h16449,_h16603),
        is(_h16776,+(_h16603,1)),
        =(_h16427,_h16776).


%%%  apply(pps,produces):edbmultivalued.
isa(apply(pps,produces),edbmultivalued).


%%% apply(P,hasConfig):edbscalar :- pps[produces@(S)->>P], S:state.
isa(apply(_h17933,hasConfig),edbscalar) :- 
        mvd_rhs(pps,'flora_@'(produces,_h17999),_h17933),
        isa_rhs(_h17999,state).


%%% insert(pps,produces,P,S) :- start(P,S), S:stage4.
insert(pps,produces,_h18702,_h18716) :- 
        start(_h18702,_h18716),
        isa_rhs(_h18716,stage4).


%%% :- table delete/4.
:- table
        delete/4.


%%% delete(pps,produces,P,S) :- stop(P,S), S:stage4.
:- table delete/4.
delete(pps,produces,_h19605,_h19619) :- 
        stop(_h19605,_h19619),
        isa_rhs(_h19619,stage4).


%%% stop(P,S) :- P.hasConfig@(S)[needsPart->>Q], stop(Q,S), S:stage4.
:- table stop/2.
stop(_h20420,_h20434) :- 
        fd_rhs(_h20420,'flora_@'(hasConfig,_h20434),_h20617),
        mvd_rhs(_h20617,'flora_@'(needsPart),_h20525),
        stop(_h20525,_h20434),
        isa_rhs(_h20434,stage4).


%%% change(O,S,hasConfig) :- addTo(O,P,S), S:stage4.
change(_h21286,_h21300,hasConfig) :- 
        addTo(_h21286,_h21353,_h21300),
        isa_rhs(_h21300,stage4).


%%% change(O,S,hasConfig) :- removeFrom(O,P,S), S:stage4.
change(_h22011,_h22025,hasConfig) :- 
        removeFrom(_h22011,_h22078,_h22025),
        isa_rhs(_h22025,stage4).


derived_fd(_h23008,'flora_@'(change,_h23036,hasConfig),newConfig(_h23008,_h23089)) :- 
        change(_h23008,_h23036,hasConfig),
        is(_h23313,+(_h23036,1)),
        =(_h23089,_h23313),
        isa_rhs(_h23036,stage4).


derived_mvd(newConfig(_h24592,_h24606),'flora_@'(needsPart),_h24640) :- 
        change(_h24592,_h24685,hasConfig),
        is(_h24996,+(_h24685,1)),
        =(_h24606,_h24996),
        fd_rhs(_h24592,'flora_@'(hasConfig,_h24685),_h25020),
        mvd_rhs(_h25020,'flora_@'(needsPart),_h24640),
        not(removeFrom(_h24592,_h24640,_h24685)),
        isa_rhs(_h24685,stage4).


derived_mvd(newConfig(_h26136,_h26150),'flora_@'(needsPart),_h26184) :- 
        change(_h26136,_h26229,hasConfig),
        is(_h26453,+(_h26229,1)),
        =(_h26150,_h26453),
        addTo(_h26136,_h26184,_h26229),
        isa_rhs(_h26229,stage4).


%%%  stage1::state[ready_edb *-> true].
sub(stage1,state).

base_ifd(stage1,'flora_@'(ready_edb),true).


%%% stage2::state[ready_idb *-> true].
sub(stage2,state).

base_ifd(stage2,'flora_@'(ready_idb),true).


%%% stage3::state.
sub(stage3,state).


%%% stage4::state[ready_changes *-> true].
sub(stage4,state).

base_ifd(stage4,'flora_@'(ready_changes),true).


%%% S:stage2 :- S.ready_edb[].
isa(_h28525,stage2) :- 
        fd_rhs(_h28525,'flora_@'(ready_edb),_h28606).


%%% S:stage3 :- S.ready_idb[].
isa(_h28934,stage3) :- 
        fd_rhs(_h28934,'flora_@'(ready_idb),_h29015).


%%% S:stage4 :- S:stage3, S.ready_user[].
isa(_h29429,stage4) :- 
        isa_rhs(_h29429,stage3),
        fd_rhs(_h29429,'flora_@'(ready_user),_h29553).


%%% T:stage1 :- S.ready_changes[], T= S + 1.
isa(_h30017,stage1) :- 
        fd_rhs(_h30051,'flora_@'(ready_changes),_h30143),
        is(_h30191,+(_h30051,1)),
        =(_h30017,_h30191).


%%%   tst1 :- pps[produces@(S)->>P], write(S), write('  '), writeln(P), fail.
tst1 :- 
	(mvd_rhs(pps,'flora_@'(produces,X),Y), fail
	; get_residual(mvd_rhs(pps,'flora_@'(produces,X),Y),[]),
	    write(X),
	    write('  '),
	    writeln(Y),
	    fail).
tst11 :- 
	mvd_rhs(pps,'flora_@'(produces,X),Y),
        write(X),
        write('  '),
        writeln(Y),
        fail.


%%% tst1.
tst1.


tst2 :- 
	(aux(X,Y,Z), fail
	; get_residual(aux(X,Y,Z), []),
	    write(X),
	    write(Y),
	    write('  '),
	    writeln(Z),
	    fail).
tst22 :- 
	aux(X,Y,Z),
        write(X),
        write(Y),
        write('  '),
        writeln(Z),
        fail.

:- table aux/3.
aux(X,Y,Z) :-
	fd_rhs(X,'flora_@'(hasConfig,Y),_h32354),
	mvd_rhs(_h32354,'flora_@'(needsPart),Z).


%%% tst2.
tst2.


test :- 
        tell(temp),
        tst1,
        tst2,
        told.


%%%  :- table ttt1/2, ttt2/3.
:- table
        ttt1/2,
        ttt2/3.


%%% ttt1(S,P) :- pps[produces@(S)->>P].
ttt1(_h33921,_h33935) :- 
        mvd_rhs(pps,'flora_@'(produces,_h33921),_h33935).


%%% tttt1(S,P) :- ttt1(S,P), get_residual(ttt1(S,P),[]).
tttt1(_h34610,_h34624) :- 
        (ttt1(_h34610,_h34624), fail
	; get_residual(ttt1(_h34610,_h34624),[])).


%%%  ttt2(Q,P,S) :- P.hasConfig@(S)[needsPart->>Q].
ttt2(_h35348,_h35362,_h35376) :- 
        fd_rhs(_h35362,'flora_@'(hasConfig,_h35376),_h35497),
        mvd_rhs(_h35497,'flora_@'(needsPart),_h35348).


%%% tttt2(Q,P,S) :- ttt2(Q,P,S), get_residual(ttt2(Q,P,S),[]).
tttt2(_h36174,_h36188,_h36202) :- 
        (ttt2(_h36174,_h36188,_h36202), fail
	; get_residual(ttt2(_h36174,_h36188,_h36202),[])).


%%%  v1(S,P) :- passatC.hasConfig@(0)[needsPart->>P].
%%% 
v1(S,P) :- 
        fd_rhs(passatC,'flora_@'(hasConfig,0),Y),
        mvd_rhs(Y,'flora_@'(needsPart),P).
:- table vv1/2.
vv1(S,P) :- 
        fd_rhs(passatC,'flora_@'(hasConfig,S),Y),
        mvd_rhs(Y,'flora_@'(needsPart),P).
%%%  


:- table subclass/2.
:- table strict_subclass/2.

subclass(X,X) :- exists(X).
subclass(X,Y) :- strict_subclass(X,Y).

:- table strict_sub/2.
strict_sub(X,Y) :- 
	ground(X), ground(Y), !,
	X \= Y, sub(X,Y).
strict_sub(X,Y) :- sub(X,Y),  X \= Y.
strict_subclass(X,Y) :- strict_sub(X,Y).


/*****************************************************************************
  closure rules for X:Y, X::Z implies X:Z
*****************************************************************************/
:- table isa/2.

isa(O,C) :-
	strict_subclass(C1,C),
	isa(O,C1).


/*****************************************************************************
  rules for monotonic inheritance of signatures
*****************************************************************************/
:- table fs/3.
:- table mvs/3.
:- table ifs/3.
:- table imvs/3.

%% Instance
fs(O,MethodArgs,R) :-
	fs(Class,MethodArgs,R),
	( isa(O,Class)
	  ;
	  eql(Class,C), isa(O,C)
	).
ifs(O,MethodArgs,R) :-
	ifs(Class,MethodArgs,R),
	( isa(O,Class)
	  ;
	  eql(Class,C), isa(O,C)
	).

mvs(O,MethodArgs,R) :-
	mvs(Class,MethodArgs,R),
	( isa(O,Class)
	  ;
	  eql(Class,C), isa(O,C)
	).
imvs(O,MethodArgs,R) :-
	imvs(Class,MethodArgs,R),
	( isa(O,Class)
	  ;
	  eql(Class,C), isa(O,C)
	).

%% Subclass
fs(Sub,MethodArgs,R) :-
	fs(Class,MethodArgs,R),
	( strict_subclass(Sub,Class)
	  ;
	  eql(Class,S), strict_subclass(Sub,S)
	).
ifs(Sub,MethodArgs,R) :-
	ifs(Class,MethodArgs,R),
	( strict_subclass(Sub,Class)
	  ;
	  eql(Class,S), strict_subclass(Sub,S)
	).

mvs(Sub,MethodArgs,R) :-
	mvs(Class,MethodArgs,R),
	( strict_subclass(Sub,Class)
	  ;
	  eql(Class,S), strict_subclass(Sub,S)
	).
imvs(Sub,MethodArgs,R) :-
	imvs(Class,MethodArgs,R),
	( strict_subclass(Sub,Class)
	  ;
	  eql(Class,S), strict_subclass(Sub,S)
	).


/*****************************************************************************
  rules for nonmonotonic inheritance of behavior
*****************************************************************************/


fd(Object,Method,Value) :-
	%% If Object,Method are ground, check if Object[Method->...]
	%% is defined right away --an optimization
	(ground(Object), ground(Method) ->
	    tnot(defined_fd(Object,Method)),
	    isa(Object,Class),
	    derived_ifd(Class,Method,Value)
	  ;
	    isa(Object,Class),
	    derived_ifd(Class,Method,Value),
	    tnot_all(defined_fd(Object,Method))
	),
	tnot_all(overwritten_fd(Object,Class,Method)),
	tnot_all(conflict_fd(Object,Class,Method)).

overwritten_fd(Object,Class,Method) :-
	isa(Object,SubClass),
	strict_subclass(SubClass,Class),
	defined_ifd(SubClass,Method).

%% When this is called, Object always belongs to Class
conflict_fd(Object,Class,Method) :-
	defined_ifd(Class1,Method),
	ground(Class1), ground(Class),
	Class \= Class1,
	isa(Object,Class1),
	tnot(strict_subclass(Class1,Class)),
	tnot(strict_subclass(Class,Class1)).


mvd(Object,Method,Value) :-
	(ground(Object), ground(Method) ->
	    tnot(defined_mvd(Object,Method)),
	    isa(Object,Class),
	    derived_imvd(Class,Method,Value)
	  ;
	    isa(Object,Class),
	    derived_imvd(Class,Method,Value),
	    tnot_all(defined_mvd(Object,Method))
	),
	tnot_all(overwritten_mvd(Object,Class,Method)),
	tnot_all(conflict_mvd(Object,Class,Method)).

overwritten_mvd(Object,Class,Method) :-
	isa(Object,SubClass),
	strict_subclass(SubClass,Class),
	defined_imvd(SubClass,Method).

conflict_mvd(Object,Class,Method) :-
	defined_imvd(Class1,Method),
	ground(Class1), ground(Class),
	Class \= Class1,
	isa(Object,Class1),
	tnot(strict_subclass(Class1,Class)),
	tnot(strict_subclass(Class,Class1)).


ifd(Class,Method,Value) :-
	(ground(Class), ground(Method) ->
	    tnot(defined_ifd(Class,Method)),
	    strict_subclass(Class,Super),
	    derived_ifd(Super,Method,Value)
	  ;
	    strict_subclass(Class,Super),
	    derived_ifd(Super,Method,Value),
	    tnot_all(defined_ifd(Class,Method))
	),
	tnot_all(overwritten_ifd(Class,Super,Method)),
	tnot_all(conflict_ifd(Class,Super,Method)).


overwritten_ifd(Class,Super,Method) :-
	strict_subclass(Class,S),
	strict_subclass(S,Super),
	defined_ifd(S,Method).

%% Note: when this is called, Class is always a subclass of Super
conflict_ifd(Class,Super,Method) :-
	defined_ifd(Super1,Method),
	ground(Super1), ground(Super),
	Super1 \= Super,
	strict_subclass(Class,Super1),
	tnot(strict_subclass(Super1,Super)),
	tnot(strict_subclass(Super,Super1)).


imvd(Class,Method,Value) :-
	(ground(Class), ground(Method) ->
	    tnot(defined_imvd(Class,Method)),
	    strict_subclass(Class,Super),
	    derived_imvd(Super,Method,Value)
	  ;
	    strict_subclass(Class,Super),
	    derived_imvd(Super,Method,Value),
	    tnot_all(defined_imvd(Class,Method))
	),
	tnot_all(overwritten_imvd(Class,Super,Method)),
	tnot_all(conflict_imvd(Class,Super,Method)).


overwritten_imvd(Class,Super,Method) :-
	strict_subclass(Class,S),
	strict_subclass(S,Super),
	defined_imvd(S,Method).

%% Note: when this is called, Class is always a subclass of Super
conflict_imvd(Class,Super,Method) :-
	defined_imvd(Super1,Method),
	ground(Super1), ground(Super),
	Super1 \= Super,
	strict_subclass(Class,Super1),
	tnot(strict_subclass(Super1,Super)),
	tnot(strict_subclass(Super,Super1)).


/*****************************************************************************
  rules for congruence due to single-valued methods
*****************************************************************************/
:- table eql/2.
:- table metheql/2.

eql(X,Y) :-
	fd(O,M1,X),
	(M1=M2 ; metheql(M1,M2)),
	fd(O,M2,Y),
	not X=Y.

eql(X,Y) :-
	eql(O1,O2),
	fd(O1,M1,X),
	(M1=M2 ; metheql(M1,M2)),
	fd(O2,M2,Y),
	not X=Y.

eql(X,Y) :-
	eql(X,Z),
	eql(Z,Y),
	not X=Y.


metheql(X,Y) :-
	( nonvar(X) ->
	    functor(X,'flora_@',N), N >= 1,
	    functor(Y,'flora_@',N)

	  ; nonvar(Y) ->
	    functor(Y,'flora_@',N), N >= 1,
	    functor(X,'flora_@',N)
	),
	argeql(X,Y,N),
	not X=Y.


argeql(_,_,0).

argeql(X,Y,N) :-
	N >= 1,
	arg(N,X,Xn),
	arg(N,Y,Yn),
	(Xn=Yn ; eql(Xn,Yn)),
	M is N-1,
	argeql(X,Y,M).


/*****************************************************************************
  rules for object existence and empty result sets
*****************************************************************************/
:- table exists/1.
:- table mvd/2.
:- table imvd/2.

exists(X) :- fd(X,_,_).
exists(X) :- fd(_,_,X).
exists(X) :- mvd(X,_,_).
exists(X) :- mvd(_,_,X).
exists(X) :- ifd(X,_,_).
exists(X) :- ifd(_,_,X).
exists(X) :- imvd(X,_,_).
exists(X) :- imvd(_,_,X).
exists(X) :- isa(X,_).
exists(X) :- isa(_,X).
exists(X) :- sub(X,_).
exists(X) :- sub(_,X).

exists(X) :-
	fd(_,M,_),
	M =.. [_|L],
	member(X,L).

exists(X) :-
	mvd(_,M,_),
	M =.. [_|L],
	member(X,L).

exists(X) :-
	ifd(_,M,_),
	M =.. [_|L],
	member(X,L).

exists(X) :-
	imvd(_,M,_),
	M =.. [_|L],
	member(X,L).


mvd(O,M) :- mvd(O,M,_).

imvd(O,M) :- imvd(O,M,_).


/*****************************************************************************
  rules for flattened path expressions on RHS
*****************************************************************************/
:- table fd_rhs/3.
:- table mvd_rhs/3.
:- table ifd_rhs/3.
:- table imvd_rhs/3.
:- table mvd_rhs/3.
:- table imvd_rhs/2.
:- table isa_rhs/2.
:- table sub_rhs/2.
:- table fs_rhs/3.
:- table mvs_rhs/3.


/****************************************************************************/
fd_rhs(Object,Method,Result) :-
	fd(Object,M,Result),
	(M=Method ; metheql(M,Method)).

fd_rhs(Object,Method,Result) :-
	eql(Object,O),
	fd(O,M,Result),
	(M=Method ; metheql(M,Method)).

fd_rhs(Object,Method,Result) :-
	eql(Result,R),
	fd(Object,M,R),
	(M=Method ; metheql(M,Method)).

fd_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O),
	fd(O,M,R),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
mvd_rhs(Object,Method,Result) :-
	mvd(Object,M,Result),
	(M=Method ; metheql(M,Method)).

mvd_rhs(Object,Method,Result) :-
	eql(Object,O),
	mvd(O,M,Result),
	(M=Method ; metheql(M,Method)).

mvd_rhs(Object,Method,Result) :-
	eql(Result,R),
	mvd(Object,M,R),
	(M=Method ; metheql(M,Method)).

mvd_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O),
	mvd(O,M,R),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
ifd_rhs(Object,Method,Result) :-
	ifd(Object,M,Result),
	(M=Method ; metheql(M,Method)).

ifd_rhs(Object,Method,Result) :-
	eql(Object,O),
	ifd(O,M,Result),
	(M=Method ; metheql(M,Method)).

ifd_rhs(Object,Method,Result) :-
	eql(Result,R),
	ifd(Object,M,R),
	(M=Method ; metheql(M,Method)).

ifd_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O),
	ifd(O,M,R),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
imvd_rhs(Object,Method,Result) :-
	imvd(Object,M,Result),
	(M=Method ; metheql(M,Method)).

imvd_rhs(Object,Method,Result) :-
	eql(Object,O),
	imvd(O,M,Result),
	(M=Method ; metheql(M,Method)).

imvd_rhs(Object,Method,Result) :-
	eql(Result,R),
	imvd(Object,M,R),
	(M=Method ; metheql(M,Method)).

imvd_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O),
	imvd(O,M,R),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
mvd_rhs(Object,Method) :-
	mvd(Object,M),
	(M=Method ; metheql(M,Method)).

mvd_rhs(Object,Method) :-
	eql(Object,O),
	mvd(O,M),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
imvd_rhs(Object,Method) :-
	imvd(Object,M),
	(M=Method ; metheql(M,Method)).

imvd_rhs(Object,Method) :-
	eql(Object,O),
	imvd(O,M),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
isa_rhs(Object1,Object2) :-
	isa(Object1,Object2).

isa_rhs(Object1,Object2) :-
	eql(Object1,O1),
	isa(O1,Object2).

isa_rhs(Object1,Object2) :-
	eql(Object2,O2),
	isa(Object1,O2).

isa_rhs(Object1,Object2) :-
	eql(Object1,O1),
	eql(Object2,O2),
	isa(O1,O2).

/****************************************************************************/
sub_rhs(Object1,Object2) :-
	subclass(Object1,Object2).

sub_rhs(Object1,Object2) :-
	eql(Object1,O1),
	subclass(O1,Object2).

sub_rhs(Object1,Object2) :-
	eql(Object2,O2),
	subclass(Object1,O2).

sub_rhs(Object1,Object2) :-
	eql(Object1,O1),
	eql(Object2,O2),
	subclass(O1,O2).

/****************************************************************************/
fs_rhs(Object,Method,Result) :-
	fs(Object,M,Result),
	(M=Method ; metheql(M,Method)).

fs_rhs(Object,Method,Result) :-
	eql(Object,O),
	fs(O,M,Result),
	(M=Method ; metheql(M,Method)).

fs_rhs(Object,Method,Result) :-
	eql(Result,R),
	fs(Object,M,R),
	(M=Method ; metheql(M,Method)).

fs_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O,
	fs(O,M,R)),
	(M=Method ; metheql(M,Method)).

/****************************************************************************/
mvs_rhs(Object,Method,Result) :-
	mvs(Object,M,Result),
	(M=Method ; metheql(M,Method)).

mvs_rhs(Object,Method,Result) :-
	eql(Object,O),
	mvs(O,M,Result),
	(M=Method ; metheql(M,Method)).

mvs_rhs(Object,Method,Result) :-
	eql(Result,R),
	mvs(Object,M,R),
	(M=Method ; metheql(M,Method)).

mvs_rhs(Object,Method,Result) :-
	eql(Result,R),
	eql(Object,O),
	mvs(O,M,R),
	(M=Method ; metheql(M,Method)).

/*****************************************************************************
  complementary rules to protect against "undefined" error reported by XSB 
*****************************************************************************/
fd(_,_,_)   :- fail.
mvd(_,_,_)  :- fail.
ifd(_,_,_)  :- fail.
imvd(_,_,_) :- fail.
fs(_,_,_)   :- fail.
mvs(_,_,_)  :- fail.
sub(_,_)    :- fail.
isa(_,_)    :- fail.


/****************************  End of Trailer  ******************************/

