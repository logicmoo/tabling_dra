
:- import storage_insert_keypair/4, storage_insert_keypair_bt/4,
	storage_find_keypair/3, storage_delete_keypair_bt/3,
	storage_delete_keypair/3, storage_insert_fact_bt/3,
	storage_find_fact/2, storage_delete_fact_bt/3,
	storage_insert_fact/3, storage_delete_fact/3,
	storage_reclaim_space/1
	from storage.

%% X is 1
:- storage_insert_keypair(storage123, 111,f(a),X), write('X='), writeln(X).

%% Y=-1 (this key exists)
:- storage_insert_keypair(storage123, 111,f(_X), Y), write('Y='), writeln(Y).

%% Y=0
:- storage_insert_keypair(storage123, 111,f(a), Y), write('Y='), writeln(Y).

%% X=a
:- storage_find_keypair(storage123, 111,f(X)), write('X='), writeln(X).

%% X=1
%% Temporarily deleted1
:- storage_delete_keypair_bt(storage123, 111,X),write('X='), writeln(X),
	(storage_find_keypair(storage123, 111,f(X))->writeln('bug')
	; writeln('temporarily deleted1')
	),
	fail.

%% Still XXX=a, since we failed with storage_delete_keypair_bt
:- storage_find_keypair(storage123, 111,f(X)), write('XXX='), writeln(X).

%% X=1: succeded
:- storage_delete_keypair_bt(storage123, 111,X),write('XX='), writeln(X).

%% not found
:- (storage_find_keypair(storage123, 111,f(X))->writeln('bug1')
   ; writeln('not found')
   ).


%% YY=1
:- storage_insert_keypair(storage123, 111,f(_X), Y),  write('YY='), writeln(Y).

%% not found1
:- storage_find_keypair(storage123, 111,g(X))
	-> writeln(bug2)
	; writeln('not found1').

%% ok
:- storage_find_keypair(storage123, 111,f(_X))->writeln(ok); writeln('bug4').

%% XXXX=1
:- storage_delete_keypair(storage123, 111,X), write('XXXX='), writeln(X).

%% not found3
:- (storage_find_keypair(storage123, 111,X)->writeln('bug5')
   ; writeln('not found3')
   ).


%% aaa=kkk
%% Del=1
%% Not found4
:- storage_insert_keypair_bt(storage123, aaa,kkk,_),
	storage_find_keypair(storage123, aaa,X),write('aaa='), writeln(X),
	storage_delete_keypair_bt(storage123, aaa,Del),
	write('Del='), writeln(Del),
	(storage_find_keypair(storage123, aaa,X)->writeln('bug6')
	; writeln('not found4')
	),
	fail.
%% Not found5
:- (storage_find_keypair(storage123, aaa,X)->writeln('bug7')
   ; writeln('not found5')
   ).

:- storage_insert_keypair_bt(storage123, aaa,pp,X).

%% aaa=pp
%% Del=1
%% not found6
:- storage_find_keypair(storage123, aaa,X), write('aaa='), writeln(X),
	storage_delete_keypair_bt(storage123, aaa,Del),
	write('Del='), writeln(Del),
	(storage_find_keypair(storage123, aaa,X)->writeln('bug66')
	; writeln('not found6')
	),
	fail.
%% X=pp
:- (storage_find_keypair(storage123, aaa,X)->write('X='),writeln(X)
   ; writeln('bug8')
   ).

%% aaa=pp
%% Del=1
%% Not found9
%% Del2=0 (because aaa was temporarily deleted at this point)
:- storage_find_keypair(storage123, aaa,X), write('aaa='), writeln(X),
	storage_delete_keypair_bt(storage123, aaa,Del),
	write('Del='), writeln(Del),
	(storage_find_keypair(storage123, aaa,X)->writeln('bug66')
	; writeln('not found9')
	),
	storage_delete_keypair(storage123, aaa,Del2),
	write('Del2='), writeln(Del2),
	fail.

%% aaa=pp (because storage_delete_keypair above didn't do anything,
%% and we then backtreacked erasing all backtrackable changes)
:- (storage_find_keypair(storage123, aaa,X)->write('aaa='), writeln(X)
   ; writeln('bug99')
   ).


%% aaa=pp
%% Del3=1
%% not found009
%% Del4=0 (because it was previously deleted
:- storage_find_keypair(storage123, aaa,X), write('aaa='), writeln(X),
	storage_delete_keypair(storage123, aaa,Del),
	write('Del3='), writeln(Del),
	(storage_find_keypair(storage123, aaa,X)->writeln('bug666')
	; writeln('not found009')
	),
	storage_delete_keypair_bt(storage123, aaa,Del2),
	write('Del4='), writeln(Del2),
	fail.

%% not found0
:- (storage_find_keypair(storage123, aaa,X)->writeln('bug777')
   ; writeln('not found0')
   ).


%% Need to add these
%% Answer: hello,yes,no
p :- storage_insert_fact_bt(aaa, f(a), _X),
	(storage_insert_fact_bt(aaa, f(a), _), fail
	; storage_find_fact(aaa, f(a)), writeln(hello)),
	(storage_delete_fact_bt(aaa,f(a),_),
	    (storage_find_fact(aaa,f(a))->writeln(no); writeln(yes)),
	    fail
	; (storage_find_fact(aaa,f(a))->writeln(no); writeln(yes))).

%% Answer: hello,yes,no,hello
q :- storage_insert_fact_bt(aaa, f(a), _X),
	(storage_insert_fact(aaa, f(a), _), fail
	; storage_find_fact(aaa, f(a)), writeln(hello)),
	(storage_delete_fact_bt(aaa,f(a),_),
	    (storage_find_fact(aaa,f(a))->writeln(no); writeln(yes)),
	    fail
	; (storage_find_fact(aaa,f(a))->writeln(no); writeln(yes))),
	fail.
q:- storage_find_fact(aaa,f(a)), writeln(hello).


:- p, q.
