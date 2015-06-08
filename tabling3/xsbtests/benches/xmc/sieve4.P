% XSB Code for Eratosthenes' Sieve:
%    patterned after, but slightly diffrent from,
%    the Bell Labs bench suite from Doron Peled
%    Y.S.R., 23 Dec 96.
                         
                 
% value BUF_SIZE	: 1 { buffer size in each link }
% value N		: 7 { number of processes }
% value MAX       : 32 { largest number to check for primeness + 1 }
% value NUM_PRIME : 4  { number of primes between the Nth & MAX (exclusive) }
% value MAX_PRIME : 31 { the largest prime less than MAX, not divisible by
%                       first N primes less than MAX }

% type buf_ptr	:BUF_SIZE   { for use in the ring buffer }
% type num        :MAX        { domain of numbers }


%%%%%%%%%%%%%%%%%%%%%%%%
% a size N buffer


chan(N, Buf) ::=
	length(Buf, Len) o 
	if(Len == 0,
		% empty buffer can only receive
		receive_only(N, Buf),
		% buffer either full or partly filled
		if(Len == N,
			% full buffer can only send
			send_only(N, Buf),
			% partially filled buffer can either receive or send
			receive_or_send(N, Buf)
		)
	).

receive_only(N, Buf) ::=
	in(chan_in(Msg)) o chan(N, [Msg|Buf]).

send_only(N, Buf) ::=
	strip_from_end(Buf, Msg, RNBuf) o
			out(chan_out(Msg)) o 
			chan(N, RNBuf).

receive_or_send(N, Buf) ::= 
	send_only(N, Buf) # receive_only(N, Buf).


strip_from_end([X], X, []).
strip_from_end([X,Y|Ys], Z, [X|Zs]) :- strip_from_end([Y|Ys], Z, Zs).



%%%%%%%%%%%%%%%%%%%%%%%%
% {  the generator process }
% 

/*
generator(Cur, Max) ::= if( Cur \== Max,
			    out(right(Cur)) o Cur1 := (Cur + 1) o generator(Cur1, Max),
			    nil ).
*/

generator(Cur, Max) ::= if( Cur \== Max,
			    out(right(Cur)) o generator1(Cur,Max), nil).

generator1(Cur,Max) ::= 
	Cur1 := (Cur + 1) o generator(Cur1, Max).
			    

%%%%%%%%%%%%%%%%%%%%%%%%
% { the tester process }
%

tester ::= in(left(Myval)) o tester1(Myval).

/*
tester1(Myval) ::=
	in(left(Val)) o
	if((Tmp is (Val mod Myval), Tmp \== 0),
		out(right(Val)) o tester1(Myval),
		tester1(Myval)).
*/

tester1(Myval) ::=
	in(left(Val)) o tester2(Myval,Val).

tester2(Myval,Val) ::= 
	if((Tmp is (Val mod Myval), Tmp \== 0),
		out(right(Val)) o tester1(Myval),
		tester1(Myval)).

%%%%%%%%%%%%%%%%%%%%%%%%
% { the consumer process }

/*
consumer(N, Max, Val) ::=  % N is NUM_PRIME
	if(N \== 0,
		in(left(NVal)) o N1 := (N-1) o consumer(N1, Max, NVal),
		if(Val == Max, finish, fail)).
*/

consumer(N, Max, Val) ::=  % N is NUM_PRIME
	if(N \== 0,
		in(left(NVal)) o consumer2(N,Max,NVal),
		if(Val == Max, finish, fail)).

consumer2(N,Max,NVal) ::= 
	N1 := (N-1) o consumer(N1, Max, NVal).

%%%%%%%%%%%%%%%%%%%%%%%%
% { the sieve system }

sieve ::= (gen '||'
	test1 '||' test2 '||' test3 '||' test4 '||'
	cons '||'
	chan0 '||' chan1 '||' chan2 '||' chan3 '||' chan4
	)
	\ {gen_out(X), in1(X), in2(X), in3(X), in4(X), in4(X),
            con_in(X), out1(X), out2(X), out3(X), out4(X)}.


gen ::= generator(2, 25) @ [right(X) / gen_out(X)].

test1 ::= tester @ [left(X) / in1(X), right(X) / out1(X)].
test2 ::= tester @ [left(X) / in2(X), right(X) / out2(X)].
test3 ::= tester @ [left(X) / in3(X), right(X) / out3(X)].
test4 ::= tester @ [left(X) / in4(X), right(X) / out4(X)].

cons ::= consumer(5, 23, _) @ [left(X) / con_in(X), finish / finish, fail / fail].

chan0 ::= chan(1,[]) @ [chan_in(X) / gen_out(X), chan_out(X) / in1(X)]. % { link from generator to 1 }

chan1 ::= chan(1,[]) @ [chan_in(X) / out1(X), chan_out(X) / in2(X)].
chan2 ::= chan(1,[]) @ [chan_in(X) / out2(X), chan_out(X) / in3(X)].
chan3 ::= chan(1,[]) @ [chan_in(X) / out3(X), chan_out(X) / in4(X)].
chan4 ::= chan(1,[]) @ [chan_in(X) / out4(X), chan_out(X) / con_in(X)].

% should be Yes
 
fDef( ae_finish, lfp(fAnd( fDiamMinus( nil, tt),
                       fBoxMinus( finish, form(ae_finish))))).
 
% should be No
 
fDef( ee_fail, lfp(fOr( fDiam( fail, tt),
                    fDiamMinus(nil, form(ee_fail))))).

