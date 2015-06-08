% XSB/LMC Code for Leader Election Protocol:
%    Dolev, Klawe, Rodeh,
%    exactly as in the Bell Labs bench suite from Doron Peled
%    Y.S.R., 19 Dec 96.
                         
                 
% bUF_SIZE	is 10.	% { buffer size in each link }
% n		is 5.	% { number of processes }

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
% {  the node process involved in electing a leader }
 
node(N) ::=
      in(recv(Id)) o
      out(right(msg(first, Id))) o
      nodeActive(N, Id, _Nbr).
%      nodeStart(N, true, Id, _Nbr).

nodeStart(N, Active, Maxi, Nbr) ::=
     if(Active == true,
	nodeActive(N, Maxi, Nbr),
	nodeInactive).

nodeActive(N, Maxi, Nbr) ::=
	in(left(msg(Type,NId))) o
		if(Type == first,
		  type_is_first(N, Maxi, NId),
		  type_is_second(N, Maxi, Nbr, NId)
		).

type_is_first(N, Maxi, NId) ::=
	if(NId \== Maxi,
		out(right(msg(second,NId))) o
%			nodeStart(N, true, Maxi, NId),
			nodeActive(N, Maxi, NId),
		if( (Temp is (N - 1), Maxi \== Temp), fail,
			leader o
%			  nodeStart(N, true, Maxi, NId))).
			  nodeActive(N, Maxi, NId))).
type_is_second(N, Maxi, Nbr, NId) ::=
	if((Nbr > NId, Nbr > Maxi),
		out(right(msg(first,Nbr))) o
%			nodeStart(N, true, Nbr, Nbr),
			nodeActive(N, Nbr, Nbr),
%		nodeStart(N, false, Maxi, Nbr)).
		nodeInactive).
nodeInactive ::=
	in(left(Msg)) o out(right(Msg)) o nodeInactive.

%%%%%%%%%%%%%%%%%%%%%%%%
% The Id giver

idGiver ::=
	out(snd0(2)) o
	out(snd1(1)) o
	out(snd2(0)) o
	out(snd3(4)) o
	out(snd4(3)).

%%%%%%%%%%%%%%%%%%%%%%%%
% { Leader election Protocol }
% 
% 

 
/*
systemLeader ::=
(idGiver '||'
	node0 '||' node1 '||' node2 '||' node3 '||' node4 '||' 
	chan0 '||' chan1 '||' chan2 '||' chan3 '||' chan4)
			\ {snd0(_), snd1(_), snd2(_), snd3(_), snd4(_),
		in4to0(_), in0to1(_), in1to2(_), in2to3(_), in3to4(_),
		out0to1(_), out1to2(_), out2to3(_), out3to4(_), out4to0(_)}.
*/
 

systemLeader ::=
	(idGiver '||'
	node0 '||' chan0 '||' node1 '||' chan1 '||' node2 '||' chan2 '||'
	node3 '||' chan3 '||' node4 '||' chan4 )
			\ {snd0(_), snd1(_), snd2(_), snd3(_), snd4(_),
		in4to0(_), in0to1(_), in1to2(_), in2to3(_), in3to4(_),
		out0to1(_), out1to2(_), out2to3(_), out3to4(_), out4to0(_)}.


node0 ::= node(5) @ [recv(X) / snd0(X), left(X) / in4to0(X), right(X) / out0to1(X), fail / fail, leader / leader].
node1 ::= node(5) @ [recv(X) / snd1(X), left(X) / in0to1(X), right(X) / out1to2(X), fail / fail, leader / leader].
node2 ::= node(5) @ [recv(X) / snd2(X), left(X) / in1to2(X), right(X) / out2to3(X), fail / fail, leader / leader].
node3 ::= node(5) @ [recv(X) / snd3(X), left(X) / in2to3(X), right(X) / out3to4(X), fail / fail, leader / leader].
node4 ::= node(5) @ [recv(X) / snd4(X), left(X) / in3to4(X), right(X) / out4to0(X), fail / fail, leader / leader].

chan0 ::= (chan(10, []) @ [chan_in(X) / out0to1(X), chan_out(X) / in0to1(X)]).
chan1 ::= (chan(10, []) @ [chan_in(X) / out1to2(X), chan_out(X) / in1to2(X)]).
chan2 ::= (chan(10, []) @ [chan_in(X) / out2to3(X), chan_out(X) / in2to3(X)]).
chan3 ::= (chan(10, []) @ [chan_in(X) / out3to4(X), chan_out(X) / in3to4(X)]).
chan4 ::= (chan(10, []) @ [chan_in(X) / out4to0(X), chan_out(X) / in4to0(X)]).
