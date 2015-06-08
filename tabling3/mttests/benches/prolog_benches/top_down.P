/* Need to add foreign / domestic flags */

label(Beflag,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	order_of(Beflag,
		ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)),!.
label(inflags(_,'B'),ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 	
	strict_get_attention(Attn,Attn1),!,
	entity('B',ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn1,Fl,Oth)).
label(Beflag,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 	
	entity(Beflag,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)),
	!.		%arity hack.

order_of(inflags(Fd,'B'),
		ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	['TO'],!,
	{create_flag_struct(Fl)},
	order_of_1(inflags(Fd,'B'),
		ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)),
	{set_oo_flag(Fl)},!.
order_of(inflags(Fd,'B'),
		ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	order,
	opt_of,
	opt_of,!,
	{create_flag_struct(Fl)},
	order_of_2(inflags(Fd,'B')
		,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)),
	{set_oo_flag(Fl)}.
order_of(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	['HOLDER'],
	opt_of,
	opt_of,!,
	bl_adj(One),
	{create_flag_struct(Fl)},
	bill(Bef,ea(_Rel,entity(Ent,Title,Two),Addr,Tel,Attn,Fl,Oth)),
	{append(One,['B/L'|Two],Rest)},	
	{set_oo_flag(Fl)}.

order_of_1(Bef,Ea) --> 
	opt_word,
	order,!,
	opt_of,
	order_of_2(Bef,Ea).
order_of_1(Bef,ea(_Rel,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	opt_word,
	['HOLDER'],
	opt_of,
	bl_adj(One),
	bill(Bef,ea(_Rel,entity(Ent,Title,Two),Addr,Tel,Attn,Fl,Oth)),
	!,{append(One,['B/L'|Two],Rest)}.
order_of_1(Bef,Ea) --> 
	entity(Bef,Ea). 	%dont bother with special entity stuff here.

order_of_2(Bef,ea(_Rel,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	['HOLDER'],!,
	opt_of,
	bl_adj(One),
	bill(Bef,ea(_Rel,entity(Ent,Title,Two),Addr,Tel,Attn,Fl,Oth)),
	!,{append(One,['B/L'|Two],Rest)}.
order_of_2(Bef,ea(_Rel,entity(Ent,Title,Two),Addr,Tel,Attn,Fl,Oth)) --> 
	opt(['ORIG']),
	bill_of_lading,!,
	bill(Bef,ea(_Rel,entity(Ent,Title,Two),Addr,Tel,Attn,Fl,Oth)),!.
order_of_2(Bef,ea(_Rel,entity([],[],['BLANK']),Addr,Tel,Attn,Fl,Oth)) --> 
	opt(['&']),
	['BLANK'],
	!,
	address(Bef,Addr,Attn,Tel,Fl,Oth).
order_of_2(Bef,ea(_Rel,entity([],[],Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	referent(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
order_of_2(Bef,Ea) --> 
        order_of_3(Bef,Ea),!.
order_of_2(Bef,Ea) --> 	entity(Bef,Ea),!.
order_of_2(_Bef,ea(_Rel,entity([],[],[]),[],tel([],[]),[],_Fl,_Oth)) --> [].
			   
order_of_3(Bef,Ea) --> 
	order,
	opt_of,
	order_of_2(Bef,Ea).

bill(Bef,Ea) --> 
	order_of(Bef,Ea),!.
bill(Bef,ea(_Rel,entity(BL,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	issued(Iss),!,
	bill(Bef,ea(_Rel,entity(BL,Title,R),Addr,Tel,Attn,Fl,Oth)),
	{append(Iss,R,Rest)}.
bill(_Bef,ea(_Rel,entity([],[],Cnee),Addr,Tel,Attn,Fl,Oth)) --> 
	consignee(Cnee,Addr,Tel,Attn,Fl,Oth),!.
bill(_Bef,ea(_Rel,entity([],[],[]),[],tel([],[]),[],_Fl,[])) --> 
	subject,!.
bill(Bef,ea(_Rel,entity([],[],[]),Addr,Tel,Attn,Fl,Oth)) --> 
	non_num_unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!. 
bill(Bef,ea(_Rel,entity(Name,Title,[Word|Rest]),Addr,Tel,Attn,Fl,Oth)) --> 
	[Word],!,
	bill(Bef,ea(_Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)).
bill(_Bef,ea(_Rel,entity([],[],[]),[],tel([],[]),[],_Fl,[])) --> [].
		  
entity(Bef,Ea) --> 	organization(Bef,Ea).

organization(Bef,ea(Rel,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	{create_flag_struct(Fl)},
        org_name(Bef,ea(Rel,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)).

unamb_org_name(Rel,ea(Rel,entity([Word|Nm],Title,Rest),
					Addr,Tel,Attn,Fl,Oth)) --> 
	['MS'],!,
	{create_flag_struct(Fl)},
	[Word],
	org_name_1(inflags('N','N'),Rel,Nm,Title,Addr,Rest,Tel,Attn,Fl,Oth).

org_name(Bef,ea(_Rel,entity([],[],Rest),Addr,Tel,Attn,Fl,Oth)) -->
	referent(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
org_name(Bef,ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	['('],!, 
	org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name(Bef,ea(Rel,entity(Nm,Title,[Rest1|Rest]),Addr,Tel,Attn,Fl,Oth)) --> 
	org_sequencer(Rest1,Fl),!,
	org_name(Bef,ea(Rel,entity(Nm,Title,Rest),Addr,Tel,Attn,Fl,Oth)).
org_name(_Bef,ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	special_org_name(ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)),
	!.
org_name(Bef,ea(Rel,entity([Word|Nm],Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	org_preamble(Bef,Rest1,Fl),
	[Word],!,
	org_name_1(Bef,Rel,Nm,Title,Addr,Rest2,Tel,Attn,Fl,Oth),	
	{append(Rest1,Rest2,Rest)}.
org_name(Bef,ea(_Rel,entity([],[],Rest1),[],tel([],[]),[],Fl,[])) -->
	org_preamble(Bef,Rest1,Fl).

% Bills, usually representing domestic entities and addresses make
% less use of pipes, which tend to be misleading.
% Entries, make more use of them in disambiguating names, expecially
% if they are foreign entities from the ssm table.
org_name_1(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth) --> 
	['('],!, 
	org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_1(Bef,Rel,Final,Title,Addr,R1,Tel,Attn,Fl,Oth) --> 
	org_term_prim_general(Final,Fin_handle,Title,Termr,R1,R2,Fl),
	org_terminator(Bef,Rel,Termr,Fin_handle,R2,Addr,Tel,Attn,Fl,Oth),!.
org_name_1(Bef,_Rel,[],[],Addr,[],Tel,Attn,Fl,Oth) --> 
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
org_name_1(Bef,_Rel,[],[],Addr,Rest,Tel,Attn,Fl,Oth) --> 
	referent_on1(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
org_name_1(Bef,_Rel,[],[],[],[],tel([],[]),[],_Fl,Oth) --> 
	order_of(Bef,Oth),!.
org_name_1(inflags(Fd,'E'),Rel,[],[],Addr,Rest,Tel,Attn,Fl,Oth) --> 
	filer_garbage,!,
	org_terminator_continuation(inflags(Fd,'E'),Rel,Rest,
				Addr,Tel,Attn,Fl,Oth).
org_name_1(inflags('D','E'),_Rel,['|'|Name],Title,
				Addr,Rest,Tel,Attn,Fl,Oth) --> 
	['|'],!,
	org_name_2(inflags('D','E'),_Rel,Name,Title,Addr,Rest,
					Tel,Attn,Fl,Oth,Tmp,Tmp).
org_name_1(inflags('F','E'),_Rel,['|'|Name],Title,
				Addr,Rest,Tel,Attn,Fl,Oth) --> 
	['|'],!,
	org_name_3(inflags('F','E'),1,_Rel,Name,Title,Addr,Rest,
					Tel,Attn,Fl,Oth,Tmp,Tmp).
org_name_1(Bef,_Rel,[Word|Name],Title,Addr,Rest,Tel,Attn,Fl,Oth) --> 
	[Word],!,
	org_name_1(Bef,_Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_1(_,_,[],[],[],[],tel([],[]),[],_,[]) --> [].

% org_name_2 is used for domestic entities: after first pipe, PROBABLY
% not name any more.
org_name_2(Bef,Rel,Tmp1,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,Name) --> 
	['('],!, 
	org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_2(Bef,Rel,Tmp1,Title,Addr,R1,Tel,Attn,Fl,Oth,Tmp1,Tmp2) --> 
	org_term_prim_general(Tmp2,Fin_handle,Title,Termr,R1,R2,Fl),
	org_terminator(Bef,Rel,Termr,Fin_handle,R2,Addr,Tel,Attn,Fl,Oth),!.
org_name_2(Bef,_Rel,Tmp1,[],Addr,[],Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
org_name_2(Bef,_Rel,Tmp1,[],Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	referent_on1(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
org_name_2(Bef,_Rel,Tmp1,[],[],[],tel([],[]),[],_Fl,Oth,Tmp1,[]) --> 
	order_of(Bef,Oth),!.
org_name_2(inflags(Fd,'E'),Rel,Tmp1,[],Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	filer_garbage,!,
	org_terminator_continuation(inflags(Fd,'E'),Rel,Rest,
				Addr,Tel,Attn,Fl,Oth).
org_name_2(Bef,_Rel,[],[],address(Room,Bld,Tmp1,Po,Town,State,Cntry,Zip,ARest),
			[],Tel,Attn,Fl,Oth,Tmp1,Str) --> 
	street_d(Str,Str1),!,
	address_rptsczr(Bef,Room,Bld,Str1,Po,Town,State,Cntry,Zip,ARest,
	                      Tmp,Tmp,Tel,Attn,Fl,Oth),!.
org_name_2(Bef,_Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[Word|Tmp2]) --> 
	[Word],!,
	org_name_2(Bef,_Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,Tmp2).
org_name_2(_,_,Tmp,[],[],[],tel([],[]),[],_,[],Tmp,[]) --> [].

% org_name_3 is used for foreign entities: after first pipe, ASSUME
% not name any more.
% need to add building switch to org_name_3.
org_name_3(Bef,_Ctr,Rel,Tmp1,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,Name) --> 
	['('],!, 
	org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_3(Bef,_Ctr,Rel,Tmp1,Title,Addr,R1,Tel,Attn,Fl,Oth,Tmp1,Tmp2) --> 
	org_term_prim_general(Tmp2,Fin_handle,Title,Termr,R1,R2,Fl),
	org_terminator(Bef,Rel,Termr,Fin_handle,R2,Addr,Tel,Attn,Fl,Oth),!.
org_name_3(Bef,_Ctr,_Rel,[],[],Addr,Tmp1,Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
org_name_3(Bef,_Ctr,_Rel,Tmp1,[],Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	referent_on1(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
org_name_3(Bef,_Ctr,_Rel,Tmp1,[],[],[],tel([],[]),[],_Fl,Oth,Tmp1,[]) --> 
	order_of(Bef,Oth),!.
org_name_3(inflags(Fd,'E'),_Ctr,Rel,Tmp1,[],Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[]) --> 
	filer_garbage,!,
	org_terminator_continuation(inflags(Fd,'E'),Rel,Rest,
				Addr,Tel,Attn,Fl,Oth).
org_name_3(Bef,_Ctr,_Rel,[],[],address(Room,Bld,Tmp1,Po,Town,State,Cntry,Zip,ARest),
			[],Tel,Attn,Fl,Oth,Tmp1,Str) --> 
	street_d(Str,Str1),!,
	address_rptsczr(Bef,Room,Bld,Str1,Po,Town,State,Cntry,Zip,ARest,
	                      Tmp,Tmp,Tel,Attn,Fl,Oth),!.
org_name_3(Bef,1,_Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,['|'|Tmp2]) --> 
	['|'],!,
	org_name_3(Bef,2,_Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,Tmp2).
org_name_3(Bef,2,_Rel,[],[],
		address(Room,Bld,Tmp1,Po,Town,State,Cntry,Zip,ARest),
			[],Tel,Attn,Fl,Oth,Tmp1,['|'|Str]) --> 
	['|'],!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,ARest,
	                      Tmp,Tmp,Tel,Attn,Fl,Oth),!.
org_name_3(Bef,Ctr,_Rel,Name,Title,
		Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,[Word|Tmp2]) --> 
	[Word],!,
	org_name_3(Bef,Ctr,_Rl,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth,Tmp1,Tmp2).
org_name_3(_,_,_,[],[],address([],[],Tmp,[],[],[],[],[],[]),
				[],tel([],[]),[],_,[],Tmp,[]) --> [].

filer_garbage --> ['C'],[integer(_)].
filer_garbage --> ['I'],[integer(_)].

org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth) --> 
	[')'],!, 
	org_name_1(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_1_paren(Bef,_,[],[],Addr,[],Tel,Attn,Fl,Oth) --> 
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
org_name_1_paren(Bef,Rel,Name,Title,Addr,[Word|Rest],Tel,Attn,Fl,Oth) --> 
	[Word],!,
	org_name_1_paren(Bef,Rel,Name,Title,Addr,Rest,Tel,Attn,Fl,Oth).
org_name_1_paren(_Bef,_,[],[],[],[],tel([],[]),[],_,[]) --> [].

org_terminator(Bef,Rel,Term,Final,Rest,Addr,Tel,Attn,Fl,Oth) -->
	org_term_prim(Term,Termr,Rest,R1,Fl),!,
	org_terminator(Bef,Rel,Termr,Final,R1,Addr,Tel,Attn,Fl,Oth).
org_terminator(Bef,Rel,Off,[Sbd|Final],Rest,Addr,Tel,Attn,Fl,Oth) --> 
        [Sbd],{init_subdivision_desig(Sbd)},!,
	org_name_1(Bef,Rel,Final,Off,Addr,Rest,Tel,Attn,Fl,Oth).
org_terminator(inflags(Fd,_),Rel,Off,Final,Rest,Addr,Tel,Attn,Fl,Oth) --> 
	office(Off,R),!,
	org_terminator(inflags(Fd,'N'),Rel,R,Final,Rest,Addr,Tel,Attn,Fl,Oth).
org_terminator(Bef,Rel,[Other|R],Final,Rest,Addr,Tel,Attn,Fl,Oth) --> 
	[Other],{org_term_country(Other)},!,
	org_terminator(Bef,Rel,R,Final,Rest,Addr,Tel,Attn,Fl,Oth).
org_terminator(inflags(Fd,'N'),Rel,R,Final,Rest,Addr,Tel,Attn,Fl,Oth) --> 
	['|'],!,
	org_terminator(inflags(Fd,'N'),Rel,R,Final,Rest,Addr,Tel,Attn,Fl,Oth).
org_terminator(_Bef,Rel,Term,Final,Rest,Addr,Tel,Attn,Fl,Oth) --> 
	['('],!,
	org_terminator_paren(Rel,Term,Final,Rest,Addr,Tel,Attn,Fl,Oth).
org_terminator(inflags(Fd,'E'),Rel,[],[],Rest,Addr,Tel,Attn,Fl,Oth) --> 
	filer_garbage,!,
	org_terminator_continuation(inflags(Fd,'N'),Rel,Rest,
			Addr,Tel,Attn,Fl,Oth).

org_terminator(inflags(Fd,_),Rel,[],[],Rest,Addr,Tel,Attn,Fl,Oth) --> 
	org_terminator_continuation(inflags(Fd,'N'),Rel,Rest,
			Addr,Tel,Attn,Fl,Oth).

org_terminator_paren(Rel,Term,Final,Rest,Addr,Tel,Attn,Fl,Oth) -->
	[')'],!,
	org_terminator(inflags('N','N'),Rel,Term,Final,Rest,
			Addr,Tel,Attn,Fl,Oth).
org_terminator_paren(_,[],[],[],Addr,Tel,Attn,Fl,Oth) -->
	unamb_address(inflags('N','N'),Addr,Tel,Attn,Fl,Oth),
	!.
org_terminator_paren(Rel,Term,Final,[Word|Rest],Addr,Tel,Attn,Fl,Oth) -->
	[Word],!,
	org_terminator_paren(Rel,Term,Final,Rest,Addr,Tel,Attn,Fl,Oth).
org_terminator_paren(_,[],[],[],[],tel([],[]),[],_,[]) --> [].

org_terminator_continuation(Bef,_Rel,Rest,Addr,Tel,Attn,Fl,Oth) -->
	referent(Bef,Rest,Addr,Tel,Attn,Fl,Oth),!.
org_terminator_continuation(Bef,_Rel,_Rest,Addr,Tel,Attn,Fl,Oth) --> 
	opt(['ORIG']),
	bill_of_lading,!,
	bill(Bef,ea(_Rel,_,Addr,Tel,Attn,Fl,Oth)),!.
org_terminator_continuation(_Bef,Rel,[],[],tel([],[]),[],_Fl,Oth) -->
	unamb_org_name(Rel,Oth),!.
org_terminator_continuation(Bef,_Rel,[],Addr,Tel,Attn,Fl,Oth) -->
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
org_terminator_continuation(Bef,_Rel,[],[],tel([],[]),[],_Fl,Oth) --> 
	order_of(Bef,Oth),!.
org_terminator_continuation(Bef,_Rel,[],[],tel([],[]),[],_Fl,Oth) -->
	second_entity(Bef,Oth),!.
org_terminator_continuation(Bef,_Rel,[],address(
	       Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest),Tel,Attn,Fl,Oth) --> 
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
	                      Tmp,Tmp,Tel,Attn,Fl,Oth),!.
org_terminator_continuation(Bef,_Rel,[Word|R],Addr,Tel,Attn,Fl,Oth) -->
	[Word],!,
	org_terminator_continuation(Bef,_Rel,R,Addr,Tel,Attn,Fl,Oth).
org_terminator_continuation(_Bef,_,[],[],tel([],[]),[],_,[]) --> [].

ob_entity(Bef,ea(ob,entity([],[],Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	{create_flag_struct(Fl)},
	referent(Bef,Rest,Addr,Tel,Attn,Fl,Oth),
	!.
ob_entity(Bef,ea(ob,entity([],[],[]),Addr,Tel,Attn,Fl,Oth)) --> 
	{create_flag_struct(Fl)},
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),
	!.
ob_entity(Bef,ea(ob,entity([],[],[]),Addr,Tel,Attn,Fl,Oth)) --> 
	{create_flag_struct(Fl)},
	city_country(Bef,Addr,Tel,Attn,Fl,Oth),
	!.
ob_entity(Bef,ea(ob,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) --> 
	entity(Bef,ea(ob,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)).

city_country(Bef,address(Rm,Bld,[],Po,[City],St,[Cn],Zp,Rest),
						Tel,Attn,Fl,Oth) -->
	[City],{city(City)},
	[Cntry],{country(Cntry,Cn)},
	address_rspczr(Bef,Rm,Bld,Po,St,_,Zp,Rest,Tel,Attn,Fl,Oth).

/* used in referent */
address(Bef,address([],[],[],[],[],[],[],[],[]),tel([],[]),[],_Fl,Oth) -->  
	and_connector(Bef,Oth),!.
address(Bef,Addr,Tel,Attn,Fl,Oth) --> 
	unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.
address(Bef,
	address(Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest),Tel,Attn,Fl,Oth) -->
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
	                      Tmp,Tmp,Tel,Attn,Fl,Oth),!.
address(_Bef,address([],[],[],[],[],[],[],[],[]),tel([],[]),[],_Fl,[]) --> [].

unamb_address(Bef,address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	['|'],!,
	unamb_address(Bef,
		address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth).
unamb_address(Bef,address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	num_street_addr(Bef,Street,Room,Bld,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth),!.
unamb_address(Bef,Addr,Tel,Attn,Fl,Oth) --> 
	non_num_unamb_address(Bef,Addr,Tel,Attn,Fl,Oth),!.

non_num_unamb_address(Bef,address(Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest),
	                        Tel,Attn,Fl,Oth) --> 
	telephone(Tel,Tel1),!,  % bug, no street after tel.
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,Tmp,Tmp,
				Tel1,Attn,Fl,Oth).
non_num_unamb_address(Bef,address(Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest),
	                        Tel,Attn,Fl,Oth) -->
	get_attention(Attn,Attn1),!,
	attn_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn1,Fl,Oth).
non_num_unamb_address(Bef,
		address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	non_num_room(Room,Room1),!,
        address_rsptsczr(Bef,Room1,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
% This may need to be extended for certain types of foreign addresses.
non_num_unamb_address(Bef,
		address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	non_num_building(Bld,Bld1),!,
        address_rsptsczr(Bef,Room,Bld1,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
non_num_unamb_address(Bef,
	address(Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	non_num_street(Bef,Street,Street1),!,
        address_rsptsczr(Bef,Room,Bld,Street1,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
non_num_unamb_address(Bef,address([],[],[],[],[],[],[],[],[]),
						tel([],[]),[],_Fl,Oth) --> 
	connector(Bef,Oth),!.
non_num_unamb_address(Bef,address(Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest),
							Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
        address_rsptsczr(Bef,Room,Bld,Str,_,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
non_num_unamb_address(Bef,address(R,Bld,Str,P,T,St,C,Z,Rest),
	                      Tel,Attn,Fl,Oth) --> 
	other(Rest,Rest1,Fl),
	address(Bef,address(R,Bld,Str,P,T,St,C,Z,Rest1),Tel,Attn,Fl,Oth).

address_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
						Tel,Attn,Fl,Oth) --> 
	opt_pipe,
	street_addr(Bef,Street,Room,Bld,Po,Town,State,Cntry,Zip,Rest,
						Tel,Attn,Fl,Oth),!.
address_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							Tel,Attn,Fl,Oth) --> 
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
	                    Tmp,Tmp,Tel,Attn,Fl,Oth).

address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
	address_rptsczr(Bef,Room,Bld,Str,_Po,Town,State,Cntry,Zip,Rst,
						T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
						Tel,Attn,Fl,Oth) --> 
	telephone(Tel,Tel1),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						T1,T2,Tel1,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
					Tel,Attn,Fl,Oth) --> 
	get_attention(Attn,Attn1),!,
	attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						T1,T2,Tel,Attn1,Fl,Oth),!.
address_rptsczr(Bef,[],[],[],[],Tmp,[],[],[],[],Tmp,[],
						tel([],[]),[],_Fl,Oth) --> 
	connector(Bef,Oth),!.
address_rptsczr(inflags(Fd,Be),Room,Bld,[],Po,[Town],State,Cntry,Zip,Tmp,Tmp,Rr,
							Tel,Attn,Fl,Oth) --> 
	city_prod(Fd,Town),!,
	address_rspczr(inflags(Fd,Be),Room,Bld,Po,State,Cntry,Zip,Rr,
							Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) -->
        [Des],
	{post_building_desig_no_fl(Des,Sp)},
	not_street_lookahead,!,
	building_switch(Bef,Sp,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) -->
        [Des],
	{post_room_desig_no_fl(Des,Sp)},!,
	room_switch(Bef,Sp,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	room(Room,Room1),!,
	address_rptsczr(Bef,Room1,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	building(Bld,Bld1),not_street_lookahead,!,
	address_rptsczr(Bef,Room,Bld1,Str,Po,Town,State,Cntry,Zip,Rest,
						T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,T1,Po,Town,State,Cntry,Zip,Rest,T1,Str,
							Tel,Attn,Fl,Oth) --> 
	street_d(Str,Str1),
	{\+var(T1)},!,
	address_rptsczr(Bef,Room,Bld,Str1,Po,Town,State,Cntry,Zip,Rest,
						T,T,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Rm,Bld,[],Po,T1,[St|Srest],Ctry,Zip,Rst,T1,[],
							Tel,Attn,Fl,Oth) --> 
	[State],
	{state_with_transition(State,St)},!,
	address_rspczr(Bef,Rm,Bld,Po,Srest,Ctry,Zip,Rst,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Rm,Bld,Str,Po,Town,[St|Srest],Ctry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	[State],
	{state(State,St)},!,
	address_rptsczr(Bef,Rm,Bld,Str,Po,Town,Srest,Ctry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,[Cn],Zip,Rest,T1,T2,
						Tel,Attn,Fl,Oth) --> 
	[Cntry],
	{country(Cntry,Cn)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,_Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	zip(Zip),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,_,Rest,
						T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	unamb_street(Street,Srest),!,
	street_desig_switch(Bef,Street,Srest,Room,Bld,Str,Po,Town,State,
				Cntry,Zip,Rst,T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Tmp,Tmp,[Subd1|Rr],
						Tel,Attn,Fl,Oth) --> 
        [Subd],
	{org_subdivision_desig(Subd,Subd1)},!,
	address_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rr,
							Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Twn,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	other(Rest,More,Fl),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Twn,State,Cntry,Zip,More,
						   T1,T2,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
			T1,[integer(Num)|T2],Tel,Attn,Fl,Oth) --> 
	[integer(Num)],!,
	integer_switch(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
						T1,T2,Tel,Attn,Fl,Oth).
% Note possible problem with sequences could be fixed with city table.
address_rptsczr(inflags('B','N'),[],[],[],[],[],[],[],[],[],Ent,[],
					tel([],[]),[],_fl,Oth) --> 
	addr_org_term_prim(Ent1,Terms,Title),!,
	{append(Ent,Ent1,Finent)},
	{Oth = ea(seq,entity(Finent,Terms,Rest),Addr,Tel,Attn,Fl,Oth1)},
	{create_flag_struct(Fl)},
	org_terminator(inflags('B','N'),_Rel,Title,_Nm,Rest,Addr,
							Tel,Attn,Fl,Oth1).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
					   Tmp,[Word|D],Tel,Attn,Fl,Oth) --> 
	[Word],
	{\+improper_town_word(Word)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
					   Tmp,D,Tel,Attn,Fl,Oth).
address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,[Word|Rest],
					   Tmp,Tmp1,Tel,Attn,Fl,Oth) --> 
	[Word],
	{improper_town_word(Word)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
					   Tmp,Tmp1,Tel,Attn,Fl,Oth).
address_rptsczr(_Bef,[],[],[],[],Tmp,[],[],[],[],Tmp,[],
						tel([],[]),[],_fl,[]) --> []. 

address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
	address_rspczr(Bef,Room,Bld,_Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,[Cn],Zip,Rest,Tel,Attn,Fl,Oth) --> 
	[Cntry],
	{country(Cntry,Cn)},!,
	address_rspczr(Bef,Room,Bld,Po,State,_,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,[St|Srest],Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	[State],
	{state(State,St)},!,
	address_rspczr(Bef,Room,Bld,Po,Srest,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	general_phone(Tel,Tel1),!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel1,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	get_attention(Attn,Attn1),!,
	attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn1,Fl,Oth),!.
address_rspczr(Bef,[],[],[],[],[],[],[],tel([],[]),[],_fl,Oth) --> 
	connector(Bef,Oth),!.
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	zip(Zip),!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,_,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	room(Room,Room1),!,
	address_rspczr(Bef,Room1,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	building(Bld,Bld1),!,
	address_rspczr(Bef,Room,Bld1,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,[Word|Rest],Tel,Attn,Fl,Oth) -->
	[Word],!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
address_rspczr(_Bef,[],[],[],[],[],[],[],tel([],[]),[],_Fl,[]) --> []. 

attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
	['|'],!,
	attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,
						Rest,Tel,Attn,Fl,Oth) -->
	pobox(Po),!,
	address_rsptsczr(Bef,Room,Bld,Street,_Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
 	telephone(Tel,Tel1),!,
	address_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel1,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) --> 
	attention,
	attn_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth),!.
attn_rsptsczr(Bef,[],[],[],[],[],[],[],[],[],tel([],[]),[],_fl,Oth) --> 
	connector(Bef,Oth),!.
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,[St],Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
	[State],{State \== 'MISS'},
	{state(State,St)},!,
	address_rsptsczr(Bef,Room,Bld,Street,Po,Town,_State,Cntry,Zip,Rest,
							   Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,[Cn],Zip,Rest,
							Tel,Attn,Fl,Oth) --> 
	[Cntry],
	{country(Cntry,Cn)},!,
	address_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,_Cntry,Zip,Rest,
							   Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
	zip(Zip),!,
	address_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,_,Rest,
							   Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
	room(Room,Room1),!,
	address_rsptsczr(Bef,Room1,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							   Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) -->
	building(Bld,Bld1),!,
	address_rsptsczr(Bef,Room,Bld1,Street,Po,Town,State,Cntry,Zip,Rest,
							   Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,[Town|Trest],State,Cntry,Zip,Rest,
	                         Tel,Attn,Fl,Oth) --> 
	[Town],{city(Town)},!,
	address_rsptsczr(Bef,Room,Bld,Street,Po,Trest,State,Cntry,Zip,Rest,
							  Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth) --> 
	[integer(Int)],!,
	street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,Rest,
					  [integer(Int)|D],D,Tel,Attn,Fl,Oth).
attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,Tel,
						[Word|Attn],Fl,Oth) --> 
	[Word],
	{\+improper_town_word(Word)},!,
	attn_rsptsczr(Bef,Room,Bld,Street,Po,Town,State,Cntry,Zip,Rest,
							Tel,Attn,Fl,Oth).
attn_rsptsczr(_Bef,[],[],[],[],[],[],[],[],[],tel([],[]),[],_fl,[]) --> []. 

attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	['|'],!,
	attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						 T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
	address_rptsczr(Bef,Room,Bld,Str,_Po,Town,State,Cntry,Zip,Rest,
						 T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	telephone(Tel,Tel1),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						 T1,T2,Tel1,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	attention,
	attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						 T1,T2,Tel,Attn,Fl,Oth),!.
attn_rptsczr(Bef,[],[],[],[],[],[],[],[],[],[],[],tel([],[]),[],_fl,Oth) --> 
	connector(Bef,Oth),!.
attn_rptsczr(Bef,Room,Bld,[],Po,T1,[St],Cntry,Zip,Rest,T1,[],
							Tel,Attn,Fl,Oth) --> 
	[State],{State \== 'MISS'},
	{state_with_transition(State,St)},!,
	address_rspczr(Bef,Room,Bld,Po,_State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,[St],Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	[State],{State \== 'MISS'},
	{state(State,St)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,_State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,[Cntry,Cn],Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	[Cntry],
	{country(Cntry,Cn)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,_Cntry,Zip,Rest,
						  T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	zip(Zip),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,_,Rest,T1,T2,
							Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	room(Room,Room1),!,
	address_rptsczr(Bef,Room1,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
						  T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	building(Bld,Bld1),!,
	address_rptsczr(Bef,Room,Bld1,Str,Po,Town,State,Cntry,Zip,Rest,
						  T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,[],Po,[Town],State,Cntry,Zip,T1,T1,Trst,
						Tel,Attn,Fl,Oth) --> 
	[Town],
	{city(Town)},!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Trst,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Twn,State,Cntry,Zip,Rest,T1,T2,
							Tel,Attn,Fl,Oth) --> 
	other(Rest,R,Fl),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Twn,State,Cntry,Zip,R,T1,T2,
							Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,[integer(Num)|T2],
					   Tel,Attn,Fl,Oth) --> 
	[integer(Num)],!,
	integer_switch(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
					   T1,T2,Tel,[Word|Attn],Fl,Oth) --> 
	[Word],!,
	attn_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rest,
					   T1,T2,Tel,Attn,Fl,Oth).
attn_rptsczr(_Bef,[],[],[],[],[],[],[],[],[],T1,[],
						tel([],[]),T1,_fl,[]) --> []. 

attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
	address_rspczr(Bef,Room,Bld,_Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	pobox(Po),!,
	address_rspczr(Bef,Room,Bld,_Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,[Cntry,Cn],Zip,Rest,Tel,Attn,Fl,Oth) --> 
	[Cntry],
	{country(Cntry,Cn)},!,
	address_rspczr(Bef,Room,Bld,Po,State,_,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,[St],Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	[State],
	{state(State,St)},!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	telephone(Tel,Tel1),!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel1,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	attention,!,
	attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth),!.
attn_rspczr(Bef,[],[],[],[],[],[],[],tel([],[]),[],_fl,Oth) --> 
	connector(Bef,Oth),!.
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	zip(Zip),!,
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,_,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	room(Room,Room1),!,
	address_rspczr(Bef,Room1,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth) --> 
	building(Bld,Bld1),!,
	address_rspczr(Bef,Room,Bld1,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,[Word|Attn],Fl,Oth) --> 
	[Word],!,
   	attn_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,Rest,Tel,Attn,Fl,Oth).
attn_rspczr(_Bef,[],[],[],[],[],[],[],tel([],[]),[],_fl,[]) --> []. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% POST OFFICE BOXES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pobox(['POB',Number|Rest]) -->
	pobox_1,!,
	box_desig(Number,Rest).
pobox(['GEN DELEVERY']) --> 
	['GENERAL'],['DELIVERY'],!.

pobox_1 --> ['POB'],opt(['BOX']),!.
pobox_1 --> ['PF'],!.
pobox_1 --> ['POST'],opt(['OFFICE']),opt(['BOX']),!.
pobox_1 --> ['BOX'],!.
pobox_1 --> ['CP'],!.
pobox_1 --> ['BP'],!.
pobox_1 --> ['BX'],!.
pobox_1 --> ['POSTAL'],!.
pobox_1 --> ['APARTADO'],opt(['POSTAL']),!.
pobox_1 --> ['B'],['P'],!.
pobox_1 --> ['G'],['P'],!.
pobox_1 --> ['P'],['O'],!.

box_desig(Number,Rest) -->
	general_number(Number),!,
	get_general_numbers(Rest).
box_desig(Number,[Number1|Rest]) -->
	[Number],general_number(Number1),!,
	get_general_numbers(Rest).

get_raw_street([ '|'|_List0],2,[]):-!.
get_raw_street(['|'|List0],N,Acc):-!,
	N1 is N + 1,
	get_raw_street(List0,N1,Acc).
get_raw_street([H|List0],2,[H|Acc]):-!,
	get_raw_street(List0,2,Acc).
get_raw_street([_H|List0],N,Acc):-
	get_raw_street(List0,N,Acc).

get_numbers([Num|Rest]) -->
	number(Num),!,
	get_numbers(Rest).
get_numbers([]) --> [].

get_numbers([Num|Rest],Hook) -->
	number(Num),!,
	get_numbers(Rest,Hook).
get_numbers(Rest,Rest) --> [].

get_general_numbers([Num|Rest]) -->
	general_number(Num),!,
	get_general_numbers(Rest).
get_general_numbers([]) --> [].

get_general_numbers([Num|Rest],Hook) -->
	general_number(Num),!,
	get_general_numbers(Rest,Hook).
get_general_numbers(Rest,Rest) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phones/Faxes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

general_phone(Phone,Ptr) -->
	telephone(Phone,Ptr),!.
general_phone(tel([N1|Trest],Fax),tel(Tel,Fax)) -->
	opt(['(']),
	number(N1),
	{N1 = integer(N),area_code(N,_)},
	telephone_1(Trest,Tel),!.

telephone(tel([N1|Trest],Fax),tel(Tel,Fax)) -->
	['AT'],opt(['|']),
	number(N1),
	{N1 = integer(N),area_code(N,_)},
	telephone_1(Trest,Tel),!.
telephone(tel([N1|Trest],Fax),tel(Tel,Fax)) -->
	['TELEPHONE'],opt(['|']),
	opt(['(']),
	number(N1),
	telephone_1(Trest,Tel),!.
telephone(tel(Tel,[N1|Trest]),tel(Tel,Fax)) -->
	['FAX'],opt(['|']),
	opt(['(']),
	number(N1),
	telephone_1(Trest,Fax),!.
	
telephone_1([Num|Rest],Ptr) --> 
	opt([')']),
	number(Num),!,
	telephone_1(Rest,Ptr).
telephone_1(Ptr,Ptr) --> [].	

