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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Zips
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zip([integer(Zip)]) -->
	(opt(['CP']) ; opt(['CEP']) ; opt(['SF'])),
	[integer(Zip0)],
	{atom_chars(Zip0,Chars)},
	zip0(Zip0,Chars,Zip),!.
zip([Zip]) -->
	[L1],{single_letter(L1)},
	[integer(I1)],{name(I1,[_Int])},
	[L2],{single_letter(L2)},
	[integer(I2)],{name(I2,[_Int2])},
	[L3], {single_letter(L3)},
	[integer(I3)],	{name(I3,[_Int3])},
	{concat_atom([L1,I1,L2],'',Zip1),
	 concat_atom([I2,L3,I3],'',Zip2),
     concat_atom([Zip1,Zip2],'',Zip)}.

zip0(_Zip0,Chars,'')-->
	{Chars = [48,48,48,48,48,48,48,48,48]},!.
zip0(_Zip0,Chars,'')-->
	{Chars = [48,48,48,48,48]},!.
zip0(Zip0,Chars,Zip)-->
	{Chars = [_A,_B,_C,_D,E],E < 65},
	{no_hyphens(Chars)},
	(zip1(Zip1),{concat_atom([Zip0,Zip1],'',Zip)},!
	     ; {Zip = Zip0}).
zip0(_Zip0,Chars,Zip1)-->
	{Chars = [A,B,C,D,E,48,48,48,48]},!,
	{no_hyphens(Chars)},
	{atom_chars(Zip1,[A,B,C,D,E])}.
zip0(_Zip0,Chars,Zip1)-->
	{Chars = [A,B,C,D,E,F,G,H,I],I < 65},!,
	{no_hyphens(Chars)},
	{atom_chars(Zip1,[A,B,C,D,E,F,G,H,I])}.
zip0(_Zip0,Chars,Zip1)-->
        {Chars = [A,B,C,D,E,F],F < 65},     % Indian Zips
	{no_hyphens(Chars)},
	{atom_chars(Zip1,[A,B,C,D,E,F])},!.

no_hyphens([]).
no_hyphens([H|T]):-
	H \== 45,
	no_hyphens(T).

zip1(Zip) --> 
	[integer(Zip)],
	{atom_chars(Zip,Chars),no_hyphens(Chars),length(Chars,4)}.

shipord(['S/O',Num|List],Tail) --> ['S/O'],number(Num),get_numbers(List,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attention
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_attention(Attn,Rest) -->
	attention,
	get_attention_1(Attn,Rest).
	
get_attention_1([Word1,Word2|Rest],Rest) -->
	title,!,
	[Word1],[Word2].
get_attention_1(['REFERENT'|Rest],Rest) -->
	attn_referent,!.
get_attention_1([Word1|Att_Rest],Rest) -->
	[Word1],
	{\+ org_term1(Word1)},{\+org_term(Word1)},
	get_second_word(Att_Rest,Rest),!.
get_attention_1([],[]) --> [].

strict_get_attention(Attn,Rest) -->
	attention,
	strict_get_attention_1(Attn,Rest).
	
strict_get_attention_1([Word1,Word2|Rest],Rest) -->
	title,!,
	[Word1],[Word2].
strict_get_attention_1(['REFERENT'|Rest],Rest) -->
	attn_referent,!.
strict_get_attention_1([Word1|Att_Rest],Rest) -->
	[Word1],
	{\+ org_term1(Word1)},{\+org_term(Word1)},
	get_second_word(Att_Rest,Rest),!.

get_second_word([Word2|Rest],Rest) -->
	[Word2],
	{\+(Word2 = integer(_))},
	!.
get_second_word(Rest,Rest) -->	[].

attn_referent -->
	['PERSON'],['IN'],['CHARGE'].

attention --> ['ATTENTION'],!.
attention --> ['ATT'],!.
attention --> ['ATTEN'],!.
attention --> ['ATN'],!.
attention --> ['CTC'],!.
attention --> ['CONTACT'],!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Connectors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

and_connector(inflags(Fd,_),ea(seq,entity(Ent,Title,Rest),
			Addr,Tel,Attn,Fl,Oth)) -->
	['&'],!,
	entity(inflags(Fd,'N'),ea(seq,entity(Ent,Title,Rest),
			Addr,Tel,Attn,Fl,Oth)).
and_connector(Bef,EA) -->
	connector(Bef,EA).

connector(inflags(Fd,_),ea(ob,entity(Ent,Title,Rest),
				Addr,Tel,Attn,Fl,Oth)) -->
	on_behalf,!,
  	ob_entity(inflags(Fd,'N'),ea(ob,entity(Ent,Title,Rest),
				Addr,Tel,Attn,Fl,Oth)).
connector('B',ea(ob,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	['FOR'],!,
  	ob_entity('B',ea(ob,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)).
connector(inflags(Fd,_),ea(Rel,entity(Ent,Title,Rest),
				Addr,Tel,Attn,Fl,Oth)) -->
	[Word],{connector_rel(Word,Rel)},!,
	entity(inflags(Fd,'N'),ea(co,entity(Ent,Title,Rest),
				Addr,Tel,Attn,Fl,Oth)).
connector(_,[]) -->
	['&'],subject,!.
connector(Bef,Ea) -->
	order_of(Bef,Ea),
	{arg(1,Ea,seq)}.

connector_rel('CAREOF',co).
connector_rel('A/C',ac).
connector_rel('ALSO',seq).
connector_rel('2)',seq).

on_behalf --> ['$OB'],!.
on_behalf --> ['DBA'],!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Org Subdivisions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

office([Off,'OFF'|R],R) -->
	[Off],
	{office_adj(Off)},
	['OFFICE'],!.
office(Off,R) --> office_of(Off,R).

office_of(['OFF OF',Off|R],R) -->
	['OFFICE OF'],
	[Off].

office_adj('AIRFREIGHT').
office_adj('BRANCH').
office_adj('BUYING').
office_adj('CARGO').
office_adj('CORPORATE').
office_adj('LIASION').
office_adj('LOCAL').
office_adj('NATIONAL').
office_adj('REGIONAL').
office_adj('SALES').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entity Terminators and Prefixes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

special_org_name(ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	uscs_district(ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)),
	!.
special_org_name(ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	state_factory(ea(Rel,entity(Name,Title,Rest),Addr,Tel,Attn,Fl,Oth)).
special_org_name(ea(Rel,entity(['1ST','AVENUE'|Nrest],Title,Rest),
						Addr,Tel,Attn,Fl,Oth)) -->
	[cardinal('1ST')],['AVE'],
	org_name_1(inflags('N','N'),Rel,Nrest,Title,Addr,Rest,Tel,Attn,Fl,Oth).
	
uscs_district(ea(Rel,entity(['USCS','DISTRICT',integer(Num),Word],[],Rest),
						Addr,Tel,Attn,Fl,Oth)) -->
	['USCS'],['DISTRICT'],
	[integer(Num)],[Word],
	{city(Word)},opt_pipe,
	org_terminator_continuation(inflags('N','N'),Rel,Rest,
			Addr,Tel,Attn,Fl,Oth).

state_factory(ea(Rel,entity([City,Num|Factory],Title,Rest),
						Addr,Tel,Attn,Fl,Oth)) -->
	[City],{state_factory_city(City)},
	opt(['|']),
	card_int_num(Num),
	opt(['|']),
	factory(Factory),
	get_org_term(Title),
	org_terminator_continuation(inflags('N','N'),Rel,Rest,
			Addr,Tel,Attn,Fl,Oth),!.
state_factory(ea(Rel,entity(['THE',Num|Factory],[],Rest),
						Addr,Tel,Attn,Fl,Oth)) -->
	dem_art,
	opt(['|']),
	[cardinal(Num)],
	opt(['|']),
	factory(Factory),
	org_terminator_continuation(inflags('N','N'),Rel,Rest,
			Addr,Tel,Attn,Fl,Oth).

factory([Product,Fty1]) -->
	[Product],[Factory],
	{factory1(Factory,Fty1)},!.
factory([Product,Product1,Fty1]) -->
	[Product],[Product1],[Factory],
	{factory1(Factory,Fty1)},!.
factory([Product,Product1,Fty1]) -->
	[Product],[Product1],['|'],[Factory],
	{factory1(Factory,Fty1)},!.
factory([Product,Product1,Product2,Fty1]) -->
	[Product],[Product1],[Product2],[Factory],
	{factory1(Factory,Fty1)},!.
factory(Products) -->
	get_products(Products,[Fty1]),
	[Hook],
	{factory1(Hook,Fty1)}.

product('CLOTHING').
product('COTTON').
product('DYEING').
product('GLOVE').
product('JUTE').
product('KNITTING').
product('PRINTING').
product('RAMIE').
product('TEXTILE').

% change is to avoid stripping in post proc.
factory1('FAC','FACTORY').
factory1('FCTY','FACTORY').
factory1('FTY','FACTORY').
factory1('FACTORY','FACTORY').
factory1('MILL','MILL').
factory1('MNFR','MNFR').
factory1('STORE','STORE').
factory1('WORKS','FACTORY').

state_factory_city(City):-
	fcity(City,'CN').

get_products([Num|Rest],Hook) -->
	[Num],
	{product(Num)},!,
	opt(['&']),
	get_products(Rest,Hook).
get_products(Rest,Rest) --> [].

opt_title --> title,!.
opt_title --> [].

title --> ['DR'],!.
title --> ['LIC'],!.
title --> ['MISS'],!.
title --> ['MR'],!.
title --> ['MRS'],!.
title --> ['MS'],!.
title --> ['SR'],!.

org_preamble(inflags(_,'B'),['NOTIFY'],_Fl) --> notify,!,opt(['APPLICANT']).
org_preamble(_,Pre,Fl) --> 
	org_preamble(Pre,Fl).

org_preamble(['CAREOF'],_Fl) --> ['CAREOF'],!.
org_preamble(['CLEARING','AGENT'],_Fl) -->
	['CLEARING'],['AGENT'],!.
org_preamble(['CNEE'],Fl) --> ['CNEE'],!,{set_cgn_flag(Fl)}.
org_preamble(['FOR'],_Fl) --> ['FOR'],!.
org_preamble(['FOB'],_Fl) --> ['FOB'],!.
org_preamble(['MESSRS'],_Fl) --> messrs,!.
org_preamble(['MS'],_Fl) --> ['MS'],!.
org_preamble(['NVOCC'],Fl) --> ['NVOCC'],!,{set_cnf_flag(Fl)}.
org_preamble(['PT'],Fl) --> ['PT'],!,!,{set_org_type_flag(Fl)}.
org_preamble([integer(F)],_Fl) --> [integer(F)],{(F == '1' ; F == '2')},!.
		% need a better way to handle these sequences -- tls.
org_preamble([],_Fl) --> [].

org_sequencer('1)',_Fl) --> ['1)'],!.
org_sequencer('2)',_Fl) --> ['2)'],!.

messrs --> ['MESSRS'],!.
messrs --> ['MESSERS'],!.

org_term_prim_general(['CHB'|Nrest1],Nrest,['CHB'|Rest1],
			Rest,Nrest,Nrest,_fl) -->
	[Word],{custom_broker(Word)},
	org_term_prim_general_1(Nrest1,Rest1,Nrest,Rest),
	!.
org_term_prim_general(['OF',Term|Nrest],Nrest,
	                         ['OF',Term|Rest],Rest,Nrest,Nrest,_fl) --> 
	['OF'],
	[Term],{org_term_country(Term)},!.
org_term_prim_general(Off,Rest,Var,Var,Nrest,Nrest,_fl) -->
	office_of(Off,Rest),!.
org_term_prim_general(Var,Var,Terms,Rest,Nrest,Nrestrest,Fl) --> 
	org_term_prim(Terms,Rest,Nrest,Nrestrest,Fl),!.

org_term_prim([Term|Rest],Rest,Nrest,Nrest,_fl) --> 
	[Term],{org_term(Term)},!.
org_term_prim(['&',Term|Rest],Rest,Nrest,Nrest,_fl) --> 
	['&'],opt_pipe,[Term],{org_term(Term)},!.
org_term_prim(['OF',Term|Rest],Rest,Nrest,Nrest,_fl) --> 
	['OF'],
	[Term],{org_term_country(Term)},!.
org_term_prim(Title,Title,Nrest,Nrestrest,Fl) --> 
	other(Nrest,Nrestrest,Fl),!.

org_term_prim_general_1(['&','FWD'|Nrest],['&','FWD'|Rest],
							Nrest,Rest) --> 
	['&'],
	[Word],{ff_desig(Word)},!.
org_term_prim_general_1(Nrest,Rest,Nrest,Rest) --> [].

org_term_country('AMERICA'):-!.
org_term_country(Term):- country(Term,_).

custom_broker('CHB').

org_term1('ASSOC').
org_term1('CO').
org_term1('SON').

limited --> ['LTD'],!.
limited --> ['L'].

% tls -- needs better fix.
addr_org_term_prim(['CHB'],Rest,Rest) --> 
	['CHB'],!.
addr_org_term_prim([],[Term|Rest],Rest) --> 
	[Term],
	{org_term(Term),\+ ambiguous_org_term(Term)},
	!.
addr_org_term_prim([],['&',Term|Rest],Rest) --> 
	['&'],[Term],{org_term(Term),Term \== 'CO'},!.

ambiguous_org_term('AB'). % Alberta
ambiguous_org_term('AG'). % Antigua
ambiguous_org_term('CO'). 
ambiguous_org_term('NA'). % Netherlands Antilles
ambiguous_org_term('NP'). % Nepal
ambiguous_org_term('NV'). % Nevada
ambiguous_org_term('PT'). % Portugal
ambiguous_org_term('SA'). % SAUDI Arabia

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
city_prod(Fd,Out1) -->
	[City],
	{city(Fd,City)},
	{\+city_block(City)},
	rm_str_check(City),!,
	city2(Rest),
	{ca([City|Rest],Out1)}.
city_prod(Fd,Out1) -->
	[Init],
	{city_initiator(Init,In)},
	city1(Fd,Rest),
	{ca([In|Rest],Out1)}.

city1(Fd,[City|Rest]) -->
	[City],
	{city(Fd,City)},
	rm_str_check(City),!,
	city2(Rest).
city1(_Fd,[Tm|Rest]) -->
	[Term],
	{city_terminator(Term,Tm)},
	not_rm_street_lookahead,!,	
	city2(Rest).


city_prod(Out1) -->
	[City],
	{city(City)},
	{\+city_block(City)},
	rm_str_check(City),!,
	city2(Rest),
	{ca([City|Rest],Out1)}.
city_prod(Out1) -->
	[Init],
	{city_initiator(Init,In)},
	city1(Rest),
	{ca([In|Rest],Out1)}.

city1([City|Rest]) -->
	[City],
	{city(City)},
	rm_str_check(City),!,
	city2(Rest).
city1([Tm|Rest]) -->
	[Term],
	{city_terminator(Term,Tm)},
	not_rm_street_lookahead,!,	
	city2(Rest).

not_street_lookahead --> street_d(_Word),!,{fail}.
not_street_lookahead --> [].

not_rm_street_lookahead --> street_d(_Word),!,{fail}.
not_rm_street_lookahead --> [Word],
	{post_room_or_building_desig(Word,_),Word \== 'FL'},!,{fail}.
not_rm_street_lookahead --> [].

rm_str_check(_,[],[]).
rm_str_check(Old,['FL'|Rest],['FLORIDA'|Rest]):-!,
	z(Old,'FL',_Zip).
rm_str_check(_,[Word|Rest],[Word|Rest]):-
	\+ post_room_or_building_desig(Word,_),
	\+ street_desig(Word,_).

city2([Tm|Rest]) -->
	[Term],
	{city_terminator(Term,Tm)},!,
	not_rm_street_lookahead,	
	city2(Rest).
city2([]) --> not_rm_street_lookahead.

city_block('CENTER').
city_block('STREET').
city_block('PLAZA').
city_block('VALLEY').

improper_town_word('|').
improper_town_word(')').
improper_town_word('(').
improper_town_word('FN').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Streets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
post_room_switch(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Desig,Tel,Attn,Fl,Oth) -->
	street_d(Desig,Str),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,R,
	                   Temp,Temp,Tel,Attn,Fl,Oth).
post_room_switch(Bef,Str,Tmp,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Rm,Tel,Attn,Fl,Oth) -->
	address_rsptsczr(Bef,Rm,Bld,Str,Po,Town,State,Cntry,Zip,R,
			Tel,Attn,Fl,Oth).

post_building_switch(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Desig,Tel,Attn,Fl,Oth) -->
	street_d(Desig,Str),!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,R,
	                   Temp,Temp,Tel,Attn,Fl,Oth).
post_building_switch(Bef,Str,Rm,Tmp,Po,Town,State,Cntry,Zip,R,
					   Tmp,Bld,Tel,Attn,Fl,Oth) -->
	address_rsptsczr(Bef,Rm,Bld,Str,Po,Town,State,Cntry,Zip,R,
						Tel,Attn,Fl,Oth).

street_addr(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,Tel,Attn,Fl,Oth) -->
	number(Num),!,
	street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
						  [Num|D],D,Tel,Attn,Fl,Oth).
street_addr(Bef,Street,Room,Bld,Po,Town,State,Cntry,Zip,R,Tel,Attn,Fl,Oth) -->
	non_num_street(Bef,Street,[]),!,
	address_rptsczr(Bef,Room,Bld,_Str,Po,Town,State,Cntry,Zip,R,Tmp,Tmp,
							Tel,Attn,Fl,Oth).

num_street_addr(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,Tel,Attn,Fl,Oth) -->
	card_int_num(Num),!,
	street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
						  [Num|D],D,Tel,Attn,Fl,Oth).

street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Desig,Tel,Attn,Fl,Oth) -->
	street_d(Desig,Str_rest),
	address_rptsczr(Bef,Room,Bld,Str_rest,Po,Town,State,Cntry,Zip,R,
	                   Temp,Temp,Tel,Attn,Fl,Oth),!.
street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,[Sp|D],Tel,Attn,Fl,Oth) -->
	[Desig],
	{ed_dis_post_room_desig(Desig,Sp)},!,
	post_room_switch(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,D,Tel,Attn,Fl,Oth).
street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,[Sp|D],Tel,Attn,Fl,Oth) -->
	[Desig],
	{ed_dis_post_building_desig(Desig,Sp)},
	not_street_lookahead,!,
	post_building_switch(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,D,Tel,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
						Tmp,[],Tel,Attn,Fl,Oth) -->
	room(Room,Room1),!,
	address_rptsczr(Bef,Room1,Bld,_Str,Po,Town,State,Cntry,Zip,R,Temp,Temp,
							Tel,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
						Tmp,[],Tel,Attn,Fl,Oth) -->
	building(Bld,Bld1),!,
	address_rptsczr(Bef,Room,Bld1,_Str,Po,Town,State,Cntry,Zip,R,Temp,Temp,
							Tel,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R1,Tmp,Trest,
							T1,Attn,Fl,Oth) -->
	pobox(Po),!,
	address_rptsczr(Bef,Room,Bld,Trest,_,Town,State,Cntry,Zip,R1,Temp,Temp,
							T1,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R1,Tmp,[],
							T1,Attn,Fl,Oth) -->
	zip(Zip),
	!,
	address_rptsczr(Bef,Room,Bld,_Str,Po,Town,State,Cntry,_,R1,Temp,Temp,
							T1,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R1,Tmp,[],
							T1,Attn,Fl,Oth) -->
	telephone(T1,T2),!,
	address_rptsczr(Bef,Room,Bld,_Str,Po,Town,State,Cntry,Zip,R1,Temp,Temp,
							T2,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Route,Tel,Attn,Fl,Oth) -->
	route(Route,Trest),
	address_rptsczr(Bef,Room,Bld,Trest,Po,Town,State,Cntry,Zip,R,
	                   Temp,Temp,Tel,Attn,Fl,Oth),!.
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R1,Tmp,[],
							Tel,Attn,Fl,Oth) -->
	attention,!,
	attn_rptsczr(Bef,Room,Bld,_Str,Po,Town,State,Cntry,Zip,R1,T1,T1,
							Tel,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R1,Tmp,[],
							Tel,Attn,Fl,Oth) -->
	other(R1,R2,Fl),!,
	address_rptsczr(Bef,Room,Bld,_Str,Po,Town,State,Cntry,Zip,R2,T1,T1,
							Tel,Attn,Fl,Oth).
street_addr_1(Bef,Tmp,[],[],[],[],[],[],[],[],Tmp,[],
					tel([],[]),[],_fl,Oth) -->
	connector(Bef,Oth),!.
street_addr_1(inflags('F',Ent),Tmp,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,Str_rest,Tel,Attn,Fl,Oth) -->
	['|'],
	{(Ent = 'N' ; Ent = 'E')},!,
	address_rptsczr(inflags('F','N'),Room,Bld,Str_rest,Po,Town,
		State,Cntry,Zip,R,Temp,Temp,Tel,Attn,Fl,Oth),!.
street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
					   Tmp,[Word|D],Tel,Attn,Fl,Oth) -->
	[Word],!,
	street_addr_1(Bef,Str,Room,Bld,Po,Town,State,Cntry,Zip,R,
						    Tmp,D,Tel,Attn,Fl,Oth).
street_addr_1(_Bef,Tmp,[],[],[],[],[],[],[],[],Tmp,[],tel([],[]),[],_fl,[]) -->
	[].

non_num_street(_Bef,Street,Rest) --> 
	route(Street,Rest),!.
non_num_street(Bef,[Out|Srest],Street1) -->
	[Desig],
	{non_num_street_desig(Desig,Out)},!,
	non_num_street_1(Bef,Srest,Street1).

non_num_street_1(Bef,[Num|Nums],Rest) -->
	number(Num),
	get_general_numbers(Nums,Rest11),
	non_num_street_2(Bef,Rest11,Rest1),
	!,
	kilometer(Rest1,Rest).
non_num_street_1(Bef,Rest,Rest) -->
	['|'],
	{Bef = inflags(_,'E')},!.
non_num_street_1(Bef,[Word|Srest],Street1) -->
	[Word],!,
	non_num_street_1(Bef,Srest,Street1).
non_num_street_1(_,[],[]) --> [].


non_num_street_2(_Bef,Month,Rest) -->
	romance_month_prod(Month,Rest1),
	!,
	kilometer(Rest1,Rest).

romance_month_prod(['DE',Month|Rest],Rest) --> 
	['DE'],
	[Month],{romance_month(Month)},!.
romance_month_prod([Month|Rest],Rest) --> 
	[Month],{romance_month(Month)},!.
romance_month_prod(Rest,Rest) --> [].


kilometer(['KM'|Nums],Rest) --> 
	['KLM'],!,
	get_numbers(Nums,Rest).
kilometer([],[]) --> [].

res_street_d([Sp|Rest],street(_Num,_Name,Desig)) -->
	[Desig],
	{street_desig(Desig,Sp)},
	street_d_1(Rest).

street_d([Sp|Rest]) -->
	[Desig],
	{street_desig(Desig,Sp)},
	street_d_1(Rest).

street_d([Sp|Rest],Handle) -->
	[Desig],
	{street_desig(Desig,Sp)},
	street_d_1(Rest,Handle).

street_d_1([Sp|Rest],Handle) --> 
	[Desig],
	{narrow_street_desig(Desig,Sp)},!,
	street_d_1(Rest,Handle).
street_d_1([Desig|Rest],Handle) --> 
	[Desig],
	{quadrent(Desig)},!,
	street_d_1(Rest,Handle).
street_d_1(Handle,Handle) --> [].

street_d_1([Sp|Rest]) --> 
	[Desig],
	{narrow_street_desig(Desig,Sp)},!,
	street_d_1(Rest).
street_d_1([Desig|Rest]) --> 
	[Desig],
	{quadrent(Desig)},!,
	street_d_1(Rest).
street_d_1([]) --> [].

narrow_street_desig(D,Sp):- street_desig(D,Sp),\+street(D,Sp).

unamb_street([Sp|Rest],Rest) -->
	[Desig],
	{unamb_street_desig(Desig,Sp)},!.
unamb_street(Sp,Rest) -->
	route(Sp,Rest).

unamb_street_desig(D,Sp):- 
	street_desig(D,Sp),
	D \== 'ST',
	D \== 'STE'.

quadrent('NE').
quadrent('NW').
quadrent('SE').
quadrent('SW').

street('STREET','ST').
street('STREETS','STREETS').
street('STS','STREETS').
street('ST','ST').
street('STR','ST').

res_route([Out,Num|Rest],Rest,street(Num,[Out,Num],_Desc)) -->
	[Word],	
	{route_desig(Word,Out)},
	number(Num).

route([Out|Rest],Handle) -->
	[Word],	
	{route_desig(Word,Out)},
	route_number(Rest,Handle).
 
route_number([Num|Rest],Handle) --> number(Num),route_number1(Rest,Handle).

route_number1(['&',Num|Handle],Handle) --> ['&'],number(Num),!.
route_number1(Handle,Handle) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Buildings and Rooms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
second_entity(Bef,ea(seq,entity([Word|Name],Title,[]),Addr,
						Tel,Attn,Fl,Oth)) --> 
	[Word],
	get_org_name1(Name,Title),
	{create_flag_struct(Fl)},
	address(Bef,Addr,Tel,Attn,Fl,Oth),!.

get_org_name1([],[Term|Rest]) -->
	opt(['&']),[Term],
	{org_term(Term)},!,
	get_org_term(Rest).
get_org_name1([Term|Rest],Title) -->
	[Term],
	get_org_name2(Rest,Title).

get_org_name2([],[Term|Rest]) -->
	opt(['&']),[Term],
	{org_term(Term)},!,
	get_org_term(Rest).
get_org_name2([Term|Rest],Title) -->
	[Term],
	get_org_name3(Rest,Title).

get_org_name3([],[Term|Rest]) -->
	opt(['&']),[Term],
	{org_term(Term)},
	get_org_term(Rest).

get_org_term([Term|R]) --> 
	[Term],{org_term(Term)},!,
	get_org_term(R).
get_org_term([Term|R]) --> 
	['&'],[Term],{org_term(Term)},!,
	get_org_term(R).
get_org_term([]) --> [].

building_switch(Bef,Des,Room,[Des,Word|Bld1],Str,Po,Town,State,Cntry,Zip,Rst,
						[],[],Tel,Attn,Fl,Oth) -->
	opt(['|']),[Word],not_street_lookahead,!,
	address_rptsczr(Bef,Room,Bld1,Str,Po,Town,State,Cntry,Zip,Rst,
							T,T,Tel,Attn,Fl,Oth).
building_switch(Bef,Des,Room,Tmp,Str,Po,Town,State,Cntry,Zip,Rst,
                                          Tmp,[Des|T2],Tel,Attn,Fl,Oth)-->
	address_rsptsczr(Bef,Room,T2,Str,Po,Town,State,Cntry,Zip,Rst,
							Tel,Attn,Fl,Oth).

room_switch(Bef,Des,[Des,Word|Room1],Bld,Str,Po,Town,State,Cntry,Zip,Rst,
						[],[],Tel,Attn,Fl,Oth) -->
	opt(['|']),[Word],!,
	address_rptsczr(Bef,Room1,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							T,T,Tel,Attn,Fl,Oth).
room_switch(Bef,Des,Tmp,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
                                          Tmp,[Des|T2],Tel,Attn,Fl,Oth) -->
	address_rsptsczr(Bef,T2,Bld,Str,Po,Town,State,Cntry,Zip,Rst,
							Tel,Attn,Fl,Oth).

integer_switch(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth) -->
	{var(Town)},!,
	address_rptsczr(Bef,Room,Bld,Str,Po,Town,State,Cntry,Zip,Rst,T1,T2,
							Tel,Attn,Fl,Oth).
integer_switch(Bef,Room,Bld,_Str,Po,_Town,State,Cntry,Zip,T1,T1,T2,
							Tel,Attn,Fl,Oth) -->
	address_rspczr(Bef,Room,Bld,Po,State,Cntry,Zip,T2,Tel,Attn,Fl,Oth).

street_desig_switch(Bef,Des,Srest,Rm,Bld,Tmp,Po,Town,State,Cntry,Zip,Rst,
						Tmp,Des,Tel,Attn,Fl,Oth) -->
	address_rptsczr(Bef,Rm,Bld,Srest,Po,Town,State,Cntry,Zip,Rst,T,T,
							Tel,Attn,Fl,Oth).

room(Room,Rest) -->
	non_num_room(Room,Rest),!.
room([Num,Out|Rest],Rest) -->
	card_int_num(Num),
	[Desig],
	{floor(Desig,Out)},!.
room(['MAIN FLOOR'|Rest],Rest) -->
	['MAIN FLOOR'].

non_num_room([Spelled|Rest],More) --> 
	[Descr],
	{pre_room(Descr,Spelled)},!,
	get_room_number(Rest,More).
non_num_room([Spelled|Rest],More) --> 
	[Descr],
	{multiple_pre_room(Descr,Spelled)},
	get_numbers(Rest,More).

building(Building,Rest) -->
	non_num_building(Building,Rest).

non_num_building([Spelled|Rest],More) --> 
	[Descr],
	{pre_building(Descr,Spelled)},
	get_room_number(Rest,More),!.
non_num_building([Spelled|Rest],More) --> 
	[Descr],
	{pre_named_building(Descr,Spelled)},!,
	named_building_1(Rest,More).
non_num_building([Spelled|Rest],More) --> 
	[Descr],
	{multiple_pre_building(Descr,Spelled)},
	get_numbers(Rest,More).

named_building_1([Num|Nums],Rest) -->
	number(Num),
	get_numbers(Nums,Rest),!.
named_building_1([Word|Srest],Street1) -->
	[Word],!,
	named_building_1(Srest,Street1).
named_building_1([],[]) --> [].

get_room_number(Rest,More) --> 
	['|'],!,
	get_room_number(Rest,More),!.
get_room_number([Num|Rest],More) --> 
	number(Num),
	get_room_number_num(Rest,More),!.	
get_room_number([Letter|Rest],More) --> 
	[Letter],
	{single_letter(Letter)},
	get_room_number_let(Rest,More),!.	

get_room_number_let([Num|More],More) -->
	number(Num),!.       
get_room_number_let(More,More) --> [].

get_room_number_num([Let|More],More) -->
	[Let],
	{single_letter(Let)},!.       
get_room_number_num(More,More) --> [].

ed_dis_post_room_desig(Desig,Sp):-
	post_room_desig(Desig,Sp),!.

ed_dis_post_building_desig(Desig,Sp):-
	post_building_desig_no_fl(Desig,Sp),!.

/*
ed_dis_post_room_or_building_desig(Word,Sp):-
        functor(Word,_,0),
	post_room_or_building_desig(Desig,Sp),
        min_ed_dis(Word,Desig,N),N < 2,
	\+keyword(Word). */

org_subdivision_desig('BRANCH','BRANCH').
org_subdivision_desig('DEPARTMENT','DEPT').
org_subdivision_desig('DIVISION','DIV').
org_subdivision_desig('DIV','DIV').
org_subdivision_desig('OFFICE','OFFICE').
org_subdivision_desig('REGION','REGION').

init_subdivision_desig('A DIV OF').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Referents and other filler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

referent(Bef,Rest,Addr,Attn,Tel,Fl,Oth) --> 
	opt_pipe,
	referent_on1(Bef,Rest,Addr,Attn,Tel,Fl,Oth).
referent(Bef,['SHIPPER'],Addr,Attn,Tel,Fl,Oth) --> 
	opt_pipe,
	shipper_variants,!,
        {set_shiprs_flag(Fl)},
	address(Bef,Addr,Attn,Tel,Fl,Oth).

referent_on1(Bef,Rest,Addr,Attn,Tel,Fl,Oth) --> 
	ref_1(Rest,Fl),!,
	address(Bef,Addr,Attn,Tel,Fl,Oth).

other_ref(['REFERENT'|T],T,Fl) -->
	ref_1(_,Fl).

ref_1(['REFERENT'],Fl) --> 
	['AS'],
	as_referent(Fl),!.
ref_1(Various,Fl) --> 
	['VARIOUS'],
	ref_1_various(Various,Fl),!.
ref_1(['REFERENT'],Fl) --> 
	opt(['(']),
	opt_please,
        opt(['MEMO']),
	['SEE'],
	ref_2(Fl),!.
ref_1(['ACCOUNTEE'],_Fl) --> 
	['ACCOUNTEE'],!.
ref_1(['TEST','FILER'],Fl) -->
	tester(Fl),!.
ref_1(['REFERENT'],Fl) -->
	ref_2(Fl),!.
ref_1([],Fl) --> 
	same_as(Fl),
	{set_same_as_flag(Fl)}.

ref_1_various(['VARIOUS'],Fl) --> 
	['AS'],
	opt(['ATTACHED']),
	as_referent(Fl),!.
ref_1_various(['VARIOUS'],_Fl) --> 
	['VENDORS'],!.
ref_1_various(['VARIOUS',Word,'BROKE'],_Fl) --> 
	([Word]; []),
	['BROKER'],!.

ref_2(_Fl) -->
	restrict_attachment,!.
ref_2(_Fl) -->
	['ATTACHED'],
	opt_attachment,!.
ref_2(_Fl) --> ['ATTACHMENT'].
ref_2(_Fl) --> opt(['BL']),
	['REMARKS'],!,
	opt_ref_3.
ref_2(Fl) --> ['LINE'],['RELEASE'],city_prod(_),{set_bogus_flag(Fl)},!.
ref_2(Fl) --> ['LIQS'],['NOT'],['GRPED'],
		['BY'],['NEW'],['FILER'],['CODE'],{set_bogus_flag(Fl)},!.
ref_2(Fl) --> ['MAIL'],['ENTRY'],city_prod(_),{set_bogus_flag(Fl)},!.
ref_2(__Fl) --> ['NOT'],['PERMITTED'],['IN'],['DISTRICT'],[integer(_)],!.
ref_2(_Fl) --> ['NOTATION'],opt_ref_3.

opt_ref_3 --> ref_3,!.
opt_ref_3 --> [].

ref_3 --> ['FOR'],['COMPLETE'],['LIST'],['OF'],['VENDORS'],!.
ref_3 --> ['BELOW'].

opt_please --> please,!.
opt_please --> [].

please --> ['PLEASE'],!.
please --> ['PLS'],!.
please --> ['PLSE'],!.

as_referent(Fl) --> above,opt(['CNEE']),!,
		    {set_same_as_flag(Fl)}. 	% also consol.
as_referent(Fl) --> ['BELOW'],!,{set_same_as_flag(Fl)}.
as_referent(Fl) --> cnee_desig,!,{set_cgn_flag(Fl)}.
as_referent(Fl) --> notify,!,opt(['PARTY']),{set_same_as_flag(Fl)}.
as_referent(Fl) --> ['SHIPPER'],!,{set_shiprs_flag(Fl)}.
as_referent(Fl) --> opt(['ST']),notify,opt(['PARTY']),!,{set_notify_flag(Fl)}.
as_referent(_Fl) --> opt_attached,attachment.

above --> ['ABOVE'],!.
above --> ['ABV'],!.
above --> ['CONSOL'].

opt_attached --> ['ATTACHED'],opt(['LI']),!.
opt_attached --> [].

opt_attachment --> attachment,!.
opt_attachment --> [].

attachment --> restrict_attachment,!.
attachment --> ['RIDER'].

restrict_attachment --> ['DESCRIPTION'],!.
restrict_attachment --> ['ITEMIZED'],!.
%attachment --> ['LI'],!.
restrict_attachment --> ['LIST'],!.
restrict_attachment --> ['REMARKS'],!.
restrict_attachment --> ['SHEET'],!.

%same_as(_Fl) --> ['DO'],!.
same_as(Fl) --> ['DITTO'],!,opt_same_as_referent(Fl).
same_as(Fl) --> ['SAME AS'],same_as_referent(Fl).

opt_same_as_referent(Fl) --> same_as_referent(Fl),!.
opt_same_as_referent(_Fl) --> [].

same_as_referent(Fl) -->
	shipper_variants,!,{set_shiprs_flag(Fl)}.
same_as_referent(Fl) -->	
	as_referent(Fl).

branch(['CITY','BRANCH'|R],R) --> ['CITY'],['BRANCH'],!.
branch(['MAIN','BRANCH'|R],R) --> ['MAIN'],['BRANCH'].

cnee_desig --> opt(['A']),['CNEE'],!.
cnee_desig --> ['CNEE'],!.
cnee_desig --> ['CEE'],!.
cnee_desig --> ['CONSIGN'],!.

notify --> ['NOTIFY'],!.
notify --> ['NFTY'],!.
notify --> ['NTY'],!.
notify --> ['NYT'],!.
notify --> ['NTYS'],!.
notify --> ['NYY'],!.
notify --> [Word],
	   {functor(Word,_,0),
	    min_ed_dis(Word,'NOTIFY',N),N < 3,
	    \+keyword(Word) }.

shipper_variants --> ['FREIGHT'],['PAYER'],!.
shipper_variants --> ['SHIPPER'],opt(['ORDER']),opt(['BLANK']),!.
shipper_variants --> ['SHIPP'],!.
shipper_variants --> ['VARIOUS'],!,['SHIPPER'].

tester(Fl) --> ['TEST'],tester_1,{set_bogus_flag(Fl)}.
tester(Fl) --> ['INACTIVE'],tester_1,{set_bogus_flag(Fl)}.

tester_1 --> ['BROKER'].
tester_1 --> ['FILER'].


order --> ['ORDER'],!.
order --> ['KORDER'],!.
order --> ['HORDER'],!.
order --> ['ODER'],!.
order --> ['ORER'],!.
order --> ['RODER'],!.
order --> ['ISSUED'],['BY'].

bill_of_lading --> ['BL'],opt(['NO']),!.
bill_of_lading --> ['TBL'],opt(['NO']),!.
bill_of_lading --> form(_,_).

bl_adj(['']) --> bill_of_lading,!.
bl_adj(Rest) --> issued(One),!,
		 bl_adj(Two),
		 {append(One,Two,Rest)}.
bl_adj(Var) --> bl_adj_1,!,bl_adj(Var).
bl_adj([]) --> [].

bl_adj_1 --> 
	['ORIGIN'],!.
bl_adj_1 --> 
	['TMSC'],!.
bl_adj_1 --> 
	['MOSK'],!.
bl_adj_1 --> 
	['('],!.
bl_adj_1 --> 
	through,!.
bl_adj_1 --> 			% ? for now to handle CJS.
	[_Word].

opt_through --> through,!.
opt_through --> [].

through --> ['THROUGH'],!.
through --> ['T'],!.
through --> ['TH'],!.
through --> ['THR'].

other(R,R,_Fl) --> unk,!.
other(LC,R,_Fl) --> lc(LC,R),!.
other(Form,R,_Fl) --> form(Form,R),!.
other(Branch,R,_Fl) --> branch(Branch,R).
other(Date,R,_Fl) --> dated(Date,R),!.
other(PSC,R,_Fl) --> psc(PSC,R),!.  % includes INV.
other(Bond,R,_Fl) --> bond(Bond,R),!.
other(Contract,R,_Fl) --> contract(Contract,R),!.
other(Address,R,_Fl) --> address_locution(Address,R),!.
% Bug, tls better information coule be obtained
other(Shipord,R,_Fl) --> shipord(Shipord,R),!.
other(Telex,R,_Fl) --> telex(Telex,R),!.
other(Ref,R,Fl) --> other_ref(Ref,R,Fl),!.
other(['NVOCC'|R],R,Fl) --> ['NVOCC'],{set_cnf_flag(Fl)}.

address_locution(['SAME AS CNEE'|Tail],Tail)-->
	['AT'],opt(['THE']),['CNEE'],['ADDRESS'],
	!.

 /* the first clause prevents problems like bond st */
 bond(_,_) --> 
 	['BOND'],
 	[Word],
 	{street_desig(Word,_)},
 	!,
 	{fail}.
bond(['BOND',Word|List],Tail) --> 
	['BOND'],
	opt_number_desig(_Num),
	[Word],
	get_general_numbers(List,Tail).

contract(['CONTRACT',Num|List],Tail) --> 
	['CONTRACT'],
	opt_number_desig(_Num),
	number(Num),
	get_general_numbers(List,Tail).

dated(['DATED'|List],Tail) --> dated,get_numbers(List,Tail).

dated --> ['DATED'],!.
dated --> ['DTD'].
dated --> ['DT'].

form(['FORM',Word|Rest],T) --> 
	(['FORM'] ; ['RFC']),	
	[Word],
	form1(Rest,T).

form1(['NO',Word|Rest],T) -->
	['NO'],!,
	[Word],
	get_general_numbers(Rest,T).
form1(Rest,T) -->
	get_general_numbers(Rest,T).

issued(['ISSUED'|Rest]) -->
	['ISSUED'],!,
	issued_1(Rest).

issued_1(['IN',Loc]) --> 
	['IN'],!,
	[Loc].
/*
issued_1(['BY',E1,E2]) --> 
	['BY'],!,
	[E1],[E2].
*/
lc(['L/C',Num|R],R) -->
	['L/C'],opt(['NO']),general_number(Num),!.
lc(['L/C',Num|R],R) -->
	['LC'],opt(['NO']),general_number(Num).

psc(['PSC'|List],Tail) --> 
	['PSC'],	
	opt_number_desig(_Num),!,
	get_general_numbers(List,Tail),!.
psc(['INV'|List],Tail) --> 
	['INV'],	
	opt_number_desig(_Num),
	get_general_numbers(List,Tail).

telex(['TELEX'|List],Tail) --> ['TELEX'],get_numbers(List,Tail).

subject --> 
	['SUBJECT TO'],
	opt_dem_art,
	subject_1,!.

subject_1 --> collection.
subject_1 --> endorsement.

endorsement --> 
	['SHIPPER'],
	opt_word,
	['ENDORSEMENT'].

collection --> 
	['COLLECTION'],
	opt_of,
	['FREIGHT'],
	collection_location.

collection_location --> 
	['AT'],
	opt_dem_art,
	opt_word,
	['PORT'],
	port_modifyer.

port_modifyer --> 
	['TO'],opt_dem_art,
	[cardinal(_)],['CARRIER'],!.
port_modifyer --> [].

consignee(Rest,Addr,Tel,Attn,Fl,Oth) -->
	cnee_desig,!,
	opt_cnee_garbage,
	consignee_1(Rest,Addr,Tel,Attn,Fl,Oth).

opt_cnee_garbage --> 	
	['OF'],['SHIPMENTS'],!.
opt_cnee_garbage --> 	
	['CONSIGN'],opt(['TO']),!.
opt_cnee_garbage --> 	
	['CONTINUED'],!.
opt_cnee_garbage --> [].

consignee_1(Cnee,Addr,Tel,Attn,Fl,Oth) -->
	order,!,
	opt_of,
	consignee_2(Cnee,Addr,Tel,Attn,Fl,Oth).
consignee_1(Cnee,Addr,Tel,Attn,Fl,Oth) -->
	consignee_2_flag(Cnee,Addr,Tel,Attn,Fl,Oth).

consignee_2_flag(['SHIPPER'],Addr,Tel,Attn,Fl,Oth) --> 
	shipper_variants,!,
	{set_cgn_flag(Fl),set_shiprs_flag(Fl)},
	address(inflags('N','N'),Addr,Tel,Attn,Fl,Oth).
consignee_2_flag([],[],tel([],[]),[],_Fl,ea(cnee,Ent,Addr,Tel,Attn,Fl,Oth)) -->
	{(var(Fl) -> create_flag_struct(Fl) ; true)},
	org_name(inflags('N','N'),ea(_,Ent,Addr,Tel,Attn,Fl,Oth)),!.
consignee_2_flag(['REFERENT'],[],tel([],[]),[],_Fl,[]) --> [].

consignee_2(['SHIPPER'],Addr,Tel,Attn,Fl,Oth) --> 
	shipper_variants,!,
	address(inflags('N','N'),Addr,Tel,Attn,Fl,Oth).
consignee_2([],[],tel([],[]),[],_fl,ea(cnee,Ent,Addr,Tel,Attn,Fl,Oth)) -->
	{(var(Fl) -> create_flag_struct(Fl) ; true)},
	org_name(inflags('N','N'),ea(_,Ent,Addr,Tel,Attn,Fl,Oth)),!.
consignee_2(['REFERENT'],[],tel([],[]),[],_fl,[]) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% States (obsolescent).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
state_with_transition(State,St):- 
	state(State,St),
	\+ ambiguous_state(State).

ambiguous_state('CO').
ambiguous_state('CT').
ambiguous_state('IN').
ambiguous_state('LA').
ambiguous_state('MT').
ambiguous_state('NE').
ambiguous_state('MS').
ambiguous_state('ON').

state(Misp,Sp):-
	us_state(Misp,Sp).
state(Misp,Sp):-
	province(Misp,Sp).
state(Misp,Sp):-
	mexican_state(Misp,Sp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_flag_struct(Fl):-
	functor(Fl,flags,16).

get_incon_flag(Fl,Flag):- arg(15,Fl,Flag).
set_org_type_flag(Fl):- arg(1,Fl,1).
set_cnf_flag(Fl):- arg(2,Fl,1).
set_oo_flag(Fl):- arg(6,Fl,1).
set_same_as_flag(Fl):- arg(10,Fl,1).
set_cgn_flag(Fl):- arg(11,Fl,1).
set_shiprs_flag(Fl):- arg(12,Fl,1).
set_notify_flag(Fl):- arg(13,Fl,1).
set_bogus_flag(Fl):- arg(16,Fl,1).

opt([X]) --> ([X] ; []),!.

opt_pipe --> ['|'],!.
opt_pipe --> [].

opt_word --> [_].
opt_word --> [].

general_number(integer(Num)) --> [integer(Num)],!.
general_number(Let) --> [Let],{single_letter(Let)},!.

card_int_num(integer(Num)) --> [integer(Num)],!.
card_int_num(Num) --> [cardinal(Num)],!.

number(integer(Num)) --> [integer(Num)],!.

unk --> ['UNKNOWN'],!.

opt_dem_art --> dem_art,!.
opt_dem_art --> [].

dem_art --> ['THE'],!.
dem_art --> ['THRE'],!.
dem_art --> ['SHE'].

opt_of --> of,!.
opt_of --> ['TO'],!.
opt_of --> [].

of --> ['OF'],!.
of --> ['O'].	 

opt_number_desig(Num) --> number_desig(Num),!.
opt_number_desig([]) --> [].

number_desig('NO') --> ['NO'],!.
number_desig('NBR') --> ['NBR'],!.

end_of_file.

opt_entity(Ea) --> entity(Ea),!.
opt_entity([]) --> [].

attn_entity(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	general_phone(Tel,Tel1),!,
	entity(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel1,Attn,Fl,Oth)).
attn_entity(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)) -->
	order_of(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)),!.
attn_entity(Bef,ea(base,entity(Ent,Title,Rest),Addr,
						Tel,[Word|Attn],Fl,Oth)) -->
	[Word],!,
	entity(Bef,ea(base,entity(Ent,Title,Rest),Addr,Tel,Attn,Fl,Oth)).
