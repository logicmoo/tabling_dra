/************************************************************************/
/*									*/
/* SEMANTICA System							*/
/* Copyright (C) SUNY at Stony Brook, 1994				*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  semantica.P
  Author(s)		:  Kostis F. Sagonas
  Last modification	:  July 25, 1995
========================================================================*/

:- export sem_mode/1, chk_syntax/6, sort_lex_interp/0, sort_config/0,
	  unload_config/0, load_config/0, load_config/1,
	  unload_lex_interp/0, load_lex_interp/0, load_lex_interp/1,
	  lex_to_lexi/1, rule_to_conf/1.

:- import parse/2 from sem_parse.
:- import abolish_all_tables/0 from tables.
:- import str_cat/3, expand_file_name/2 from machine.
:- import sem_lexi_mode/1, sem_config_mode/1,
	  lex_interp/8, config/7, conf_val/1, lex_val/1,
	  i_lex_item/4, i_rule/6, i_start_symbol/1,
	  i_variable_category/1, i_head_direction/1
     from usermod.
:- import retract/1 from assert.

:- dynamic i_lex_item/4, i_rule/6, i_start_symbol/1,
	   i_variable_category/1, i_head_direction/1.
:- dynamic sem_lexi_mode/1, sem_config_mode/1.
:- dynamic lex_interp/8, config/7, conf_val/1, lex_val/1.


/*======================================================================*/
/*  sem_mode(?Mode)							*/
/*======================================================================*/

sem_mode(Mode) :-
	sem_config_mode(Mode),	% Config file must be loaded
	( sem_lexi_mode(LexMode) -> Mode == LexMode ; true ).


/*======================================================================*/
/*  chk_syntax(+DefStr,+Val1,+Val2,+Val3,+Val4,+Val5)			*/
/*	Succeeds iff the String denoted by DefStr and Val1..Val5 can	*/
/*	be parsed as a rule by the Semantica parser.			*/
/*======================================================================*/

chk_syntax(DefStr,Val1,Val2,Val3,Val4,Val5) :-
	make_string(DefStr,Val1,Val2,Val3,Val4,Val5,String),
	parse(String, _Translation).


/*======================================================================*/
/*  load_lex_interp							*/
/*======================================================================*/

load_lex_interp :-
	generate_lex_interp.

/*======================================================================*/
/*  load_lex_interp(+File)						*/
/*	Dynamically loads (asserts) the contents of File, and does all	*/
/*	necessary processing to assert appropriate translations of the	*/
/*	strings that describe the lexical interpretations.		*/
/*======================================================================*/

load_lex_interp(File) :-
	abolish_all_tables, unload_lex_interp, load_dyn(File), load_lex_interp.

/*======================================================================*/
/*  unload_lex_interp							*/
/*======================================================================*/

unload_lex_interp :-
	retract(lex_val(ValRule)), retract(ValRule), fail.
unload_lex_interp.

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

generate_lex_interp :-
	lex_interp(Cat, LexItem, _Index, Val1,Val3,Val4,Val5, DefStr),
	make_lexi_config(Cat, LexItem, LexConfig),
	make_string(DefStr, Val1,LexConfig,Val3,Val4,Val5, String),
	parse(String, Translation),
	assert(lex_val(Translation)),
	assert(Translation),
	fail.		% continue to the next one if errors occur!
generate_lex_interp.

/*======================================================================*/
/*  load_config								*/
/*======================================================================*/

load_config :-
	generate_config_interp.

/*======================================================================*/
/*  load_config(+File)							*/
/*	Dynamically loads (asserts) the contents of File, and does all	*/
/*	necessary processing to assert appropriate translations of the	*/
/*	strings that describe the interpretation of the configurations.	*/
/*======================================================================*/

load_config(File) :-
	abolish_all_tables, unload_config, load_dyn(File), load_config.

/*======================================================================*/
/*  unload_config							*/
/*======================================================================*/

unload_config :-
	retract(conf_val(ConfRule)), retract(ConfRule), fail.
unload_config.

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

generate_config_interp :-
	config(_Config, Val1,Val2,Val3,Val4,Val5,DefStr),
	make_string(DefStr,Val1,Val2,Val3,Val4,Val5,String),
	parse(String, Translation),
	assert(conf_val(Translation)),
	assert(Translation),
	fail.		% continue to the next one if errors occur!
generate_config_interp.

/*----------------------------------------------------------------------*/
/* make_string(+DefStr,+Val1,+Val2,+Val3,+Val4,+Val5, -String)		*/
/*----------------------------------------------------------------------*/

make_string(DefStr,Val1,Val2,Val3,Val4,Val5,String) :-
	str_cat('Val(', Val1, S1), str_cat(S1, ',', S2),
	str_cat(S2, Val2, S3),
	( Val3 == '' -> SR = S3
	; str_cat(S3, ',', S4), str_cat(S4, Val3, S5),
	  ( Val4 == '' -> SR = S5
	  ; str_cat(S5, ',', S6), str_cat(S6, Val4, S7),
	    ( Val5 == '' -> SR = S7
	    ; str_cat(S7, ',', S8), str_cat(S8, Val5, SR)
	    )
	  )
	),
	str_cat(SR, ') iff ', Siff), str_cat(Siff, DefStr, String).

/*----------------------------------------------------------------------*/
/* make_lexi_config(+Category, +LexItem, -LexConfiguration)		*/
/*----------------------------------------------------------------------*/

make_lexi_config(Cat, LexItem, LexConfig) :-
	str_cat('[', Cat, T1),
	str_cat(T1, LexItem, T2),
	str_cat(T2, ']', LexConfig).

/*======================================================================*/
/*  sort_lex_interp							*/
/*      Sorts the lexical interpretations alphabetically within	each	*/
/*      category and updates the database internally.			*/
/*======================================================================*/

sort_lex_interp :-
        setof(lex_interp(Cat,LexItem,Index,Val1,Val3,Val4,Val5,DefStr),
	      retract(lex_interp(Cat,LexItem,Index,Val1,Val3,Val4,Val5,DefStr)),
	      LexInterpretations),
        restore_retracted_items(LexInterpretations).

/*======================================================================*/
/*  sort_config								*/
/*      Sorts the configurations alphabetically and updates the		*/
/*      database internally.						*/
/*======================================================================*/

sort_config :-
	setof(config(Config,Val1,Val2,Val3,Val4,Val5,DefStr),
	      retract(config(Config,Val1,Val2,Val3,Val4,Val5,DefStr)),
	      Configurations),
	restore_retracted_items(Configurations).

/*----------------------------------------------------------------------*/
/*  restore_retracted_items(+List)					*/
/*      Asserts a list of items that have been retracted from the	*/
/*      database.							*/
/*----------------------------------------------------------------------*/

restore_retracted_items([]).
restore_retracted_items([H|T]) :- assert(H), restore_retracted_items(T).


/*======================================================================*/
/*  lex_to_lexi(+File)							*/
/*	Converts the contents of a Syntactica lexicon File to the	*/
/*	lexical interpretation format of Semantica, and asserts them	*/
/*	in memory.							*/
/*======================================================================*/

lex_to_lexi(File) :-
	seeing(OldFile),
	expand_file_name(File, FileName),
	see(FileName),
	read_and_assert_lexi,
	see(OldFile),
	fail.
lex_to_lexi(_).

read_and_assert_lexi :-
	repeat,
	    read(Term),
	    ( Term == end_of_file -> !, seen
	    ; Term = i_lex_item(Cat,SyntLexI,Index,_) ->
		make_sem_terminal(SyntLexI, SemLexI),
		assert(lex_interp(Cat,SemLexI,Index,'','','','','')), fail
	    ).

/*======================================================================*/
/*  rule_to_conf(+File)							*/
/*	Converts the contents of a Syntactica rule File to the		*/
/*	configuration format of Semantica, and asserts them in memory.	*/
/*======================================================================*/

rule_to_conf(File) :-
	load_dyn(File),
	assert_conf,
	fail.
rule_to_conf(_) :-
	retractall(i_rule(_,_,_,_,_,_)), retractall(i_start_symbol(_)),
	retractall(i_variable_category(_)), retractall(i_head_direction(_)).

assert_conf :- 
	i_rule(LHS,R1,R2,R3,R4,_),
	make_config(LHS, [R1,R2,R3,R4], Config),
	assert(config(Config,'',Config,'','','','')).

make_config(LHS, RHS, Config) :-
	str_cat('[', LHS, StrIn),
        handle_rhs(RHS, StrIn, StrOut),
        str_cat(StrOut, ']', Config).

:- mode handle_rhs(+,+,-).

handle_rhs([], Str, Str).
handle_rhs([H|T], StrIn, StrOut) :-
	( H \== '' -> 
		( i_rule(H,_,_,_,_,_) -> SH = H ; make_sem_terminal(H, SH) ),
		str_cat(StrIn, ' ', TempStr), str_cat(TempStr, SH, StrMid)
	; StrMid = StrIn
	),
	handle_rhs(T, StrMid, StrOut).

/*----------------------------------------------------------------------*/
/* make_sem_terminal(+SyntItem, -SemItem)				*/
/*	Italisizes a Syntactica terminal.				*/
/*----------------------------------------------------------------------*/

make_sem_terminal(SyntItem, SemItem) :-
	str_cat('"', SyntItem, T), str_cat(T, '"', SemItem).

/* -------------------- end of file semantica.P ----------------------- */
