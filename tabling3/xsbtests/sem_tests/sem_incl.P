/************************************************************************/
/*									*/
/* SEMANTICA System							*/
/* Copyright (C) SUNY at Stony Brook, 1995				*/
/*									*/
/************************************************************************/

/*======================================================================
  File			:  sem_incl.P
  Author(s)		:  Kostis F. Sagonas
  Last modification	:  May 3, 1995
========================================================================*/

:- export include_lexi_file/1, include_conf_file/1,
	  include_tree_file/1, include_sequence_file/1.

:- import expand_file_name/2 from machine.
:- import conset/2, conget/2, coninc/1 from prof_sbp.
:- import seeing/1, see/1, seen/0, read/1, repeat/0 from standard.
:- import sem_lexi_mode/1, sem_config_mode/1,
	  lex_interp/8, config/7, tree/5, sequence/2
     from usermod.

:- dynamic '_$ERROR'/1, '_$SEM'/1.

:- dynamic sem_lexi_mode/1, sem_config_mode/1.
:- dynamic lex_interp/8, config/7, tree/5, sequence/2.


/*======================================================================*/
/*  include_lexi_file(+File)						*/
/*      Includes the contents of the lexical interpretation File into	*/
/*	the lexical interpretations loaded in memory.  In case of an	*/
/*	error, the predicate fails.					*/
/*======================================================================*/

include_lexi_file(File) :-
	seeing(OldFile), expand_file_name(File,Filename), see(Filename),
	read_and_assert_lexi_file_temporarily,
        see(OldFile), fail.
include_lexi_file(_) :-
	( retract('_$ERROR'(sem_lexi_mode(_))) -> 
		retractall('_$ERROR'(_)), retractall('_$SEM'(_)), fail
	; ( retract('_$SEM'(Term)), assert(Term), fail ; true )
	).

read_and_assert_lexi_file_temporarily :-
	repeat,
	    read(Term),
	    ( Term == end_of_file -> !, seen
	    ; Term = lex_interp(Cat,LexItem,_I,Val1,Val3,Val4,Val5,DefStr) ->
		( lex_interp(Cat,LexItem,_,Val1,Val3,Val4,Val5,DefStr) -> true
		; assert('_$SEM'(Term))
		), fail
	    ; Term = sem_lexi_mode(Mode) ->
		( sem_lexi_mode(Mode) -> true ; assert('_$ERROR'(Term)) ),
		fail
	    ).

/*======================================================================*/
/*  include_conf_file(+File, -ErrorCode)				*/
/*      Includes the contents of the configuration File into the	*/
/*	configurations loaded in memory.  In case of an error, the	*/
/*	predicate fails.						*/
/*======================================================================*/

include_conf_file(File) :-
	seeing(OldFile), expand_file_name(File,Filename), see(Filename),
	read_and_assert_conf_file_temporarily,
	see(OldFile), fail.
include_conf_file(_) :-
	( retract('_$ERROR'(sem_config_mode(_))) -> 
		retractall('_$ERROR'(_)), retractall('_$SEM'(_)), fail
	; ( retract('_$SEM'(Term)), assert(Term), fail ; true )
	).

read_and_assert_conf_file_temporarily :-
	repeat,
	    read(Term),
	    ( Term == end_of_file -> !, seen
	    ; Term = config(Config,Val1,Val2,Val3,Val4,Val5,DefStr) ->
		( config(Config,Val1,Val2,Val3,Val4,Val5,DefStr) -> true
		; assert('_$SEM'(Term))
		), fail
	    ; Term = sem_config_mode(Mode) ->
		( sem_config_mode(Mode) -> true ; assert('_$ERROR'(Term)) ),
		fail
	    ).

/*======================================================================*/
/*  include_tree_file(+File)						*/
/*      Includes the contents of the tree File into the trees loaded	*/
/*	in memory.							*/
/*======================================================================*/

include_tree_file(File) :-
	seeing(OldFile), expand_file_name(File,Filename), see(Filename),
	conset('tree #', 0), conset_max_tree_index,
	read_and_assert_tree_file,
	see(OldFile), fail.
include_tree_file(_).

conset_max_tree_index :-
	tree(_, _, _, _, Index), conget('tree #', TreeNo),
	( Index > TreeNo -> conset('tree #', Index) ),
	fail.
conset_max_tree_index.

read_and_assert_tree_file :-
	repeat,
	    read(Term),
	    ( Term == end_of_file -> !, seen
	    ; Term = tree(Sent,Tree,S1,S2,_) ->
		( tree(Sent,Tree,S1,S2,_) -> true	% already exists
		; coninc('tree #'), conget('tree #', Index),
		  assert(tree(Sent,Tree,S1,S2,Index))
		),
		fail		% get next one
	    ).

/*======================================================================*/
/*  include_sequence_file(+File)					*/
/*      Includes the contents of the sequence File into the sequences	*/
/*	loaded in memory.						*/
/*======================================================================*/

include_sequence_file(File) :-
	seeing(OldFile), expand_file_name(File,Filename), see(Filename),
	conset('seq #', 0), conset_max_seq_index,
	read_and_assert_sequence_file,
	see(OldFile), fail.
include_sequence_file(_).

conset_max_seq_index :-
	sequence(_, Index), conget('seq #', SeqNo),
	( Index > SeqNo -> conset('seq #', Index) ),
	fail.
conset_max_seq_index.

read_and_assert_sequence_file :-
	repeat,
	    read(Term),
	    ( Term == end_of_file -> !, seen
	    ; Term = sequence(Sequence,_) ->
		( sequence(Sequence, _) -> true		% already exists
		; coninc('seq #'), conget('seq #', Index),
		  assert(sequence(Sequence,Index))
		),
		fail		% get next one
	    ).

/* -------------------- end of file sem_incl.P ------------------------ */
