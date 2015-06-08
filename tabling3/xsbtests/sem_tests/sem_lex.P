/************************************************************************/
/*									*/
/* SEMANTICA System							*/
/* Copyright (C) SUNY at Stony Brook, 1994				*/
/*									*/
/************************************************************************/

/*======================================================================
  File                  :  sem_lex.P
  Author(s)             :  Kostis F. Sagonas
  Last modification     :  October 26, 1994
========================================================================*/

:- export tokenise/2.

:- import append/3 from basics.

tokenise(String, Tokenised_List) :-
	atom_codes(String, Chars),
	form_tokens(Chars, [], Tokenised_List).

/*----------------------------------------------------------------------*/
/* form_tokens(+Character_List, +Current_ASCII_Token, -Token_List)	*/
/*	Creates Token_List as the list of tokens that derive from	*/
/*	Character_List.  Each candidate token in its ASCII form is	*/
/*	built into Current_ASCII_Token.					*/
/*									*/
/* NOTE: I should be ashamed for not using difference lists!		*/
/*----------------------------------------------------------------------*/

form_tokens([], Current_ASCII_Token, One_Token_List) :-
	make_token(Current_ASCII_Token, One_Token_List).
form_tokens([Char|Rest_Chars], Current_ASCII_Token, Token_List) :-
	( ( Char =:= 0' ; Char =:= 9 ) ->	% Ascii(9) is '\t' (tab).
		make_token(Current_ASCII_Token, One_Token_List),
		form_tokens(Rest_Chars, [], Rest_Token_List),
		append(One_Token_List, Rest_Token_List, Token_List)
	; ( atom_codes(Delimiter,[Char]), delimiter(Delimiter) ) ->
		make_token(Current_ASCII_Token, One_Token_List),
		form_tokens(Rest_Chars, [], Rest_Token_List),
		append(One_Token_List, [Delimiter|Rest_Token_List], Token_List)
	; append(Current_ASCII_Token, [Char], New_Current_ASCII_Token),
	  form_tokens(Rest_Chars, New_Current_ASCII_Token, Token_List)
	).


/*----------------------------------------------------------------------*/
/* make_token(+Current_ASCII_Token, -Token_List)			*/
/*	Token_List consists of the token that corresponds to the	*/
/*	Current_ASCII_Token.  In the following, name/2 is being used	*/
/*	so that tokens that are numbers, are returned as such.		*/
/*----------------------------------------------------------------------*/

make_token(Current_ASCII_Token, TokenList) :-
	( Current_ASCII_Token == [] -> TokenList = []
	; name(Token, Current_ASCII_Token), TokenList = [Token]
	).


/*----------------------------------------------------------------------*/
/* delimiter(+Delimiter)						*/
/*	True iff Delimiter is a delimiter in Semantica.			*/
/*----------------------------------------------------------------------*/

delimiter('&').
delimiter('=').
delimiter('(').
delimiter(')').
delimiter('[').
delimiter(']').
delimiter('{').
delimiter('}').
delimiter('|').
delimiter(':').
delimiter(',').
delimiter('<').
delimiter('>').
delimiter('"').		% Temporary to denote terminals.
delimiter('-').		% Temporary to denote negation.

/* -------------------- end of file sem_lex.P ------------------------- */
