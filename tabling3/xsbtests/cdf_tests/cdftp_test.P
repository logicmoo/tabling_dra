:- compiler_options([xpp_on]).

#include "standard.h"

messageln(Term):- messageln(Term,STDMSG).

:- import messageln/2 from standard.

:- import load_extensional_facts/1, load_intensional_rules/1 from cdf_io.
:- [altCDF].
:- [cdftp_chkCon].
:- [tp_utils].

:- import set_debug_off/0, set_debug_on/1 from tp_utils.

oneclicktest:- loadont,testont.

loadont:-
	load_extensional_facts(cdftp_testdir),
	load_intensional_rules(cdftp_testdir).

testont:- 
	(cdf_configuration(tabled_isa,on) -> 
	    testont1
	  ; messageln('Cannot perform cdfpt_test without tabled isa')).

testont1:- 
%	loadont,
	set_debug_off,
	testval(cid(bookshelfStereo,o1),true),
	testval(cid(expensiveStereo,o1),true),
	testval(cid(expBookStereo,o1),false),
	testval(cid(incon,o1),false),
	testval(cid(con,o1),true),
	testentails(cid(mother,fam),not(cid(man,fam)),true),
	testentails(cid(mother,fam),not(cid(male,fam)),false),
	testentails(cid(grandmother,fam),cid(parent,fam),true),
	testcw(cid(person,fam),exists(rid(hasBrother,fam),
	                                  cid(grandfather,fam)),true),
	testcw(cid(person,fam),exists(rid(hasBrother,fam),
	                                  cid(grandmother,fam)),false),
	testcw(cid(person,fam),exists(rid(hasBrother_oops,fam),
	                                  cid(grandmother,fam)),true),
	set_debug_on(2).

testcw(Id,CE,true):- 
	messageln(cw_testing(Id,CE)),
	(consistentWith(Id,CE) -> 
	    true
	  ; messageln('!!!different!'(cw_test(Id,CE,true)))).
testcw(Id,CE,false):- 
	messageln(cw_testing(Id,CE)),
	(\+ consistentWith(Id,CE) -> 
	    true
	  ; messageln('!!!different!'(cw_test(Id,CE,false)))).

testentails(Id,CE,true):- 
	messageln(entails_testing(Id,CE)),
	(allModelsEntails(Id,CE) -> 
	    true
	  ; messageln('!!!different!'(entails_test(Id,CE,true)))).
testentails(Id,CE,false):- 
	messageln(entails_testing(Id,CE)),
	(\+ allModelsEntails(Id,CE) -> 
	    true
	  ; messageln('!!!different!'(entails_test(Id,CE,false)))).

testval(Id,true):- 
	messageln(sat_testing(Id)),
	(checkIdConsistency(Id) -> 
	    true
	  ; messageln('!!!different!'(sat_test(Id,true)))).
testval(Id,false):- 
	messageln(sat_testing(Id)),
	(\+ checkIdConsistency(Id) -> 
	    true 
	  ; messageln('!!!different!'(checkIdConsistency(Id,false)))).

end_of_file.

check_lce([cid(expensiveStereo,o1)]).

checkIdConsistency([cid(expBookStereo,o1)]).