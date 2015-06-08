:- compiler_options([xpp_on]).

#include "standard.h"
:- import messageln/2 from standard.
messageln(Message) :- messageln(Message, STDMSG).

:- [ce_values].
:- [altcdf].
:- [cdftp_chkCon].
:- import load_extensional_facts/1 from cdf_io.

?- load_extensional_facts(ce_testdir).

%?- testall.

testall:- 
	(cdf_configuration(tabled_isa,on) -> 
	    testall1
	  ; messageln('Cannot perform ce_test without tabled isa')).

testall1:- 
	sat_test(Formula,Val),
	messageln(sat_test(Formula)),
	testval(Formula,Val),
	fail.
testall1.

testval(Formula,true):- 
	(sat(Formula) -> 
	    true 
	  ; messageln('!!!different!'(sat_test(Formula,true)))).
testval(Formula,false):- 
	(\+ sat(Formula) -> 
	    true 
	  ; messageln('!!!different!'(sat_test(Formula,false)))).


