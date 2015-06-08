#-----------------------------------------------------------------------#
#
#	File		: Semantica/Makefile
#	Authors		: Kostis Sagonas
#	Last updated	: December 14, 1998
#
#-----------------------------------------------------------------------#

OBJEXT=.xwam

ALL =	sem_lex${OBJEXT} sem_parse${OBJEXT} sem_pe${OBJEXT} \
	semantica${OBJEXT} sem_pretty${OBJEXT} sem_incl${OBJEXT} \
	sem_eval${OBJEXT} sem_dnf${OBJEXT}

Option = [optimize]

#-----------------------------------------------------------------------#
#
Prolog = xsb
#
#-----------------------------------------------------------------------#

# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P ${OBJEXT} .H
.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...

all: $(ALL)
	touch cmd...
	$(Prolog) < cmd...
	/bin/rm -f cmd...

semtest:
	for file in `ls CSH/sem*.csh`; do csh $$file; done

.DONE:
	echo Done
