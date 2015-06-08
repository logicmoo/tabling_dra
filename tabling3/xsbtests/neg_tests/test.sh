#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running neg_tests/test.sh                       ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

#-------------------------------------------------
# Tests of negation that give correct results.
#-------------------------------------------------
    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" neg1 "test."
../gentest.sh "$XEMU $options" neg2 "test."
../gentest.sh "$XEMU $options" neg3 "test."
../gentest.sh "$XEMU $options" ullman2 "test."
#------------------------------------------------------
# Tests of left-to-right modylarly stratified negation.
#------------------------------------------------------
../gentest.sh "$XEMU $options" lmod1 "test."
../gentest.sh "$XEMU $options" lmod2 "test."
../gentest.sh "$XEMU $options" lmod3 "test."
../gentest.sh "$XEMU $options" lmod4 "test."
../gentest.sh "$XEMU $options" lmod5 "test."
../gentest.sh "$XEMU $options" lmod6 "test."
../gentest.sh "$XEMU $options" lmod7 "test."
../gentest.sh "$XEMU $options" lmod8 "test."
../gentest.sh "$XEMU $options" lmod9 "test."
../gentest.sh "$XEMU $options" lmod10 "test."
../gentest.sh "$XEMU $options" lmod11 "test."
../gentest.sh "$XEMU $options" ullman1 "test."
#---------------------------------------------------------
# Tests of left-to-right dynamically stratified negation.
#---------------------------------------------------------
../gentest.sh "$XEMU $options" przy2 "test."
../gentest.sh "$XEMU $options" ldynstrat0 "test."
../gentest.sh "$XEMU $options" ldynstrat1 "test."
../gentest.sh "$XEMU $options" ldynstrat2 "test."
../gentest.sh "$XEMU $options" ldynstrat3 "test."
../gentest.sh "$XEMU $options" ldynstrat4 "test."
#--------------------------------------------------------------------
# Tests of modylarly stratified negation (that give correct results).
#--------------------------------------------------------------------
../gentest.sh "$XEMU $options" mod1 "test."
#-------------------------------------------------
# Genome tests involving unclassified negation.
#-------------------------------------------------
../gentest.sh "$XEMU $options" q7 "test."
