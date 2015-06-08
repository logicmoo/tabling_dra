#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running neg_tests/test.sh (using call subsumption)  ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

#-------------------------------------------------
# Tests of negation that give correct results.
#-------------------------------------------------
    # XEMU and options must be together in quotes
../sgentest.sh "$XEMU $options" neg1 "test."
../sgentest.sh "$XEMU $options" neg2 "test."
../sgentest.sh "$XEMU $options" neg3 "test."
../sgentest.sh "$XEMU $options" ullman2 "test."
#------------------------------------------------------
# Tests of left-to-right modylarly stratified negation.
#------------------------------------------------------
../sgentest.sh "$XEMU $options" lmod1 "test."
../sgentest.sh "$XEMU $options" lmod2 "test."
../sgentest.sh "$XEMU $options" lmod3 "test."
../sgentest.sh "$XEMU $options" lmod4 "test."
../sgentest.sh "$XEMU $options" lmod5 "test."
../sgentest.sh "$XEMU $options" lmod6 "test."
../sgentest.sh "$XEMU $options" lmod7 "test."
../sgentest.sh "$XEMU $options" lmod8 "test."
../sgentest.sh "$XEMU $options" lmod9 "test."
../sgentest.sh "$XEMU $options" lmod10 "test."
../sgentest.sh "$XEMU $options" lmod11 "test."
../sgentest.sh "$XEMU $options" ullman1 "test."
#---------------------------------------------------------
# Tests of left-to-right dynamically stratified negation.
#---------------------------------------------------------
../sgentest.sh "$XEMU $options" przy2 "test."
../sgentest.sh "$XEMU $options" ldynstrat0 "test."
../sgentest.sh "$XEMU $options" ldynstrat1 "test."
../sgentest.sh "$XEMU $options" ldynstrat2 "test."
../sgentest.sh "$XEMU $options" ldynstrat3 "test."
../sgentest.sh "$XEMU $options" ldynstrat4 "test."
#--------------------------------------------------------------------
# Tests of modylarly stratified negation (that give correct results).
#--------------------------------------------------------------------
../sgentest.sh "$XEMU $options" mod1 "test."
#-------------------------------------------------
# Genome tests involving unclassified negation.
#-------------------------------------------------
../sgentest.sh "$XEMU $options" q7 "test."
