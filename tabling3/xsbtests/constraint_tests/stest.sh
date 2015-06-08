#! /bin/sh

# This directory is intended for tests of Prolog-level constraints as
# well as CHR libraries.  Attribute variable tests per se should be
# in ../attv_tests.

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running constraint_tests/test.sh (using call subsumption) ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2

../sgentest.sh "$XEMU $opts" residualAttv "test."
../sgentest.sh "$XEMU $opts" csAttv "test."

# The following (from test.sh) dont involve tabling, and don't need to be run for -S
#../gentest.sh "$XEMU $opts" testsetarg "test."
#../gentest.sh "$XEMU $opts" test_constraintLib "test."
#../gentest.sh "$XEMU $opts" timetest "test."
#../gentest.sh "$XEMU $opts" gctest "test."
#../gentest.sh "$XEMU $opts" clprtest "test."
#../gentest.sh "$XEMU $opts" test_bounds "test."
# test of C-calling XSB w. constraints.
#../gentest.sh "$XEMU $opts" cvarconstr_make "test."
