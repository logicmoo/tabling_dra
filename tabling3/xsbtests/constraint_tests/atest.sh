#! /bin/sh

# This directory is intended for tests of Prolog-level constraints as
# well as CHR libraries.  Attribute variable tests per se should be
# in ../attv_tests.

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running constraint_tests/test.sh using call abstraction               ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2
valgrind=$3

../agentest.sh "$XEMU $opts" testsetarg "test."
../agentest.sh "$XEMU $opts" test_constraintLib "test."
../agentest.sh "$XEMU $opts" timetest "test."
../agentest.sh "$XEMU $opts" gctest "test."
../agentest.sh "$XEMU $opts" clprtest "test."
../agentest.sh "$XEMU $opts" test_bounds "test."
../agentest.sh "$XEMU $opts" residualAttv "test."
../agentest.sh "$XEMU $opts" callsubs_constraints "test."
../agentest.sh "$XEMU $opts" tab_constraint "test."
../agentest.sh "$XEMU $opts" testlightmeal "test."

# VALGRIND
if test "$valgrind" = "true"; then
	echo "Skipping cvarconstr_make in table_tests"
else
# test of C-calling XSB w. constraints.
../agentest.sh "$XEMU $opts" cvarconstr_make "test."
fi