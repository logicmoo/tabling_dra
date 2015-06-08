#! /bin/sh

# This directory is intended for tests of Prolog-level constraints as
# well as CHR libraries.  Attribute variable tests per se should be
# in ../attv_tests.

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running constraint_tests/test.sh                ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2
valgrind=$3
../gentest.sh "$XEMU $opts" testsetarg "test."
../gentest.sh "$XEMU $opts" test_constraintLib "test."
../gentest.sh "$XEMU $opts" timetest "test."
../gentest.sh "$XEMU $opts" gctest "test."
../gentest.sh "$XEMU $opts" clprtest "test."
../gentest.sh "$XEMU $opts" test_bounds "test."
../gentest.sh "$XEMU $opts" residualAttv "test."
../gentest.sh "$XEMU $opts" callsubs_constraints "test."
../gentest.sh "$XEMU $opts" tab_constraint "test."
../gentest.sh "$XEMU $opts" testlightmeal "test."

# Valgrind
# VALGRIND
if test "$valgrind" = "true"; then
	echo "Skipping cvarconstr_make in table_tests"
else
# test of C-calling XSB w. constraints.
../gentest.sh "$XEMU $opts" cvarconstr_make "test."
fi