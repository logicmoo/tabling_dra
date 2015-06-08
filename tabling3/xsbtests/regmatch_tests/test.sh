#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running regmatch_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" regtest "test."
../gentest.sh "$XEMU $options" wildtest "test."
# The perl package is deprecated
#../gentest.sh "$XEMU $options" perltest "test."
