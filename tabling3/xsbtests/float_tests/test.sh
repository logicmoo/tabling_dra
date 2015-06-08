#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running float_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" vac "test."
../gentest.sh "$XEMU $options" roots "test."
../gentest.sh "$XEMU $options" roots_int_comp "test."
../gentest.sh "$XEMU $options" read_test "test."
../gentest.sh "$XEMU $options" assert_test "test."
../gentest.sh "$XEMU $options" test_precision "test."
