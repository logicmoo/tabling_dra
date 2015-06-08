#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running compiler_tests/test.sh                  ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

    # XEMU and options must be together in quotes
./gentest.sh "$XEMU $options" myspec
./gentest.sh "$XEMU $options" myvarproc
./gentest.sh "$XEMU $options" meta
./gentest.sh "$XEMU $options" top_down

