#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running ai_tests/test.sh                        ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

$XEMU $options << EOF

[can_mono].

[set_basics_mono].

[set_unify].

EOF

    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" cs_o "test."
../gentest.sh "$XEMU $options" cs_r "test."
../gentest.sh "$XEMU $options" disj "test."
../gentest.sh "$XEMU $options" gabriel "test."
../gentest.sh "$XEMU $options" kalah "test."
../gentest.sh "$XEMU $options" peep "test."
../gentest.sh "$XEMU $options" pg "test."
../gentest.sh "$XEMU $options" plan "test."
# ../gentest.sh "$XEMU $options" press1 "test."	# Not certain on the correct results
../gentest.sh "$XEMU $options" qsort "test."
../gentest.sh "$XEMU $options" queens "test."
../gentest.sh "$XEMU $options" read "test."
