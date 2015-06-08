#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running retract_tests/test.sh                   ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2


#---------------------------------------
# Assert and retract tests.
#---------------------------------------
    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" testretract "test."
#---------------------------------------
../gentest.sh "$XEMU $options" boyer_assert "boyer."
#---------------------------------------
../gentest.sh "$XEMU $options" load_backsuite "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retract_gc "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retr_gc_noind "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retr_gc_fancy "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retra_gc "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retra_gc_noind "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retra_gc_fancy "test."
#---------------------------------------
../gentest.sh "$XEMU $options" test_retra_gc_nongen_fancy "test."
#---------------------------------------
