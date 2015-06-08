#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running table_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

#========================================================================
# Force some files to be compiled before running the tests.  cycle.P
# will always be compiled, because there was a bug on Linux in compiling
# this file and we want to catch it.  Since this step is performed only
# so that the corresponding object files exist, it is not included as part
# of the garbage collection test.

$XEMU -m 4000 -c 4000 -g none << EOF

compile(lists).
compile(sets).
compile(correct).
compile(utils).
compile(cycle).

EOF
#========================================================================

#--------------------------------------------------
# Tests get_calls and get_returns and tries as code
#--------------------------------------------------
    # XEMU and options must be together in quotes
 ../sgentest.sh "$XEMU $options" concomp "t, d1, d2." 
#--------------------------------------------------
 ../sgentest.sh "$XEMU $options" testdyntable "test." 
#--------------------------------------------------
 ../sgentest.sh "$XEMU -l $options" aggregs_test "test." 
#--------------------------------------------------
 ../sgentest.sh "$XEMU $options" float "test."  
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" expand "test."
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" grammarlrk3 "test."
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" lrbug "test."
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" unw_tr1 "test."
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" tfinda "test".
#-------------------------------------------------
# test heap reclamation after check complete
 ../sgentest.sh "$XEMU $options" testh "time2048."
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" empty_answer "test".
#------------------------------------------------- 
 ../sgentest.sh "$XEMU $options" flora1 "test".  
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" pps "test".  
#-------------------------------------------------
 ../sgentest.sh "$XEMU $options" dipti "test". 
#------------------------------------------------- atp/atc basic
 ../sgentest.sh "$XEMU $options" abol_test "test". 
#------------------------------------------------- a module t basic
 ../sgentest.sh "$XEMU $options" abol_test2 "test".
#------------------------------------------------- a module t gc diff preds
 ../sgentest.sh "$XEMU $options" abol_test2a "test". 
#------------------------------------------------- atp gc same preds
 ../sgentest.sh "$XEMU $options" abol_test3 "test". 
#------------------------------------------------- atp gc diff preds
 ../sgentest.sh "$XEMU $options" abol_test3a "test".
#------------------------------------------------- atp gc diff preds + valid
 ../sgentest.sh "$XEMU $options" abol_test3b "test".
#------------------------------------------------- atp gc diff preds + valid + multiple gcs
 ../sgentest.sh "$XEMU $options" abol_test3c "test".
#-------------------------------------------------
# ../sgentest.sh "$XEMU $options" atc_test "test".    # atc not implemented for subsumption
#------------------------------------------------- cascading abolish for subgoals with gc etc.
# ../sgentest.sh "$XEMU $options" abolish_cascade "test".  # cascade not impl for subs
#-------------------------------------------------
# ../sgentest.sh "$XEMU $options" recursive_aboltest "test".  # cascade not impl for subs
#------------------------------------------------- % very simple -- needs more cases.
# ../sgentest.sh "$XEMU $options" incremental "test".  not expected to work
#-------------------------------------------------
