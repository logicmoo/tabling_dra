#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running incremental_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

#------------------------------------------------- general incremental tests
../gentest.sh "$XEMU $options" incremental "test".
#------------------------------------------------- same as above, but with changed decls.
../gentest.sh "$XEMU $options" incremental1 "test".
#------------------------------------------------- incremental tests with interned tries
../gentest.sh "$XEMU $options" incremental_trie "test".
#------------------------------------------------- incremental tests with trie asserts
../gentest.sh "$XEMU $options" inc_trie_dyn "test".
#------------------------------------------------- inc with interned tries - for storage.P
../gentest.sh "$XEMU $options" inc_trie_alt "test".
#------------------------------------------------- inc with asserted/retracted clauses
../gentest.sh "$XEMU $options" incremental_rule "test".
#------------------------------------------------- inc_rule with initial empty dyn predicate
../gentest.sh "$XEMU $options" incremental_rule_alt "test".
#------------------------------------------------- inc trans depends (cyclic)
../gentest.sh "$XEMU $options" test_incr_depends "test".
#------------------------------------------------- inc trans depends (non cyclic)
../gentest.sh "$XEMU $options" test_incr_depends_2 "test".
#------------------------------------------------- test incremental <-> opaque
../gentest.sh "$XEMU $options" test_inc_switch "test".
#------------------------------------------------- testing executable incremental dirs.
../gentest.sh "$XEMU $options" test_directives "test".
#------------------------------------------------- testing executable incremental decls.
../gentest.sh "$XEMU $options" test_declarations "test".
#------------------------------------------------- testing executable incremental decls.
../gentest.sh "$XEMU -l $options" test_visitors "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_lazy "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_sound_updates "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_wfs_update "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" incr_scc "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" incr_test_romero "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_iso_basic "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_iso_mult_visit "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_iso_hash "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_iso_undef "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_iso_attr "test."
#--------------------------------------------------
../gentest.sh "$XEMU $options" test_tc "test".
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_abolish_nonincremental "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_abolish_incremental_call_single "test."
#------------------------------------------------- inc tests with abolish_all_tables
../gentest.sh "$XEMU $options" inc_abol "test".
#------------------------------------------------- inc tests with abolish_table_call
../gentest.sh "$XEMU $options" inc_atc "test".
#------------------------------------------------- inc tests with abolish_table_call + gc
../gentest.sh "$XEMU $options" inc_atc_gc "test".
#------------------------------------------------- more inc tests with abolish_table_call + gc
../gentest.sh "$XEMU $options" inc_atc_gc_tricky "test".
#-------------------------------------------------- yet more inc tests with abolish_table_call_single + gc
#../gentest.sh "$XEMU $options" inc_atc_single_gc_deps "test".  (doesn't work yet)
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_introspection "test."
#-------------------------------------------------- yet more tests of invalidation
../gentest.sh "$XEMU -l $options" test_invalidate "test."
#--------------------------------------------------
../gentest.sh "$XEMU -l $options" test_table_errors "test."
#--------------------------------------------------
