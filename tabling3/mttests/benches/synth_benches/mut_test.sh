#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/synth_benches/test.sh           ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../mut_genbench.sh "$XEMU"  "[anc,bench],load_dyn(chain)." "chain_ancestor_bench." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[anc,bench],load_dyn(cycle)." "cycle_ancestor_bench." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[win,bench],load_dyn(cycle)." "cycle_win_bench." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[win,bench],load_dyn(chain)." "chain_win_bench." "$FILE"



