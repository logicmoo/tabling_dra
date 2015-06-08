#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/synth_benches/test1.sh          ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../genbench.sh "$XEMU"  "[anc1,bench],load_dyn(chain)." "chain_ancestor_bench." "$FILE"
 ../genbench.sh "$XEMU"  "[anc1,bench],load_dyn(cycle)." "cycle_ancestor_bench." "$FILE"
 ../genbench.sh "$XEMU"  "[win1,bench],load_dyn(cycle)." "cycle_win_bench." "$FILE"
 ../genbench.sh "$XEMU"  "[win1,bench],load_dyn(chain)." "chain_win_bench." "$FILE"



