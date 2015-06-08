#! /bin/sh

XEMU=$1
FILE=$2
NUM=$3

echo "-------------------------------------------------------"
echo "--- Running benches/synth_benches/mttest.sh         ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../genbench.sh "$XEMU"  "[anc,bench], load_dyn(chain)." "mt_chain_ancestor_bench($NUM)." "$FILE"
 ../genbench.sh "$XEMU"  "[anc,bench], load_dyn(cycle)." "mt_cycle_ancestor_bench($NUM)." "$FILE"
 ../genbench.sh "$XEMU"  "[win,bench], load_dyn(cycle)." "mt_cycle_win_bench($NUM)." "$FILE"
 ../genbench.sh "$XEMU"  "[win,bench], load_dyn(chain)." "mt_chain_win_bench($NUM)." "$FILE"



