#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/synth_benches/test.sh           ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../genbench.sh "$XEMU"  "[chain],[anc,bench]." "chain_ancestor_bench." "$FILE"
 ../genbench.sh "$XEMU"  "[cycle],[anc,bench]." "cycle_ancestor_bench." "$FILE"
#../genbench.sh "$XEMU"  "[cycle],[win,bench]." "cycle_win_bench." "$FILE"
#../genbench.sh "$XEMU"  "[chain],[win,bench]." "chain_win_bench." "$FILE"



