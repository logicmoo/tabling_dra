#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running benches/xmc/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
FILE=$2

# XEMU and options must be together in quotes
../genbench.sh "$XEMU"  "[main]." "bench_leader." "$FILE"
../genbench.sh "$XEMU"  "[main]." "bench_sieve." "$FILE"
