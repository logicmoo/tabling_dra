#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/prolog_benches/test.sh          ---"
echo "$XEMU $2"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
../genbench.sh "$XEMU"  "[deriv]." "bench_deriv([deriv],100,500)." "$FILE"
../genbench.sh "$XEMU"  "[nreverse]." "bench_nrev([nrev],50,300)."  "$FILE"
../genbench.sh "$XEMU"  "[qsort]." "bench_qsort([qsort],1,1000)." "$FILE"
../genbench.sh "$XEMU"  "[serialise]." "bench_serialise([serialise],10,3000)." "$FILE"
../genbench.sh "$XEMU "  "[query]." "bench_query([query],500)." "$FILE"
../genbench.sh "$XEMU "  "[tak]." "bench_tak([tak],1)." "$FILE"
../genbench.sh "$XEMU "  "[compiler]." "bench_compiler([compiler])." "$FILE"
