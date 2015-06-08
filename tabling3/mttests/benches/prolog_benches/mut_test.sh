#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/prolog_benches/mut_test.sh      ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
../mut_genbench.sh "$XEMU"  "[deriv]." "bench_deriv([deriv],600,1000)." "$FILE"
../mut_genbench.sh "$XEMU"  "[nreverse]." "bench_nrev([nrev],300,600)."  "$FILE"
../mut_genbench.sh "$XEMU -m 4096"  "[qsort]." "bench_qsort([qsort],20,1000)." "$FILE"
../mut_genbench.sh "$XEMU"  "[serial]." "bench_serialise([serialise],80,3000)." "$FILE"
../mut_genbench.sh "$XEMU "  "[query]." "bench_query([query],3000)." "$FILE"
../mut_genbench.sh "$XEMU "  "[tak]." "bench_tak([tak],1)." "$FILE"
../mut_genbench.sh "$XEMU "  "[compiler]." "bench_compiler([compiler])." "$FILE"
