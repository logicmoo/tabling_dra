#! /bin/sh

XEMU=$1
FILE=prologRes.P

echo "-------------------------------------------------------"
echo "--- Running benches/prolog_benches/test.sh          ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
../genbench.sh "$XEMU"  "[deriv]." "bench_deriv([deriv],600,1000)." "$FILE"
../genbench.sh "$XEMU"  "[nreverse]." "bench_nrev([nrev],300,600)."  "$FILE"
../genbench.sh "$XEMU -m 4096"  "[qsort]." "bench_qsort([qsort],20,1000)." "$FILE"
../genbench.sh "$XEMU"  "[serial]." "bench_serialise([serialise],80,3000)." "$FILE"
../genbench.sh "$XEMU "  "[query]." "bench_query([query],3000)." "$FILE"
../genbench.sh "$XEMU "  "[tak]." "bench_tak([tak],1)." "$FILE"
../genbench.sh "$XEMU "  "[compiler]." "bench_compiler([compiler])." "$FILE"
../genbench.sh "$XEMU "  "[constr]." "test." "$FILE"
../genbench.sh "$XEMU "  "[time_call]." "test." "$FILE"
../genbench.sh "$XEMU "  "[time_assert]." "cpubench_private_assertR(100000)." "$FILE"
../genbench.sh "$XEMU "  "[time_assert]." "cpubench_share_assertR(100000)." "$FILE"
