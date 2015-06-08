#! /bin/sh

XEMU=$1
FILE=$2
NUM=$3

echo "-------------------------------------------------------"
echo "--- Running benches/prolog_benches/mttest.sh        ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
../genbench.sh "$XEMU"  "[deriv]." "mt_bench_deriv([deriv],300,250,$NUM)." "$FILE"
../genbench.sh "$XEMU"  "[nreverse]." "mt_bench_nrev([nrev],150,300,$NUM)."  "$FILE"
../genbench.sh "$XEMU -m 4096"  "[qsort]." "mt_bench_qsort([qsort],2,1000,$NUM)." "$FILE"
../genbench.sh "$XEMU"  "[serial]." "mt_bench_serialise([serialise],10,3000,$NUM)." "$FILE"
../genbench.sh "$XEMU "  "[query]." "mt_bench_query([query],375,$NUM)." "$FILE"
../genbench.sh "$XEMU "  "[tak]." "mt_bench_tak([tak],1,$NUM)." "$FILE"
# ../genbench.sh "$XEMU "  "[compiler]." "bench_compiler([compiler])." "$FILE"
#../genbench.sh "$XEMU "  "[constr]." "mttest($NUM)." "$FILE"
