#! /bin/sh

XEMU=$1
FILE=$2
NUM=$3
OPT=$4

echo "-------------------------------------------------------"
echo "--- Running benches/shared_benches/mttest.sh        ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../genbench.sh "$XEMU --max_threads 4096"  "[prod_cons]." \
		"mt_bench([prod_cons],1000000,$NUM)." "$FILE"
 ../genbench.sh "$XEMU --max_threads 4096 -c 4096" \
		"[trans_clos],readGraph(g256x128)." \
		"mt_benches(g256x128,256,$NUM)." "$FILE"
 ../genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos],readGraph(g512x8)." \
		"mt_benches(g512x8,512,$NUM)." "$FILE"
 ../genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos],readGraph(g2048x2)." \
		"mt_benches(g2048x2,2048,$NUM)." "$FILE"
 ../genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos],readGraph(g8192x1)." \
		"mt_benches(g8192x1,8192,$NUM)." "$FILE"
