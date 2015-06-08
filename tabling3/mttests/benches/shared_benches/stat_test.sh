#! /bin/sh

XEMU=$1
FILE=$2
NUM=$3
OPT=$4

echo "-------------------------------------------------------"
echo "--- Running benches/shared_benches/stat_test.sh     ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../stat_genbench.sh "$XEMU --max_threads 4096 -c 4096" \
		"[trans_clos]" "mt_mem_bench_ls(g256x128,256,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 -c 4096" \
		"[trans_clos]" "mt_mem_bench_lp(g256x128,256,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 -c 4096" \
		"[trans_clos]" "mt_mem_bench_rs(g256x128,256,$NUM)" "$FILE"
if [ $NUM -le 16 ]
then
 ../stat_genbench.sh "$XEMU --max_threads 4096 -c 4096" \
		"[trans_clos]" "mt_mem_bench_rp(g256x128,256,$NUM)" "$FILE"
fi
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_ls(g512x8,512,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_lp(g512x8,512,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rs(g512x8,512,$NUM)" "$FILE"
if [ $NUM -le 16 ]
then
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rp(g512x8,512,$NUM)" "$FILE"
fi
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_ls(g2048x2,2048,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_lp(g2048x2,2048,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rs(g2048x2,2048,$NUM)" "$FILE"
if [ $NUM -le 16 ]
then
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rp(g2048x2,2048,$NUM)" "$FILE"
fi
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_ls(g8192x1,8192,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_lp(g8192x1,8192,$NUM)" "$FILE"
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rs(g8192x1,8192,$NUM)" "$FILE"
if [ $NUM -le 16 ]
then
 ../stat_genbench.sh "$XEMU --max_threads 4096 $OPT" \
		 "[trans_clos]" "mt_mem_bench_rp(g8192x1,8192,$NUM)" "$FILE"
fi
