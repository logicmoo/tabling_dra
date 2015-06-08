XEMU=$1
MAX=$2

#XEMU=$PREF/bin/xsb-mt
FILE=./elemResDyn.P

rm -f $FILE

nthreads=1
while [ $nthreads -le $MAX ]
do
echo "-------------------- Testing $nthreads threads ------------------------------"
    ../genbench.sh "$XEMU" "[benchmark_elem], gen_proc_2(100000,$MAX)." \
"bench_private_process_dyn($nthreads)." "$FILE"

    ../genbench.sh "$XEMU" "[benchmark_elem], gen_proc_2(100000,$MAX)." \
"bench_shared_process_dyn($nthreads,$MAX)." "$FILE"

nthreads=$[$nthreads*2]
done

