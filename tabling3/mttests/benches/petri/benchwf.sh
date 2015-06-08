XEMU=$1
MAX=$2

FILE=./wfRes.P

rm -f $FILE

nthreads=1
while [ $nthreads -le $MAX ]
do

echo "-------------------- Testing $nthreads threads ------------------------------"

	../genbench.sh "$XEMU" "[benchmark_wf], gen_private_wf." \
			"private_scale_wf($nthreads)." "$FILE"

	../genbench.sh "$XEMU" "[benchmark_wf], gen_shared_wf($MAX)." \
			"shared_bench_wf($nthreads,$MAX)." "$FILE"


	nthreads=$[$nthreads*2]
done

