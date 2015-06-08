XEMU=$1
MAX=$2

FILE=./prefsRes.P

rm -f $FILE

nthreads=1
while [ $nthreads -le $MAX ]
do

echo "-------------------- Testing $nthreads threads ------------------------------"

	../genbench.sh "$XEMU" "[benchmark_prefs]." \
			"private_scale_prefs(199,$nthreads,$MAX)." "$FILE"

	../genbench.sh "$XEMU" "[benchmark_prefs]." \
			"shared_bench_prefs(199,$nthreads,$MAX)." "$FILE"

	nthreads=$[$nthreads*2]
done

