XEMU=$1
MAX=$2

FILE=./constrRes.P

rm -f $FILE

nthreads=1
while [ $nthreads -le $MAX ]
do

echo "-------------------- Testing $nthreads threads ------------------------------"

	../genbench.sh "$XEMU" "[benchmark_constraint]." \
			"private_scale_constr(301,$nthreads,1)." "$FILE"

	../genbench.sh "$XEMU" "[benchmark_constraint]." \
			"shared_bench_constr(301,$nthreads,$MAX)." "$FILE"


	nthreads=$[$nthreads*2]
done

