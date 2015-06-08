PREF=$1
DIR=$2
MAX=$3

XEMU=$PREF/bin/xsb-mt
FILE=$DIR/petri.P

nthreads=1
while [ $nthreads -le $MAX ]
do

	../genbench.sh "$XEMU" "[benchmark_elem], gen_2($MAX)." \
			"bench_2($nthreads,$MAX)." "$FILE"

	../genbench.sh "$XEMU" "[benchmark_wf], gen_2." \
			"bench_2($nthreads)." "$FILE"

	nthreads=$[$nthreads*2]
done
