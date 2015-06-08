XEMU=$1
MAX=$2
N=$3

# TLS currently testing with N = 8000 for private

FILE=./omegaRes.P

rm -f $FILE

nthreads=1
while [ $nthreads -le $MAX ]
do

echo "-------------------- Testing $nthreads threads ------------------------------"

	../genbench.sh "$XEMU" "[benchmark_omega], gen_private_omega(7000)." \
			"alt_private_scale_omega($nthreads)." "$FILE"

	nthreads=$[$nthreads*2]
done

