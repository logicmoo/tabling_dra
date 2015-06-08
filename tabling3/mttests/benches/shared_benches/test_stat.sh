PREF=$1
DIR=$2
MAX=$3

./make_graphs.sh $PREF/bin/xsb

nthreads=1
while [ $nthreads -le $MAX ]
do
	if [ $nthreads -le 256 ]
	then CP="-c 2048"
	else CP=""
	fi

	./stat_test.sh $PREF/bin/xsb-mt \
		    $DIR/stat-local.txt $nthreads
	./stat_test.sh $PREF/bin/xsb-btc-ccmpl \
		    $DIR/stat-batched.txt $nthreads "$CP"
	nthreads=$[$nthreads*2]
done

