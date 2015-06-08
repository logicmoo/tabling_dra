PREF=$1
DIR=$2

. ../funcs.sh

min $3 64
MAX=$?

nthreads=1
while [ $nthreads -le $MAX ]
do
	./mttest.sh $PREF/bin/xsb-mt $DIR/prolog.P $nthreads 
	nthreads=$[$nthreads + 1]
done

