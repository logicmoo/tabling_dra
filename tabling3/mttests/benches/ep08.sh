
if test $# -ne 3 
then
echo usage $0 PROLOG_DIR BENCH_DIR MAX_THREADS
exit
fi

PREF=$1
DIR=$2
MAX=$3

if test ! -d $DIR
then
echo $DIR not a directory
exit
fi

(cd shared_benches; ./ep08g.sh $PREF ../$DIR $MAX )
(cd petri; ./ep08p.sh $PREF ../$DIR $MAX )
