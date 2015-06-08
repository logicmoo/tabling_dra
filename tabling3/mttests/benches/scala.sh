
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

(cd prolog_benches; ./test_scal.sh $PREF ../$DIR $MAX )
(cd synth_benches; ./test_scal.sh $PREF ../$DIR $MAX )
(cd tab_benches; ./test_scal.sh $PREF ../$DIR $MAX )
(cd shared_benches; ./test_scal.sh $PREF ../$DIR $MAX )
