
if test $# -ne 2 
then
echo usage $0 PROLOG_DIR BENCH_DIR
exit
fi

PREF=$1
DIR=$2

if test \! -d $DIR
then
echo $DIR not a directory
exit
fi

(cd prolog_benches; ./test_over.sh $PREF ../$DIR )
(cd synth_benches; ./test_over_nb.sh $PREF ../$DIR )
(cd tab_benches; ./test_over_nb.sh $PREF ../$DIR )
