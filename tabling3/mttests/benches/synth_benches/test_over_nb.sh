PREF=$1
DIR=$2

./test.sh $PREF/bin/xsb $DIR/synth-seq-loc.P
./test.sh $PREF/bin/xsb-mt $DIR/synth-mt-p-loc.P
./test1.sh $PREF/bin/xsb-mt $DIR/synth-mt-s-loc.P
