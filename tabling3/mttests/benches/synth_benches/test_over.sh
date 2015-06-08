PREF=$1
DIR=$2

./test.sh $PREF/bin/xsb-dflt $DIR/synth-seq-loc.P
#./test.sh $PREF/bin/xsb-btc $DIR/synth-seq-bat.P
./test.sh $PREF/bin/xsb-mt $DIR/synth-mt-p-loc.P
./test1.sh $PREF/bin/xsb-mt $DIR/synth-mt-s-loc.P
#./test.sh $PREF/bin/xsb-btc-ccmpl $DIR/synth-mt-p-bat.P
#./test1.sh $PREF/bin/xsb-btc-ccmpl $DIR/synth-mt-s-bat.P
