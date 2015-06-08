PREF=$1
DIR=$2

./test.sh $PREF/bin/xsb-dflt $DIR/tab-seq-loc.P
#./test.sh $PREF/bin/xsb-btc $DIR/tab-seq-bat.P
./test.sh $PREF/bin/xsb-mt $DIR/tab-mt-p-loc.P
./test_s.sh $PREF/bin/xsb-mt $DIR/tab-mt-s-loc.P
#./test.sh $PREF/bin/xsb-btc-ccmpl $DIR/tab-mt-p-bat.P
#./test_s.sh $PREF/bin/xsb-btc-ccmpl $DIR/tab-mt-s-bat.P
