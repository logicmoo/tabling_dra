PREF=$1
DIR=$2

./test.sh $PREF/bin/xsb $DIR/tab-seq-loc.P
./test.sh $PREF/bin/xsb-mt $DIR/tab-mt-p-loc.P
./test_s.sh $PREF/bin/xsb-mt $DIR/tab-mt-s-loc.P
