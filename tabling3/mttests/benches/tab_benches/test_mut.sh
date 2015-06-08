PREF=$1
DIR=$2

./mut_test.sh $PREF/bin/xsb-mt $DIR/mut-tab-p-loc.txt
./mut_test.sh $PREF/bin/xsb-btc-ccmpl $DIR/mut-tab-p-bat.txt

./mut_test_s.sh $PREF/bin/xsb-mt $DIR/mut-tab-s-loc.txt
./mut_test_s.sh $PREF/bin/xsb-btc-ccmpl $DIR/mut-tab-s-bat.txt
