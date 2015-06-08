PREF=$1
DIR=$2

./mut_test.sh $PREF/bin/xsb-mt $DIR/mut-synth-p-loc.txt
./mut_test.sh $PREF/bin/xsb-btc-ccmpl $DIR/mut-synth-p-bat.txt

./mut_test_s.sh $PREF/bin/xsb-mt $DIR/mut-synth-s-loc.txt
./mut_test_s.sh $PREF/bin/xsb-btc-ccmpl $DIR/mut-synth-s-bat.txt
