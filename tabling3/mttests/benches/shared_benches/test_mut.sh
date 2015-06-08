PREF=$1
DIR=$2

./make_graphs.sh $PREF/bin/xsb

./mut_test.sh	$PREF/bin/xsb-mt	$DIR/mut-shared-s-loc.txt
./mut_test.sh	$PREF/bin/xsb-btc-ccmpl	$DIR/mut-shared-s-bat.txt

./mut_testp.sh	$PREF/bin/xsb-mt	$DIR/mut-shared-p-loc.txt
./mut_testp.sh	$PREF/bin/xsb-btc-ccmpl	$DIR/mut-shared-p-bat.txt
