PREF=$1
DIR=$2

./make_graphs.sh $PREF/bin/xsb-dflt

./test_seq.sh	$PREF/bin/xsb-dflt	$DIR/shared-seq-loc.P
#./test_seq.sh	$PREF/bin/xsb-btc	$DIR/shared-seq-bat.P
./test_priv.sh	$PREF/bin/xsb-mt	$DIR/shared-mt-p-loc.P
#./test_priv.sh	$PREF/bin/xsb-btc-ccmpl	$DIR/shared-mt-p-bat.P
./test_sh.sh	$PREF/bin/xsb-mt	$DIR/shared-mt-s-loc.P
#./test_sh.sh	$PREF/bin/xsb-btc-ccmpl	$DIR/shared-mt-s-bat.P
