DIR=~/NEWXSB/XSB
BENCH_DIR=$1
FILE=$2

$DIR/bin/xsb <<EOF >$FILE
['over-analise'].
cd('$BENCH_DIR').
table_all.
EOF
