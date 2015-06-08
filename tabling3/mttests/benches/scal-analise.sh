BENCH_DIR=$1
FILE=$2
EMUDIR=$3

if test -z "$EMUDIR$"
then	EMUDIR=~/NEWXSB/XSB
fi

$EMUDIR/bin/xsb <<EOF
['scal-analise'].
tell('$FILE').
cd('$BENCH_DIR').
table_all.
told.
EOF
