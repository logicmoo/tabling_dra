PREF=$1
DIR=$2
MAX=$3

XEMU=$PREF/bin/xsb-mt
FILE=$DIR/graph.P

$XEMU << EOF
[make_graphs].

make_big_graphs.
EOF

nthreads=1
while [ $nthreads -le $MAX ]
do

        ../genbench.sh "$XEMU" "[trans_clos],readGraph(g8192x2)." \
		"mt_benches_2(g8192x2,8192,$nthreads)." "$FILE"
	nthreads=$[$nthreads*2]
done

