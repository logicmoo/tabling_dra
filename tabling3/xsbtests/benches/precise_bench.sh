#! /bin/sh

DIR=$1
TEST=$2
EMULATOR=$3
FILE=$4

$EMULATOR  << EOF

halt.
EOF

cd $DIR

# start with fresh accumulator files.
if test -f "$FILE" ; then
    rm -f "$FILE"
    rm -f "$FILE"_tmp
fi


x=96

# Test with different memory configurations: keep results in FILE
while test $x -le 136
do
    $TEST "$EMULATOR  -o $x" $FILE
    x=`expr $x + 4`
done

# Summarize results to screen.

$EMULATOR <<EOF
 
['../get_variance.P'].

['$FILE'].

get_variance.

EOF


