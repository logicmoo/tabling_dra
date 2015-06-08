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

$TEST "$EMULATOR" "$FILE"

