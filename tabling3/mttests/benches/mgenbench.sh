#! /bin/sh

# $1 is expected to have xsb executable + command line options
EMU=$1
COMP_CMD=$2
CMD=$3
FILE=$4

DIR=`pwd`
BASEDIR=`basename $DIR`

#echo "--------------------------------------------------------------------"
#echo "Benching $BASEDIR/$FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

memusage $EMU -g none << EOF &> mu.tmp
$COMP_CMD
$CMD
EOF

if grep memsuccess mu.tmp &> /dev/null
then
	SUCC=""
else
	SUCC="(FAILED)"
fi

HEAP_PEAK=`grep "heap peak:" mu.tmp | awk '{ print $9 }' | awk -F, '{ print $1 }'`
STACK_PEAK=`grep "heap peak:" mu.tmp | awk '{ print $12 }'`

rm mu.tmp

echo "$CMD": heap peak: $HEAP_PEAK stack peak: $STACK_PEAK $SUCC >> $FILE
