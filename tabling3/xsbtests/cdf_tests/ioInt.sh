#! /bin/sh
# this does the same as gentest.sh except it doesn't sort.

# $1 is expected to have xsb ececutable + command line options
EMU=$1
FILE=$2
CMD=$3

DIR=`pwd`
BASEDIR=`basename $DIR`

echo "--------------------------------------------------------------------"
echo "Testing $BASEDIR/$FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

# print out differences.
if test -f ${FILE}_new; then
	rm -f ${FILE}_new
fi
    
$EMU << EOF
[$FILE].

$CMD

EOF

#-----------------------
# print out differences.
#-----------------------
status=0
diff -w ${FILE}_new ${FILE}_old || status=1
if test "$status" = 0 ; then 
	echo "$BASEDIR/$FILE tested"
	rm -f ${FILE}_new
else
	echo "$BASEDIR/$FILE differ!!!"
	diff -w ${FILE}_new ${FILE}_old
fi

rm -f temp
