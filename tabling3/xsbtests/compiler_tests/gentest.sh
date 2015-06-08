#! /bin/sh

# $1 is expected to have xsb ececutable + command line options
EMU=$1
FILE=$2

DIR=`pwd`
BASEDIR=`basename $DIR`

OBJEXT=xwam

echo "--------------------------------------------------------------------"

echo "Testing $BASEDIR/$FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

$EMU << EOF

compile($FILE,[spec_dump,table_dump,ti_dump]).

EOF

d=`diff -w ${FILE}.$OBJEXT ${FILE}_${OBJEXT}_old`
if test -z "$d"; then 
	echo "$BASEDIR/$FILE$.{OBJEXT} tested"
	rm -f ${FILE}.${OBJEXT}
else
	echo "$BASEDIR/$FILE differ!!!"
	diff -w ${FILE}.${OBJEXT} ${FILE}_${OBJEXT}_old
fi

d=`diff -w ${FILE}.spec ${FILE}.spec.old`
if test -z "$d"; then 
	echo "$BASEDIR/$FILE.spec tested"
	rm -f ${FILE}.spec
else
	echo "$BASEDIR/$FILE differ!!!"
	diff -w ${FILE}.spec ${FILE}.spec.old
fi

d=`diff -w ${FILE}.table ${FILE}.table.old`
if test -z "$d"; then 
	echo "$BASEDIR/$FILE.table tested"
	rm -f ${FILE}.table
else
	echo "$BASEDIR/$FILE differ!!!"
	diff -w ${FILE}.table ${FILE}.table.old
fi

d=`diff -w ${FILE}.ti ${FILE}.ti.old`
if test -z "$d"; then 
	echo "$BASEDIR/$FILE.ti tested"
	rm -f ${FILE}.ti
else
	echo "$BASEDIR/$FILE differ!!!"
	diff -w ${FILE}.ti ${FILE}.ti.old
fi
