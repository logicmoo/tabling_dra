#! /bin/sh

# $1 is expected to have xsb ececutable + command line options
EMU=$1
FILE=$2
CMD=$3

DIR=`pwd`
BASEDIR=`basename $DIR`

echo "--------------------------------------------------------------------"
echo "Testing $BASEDIR/$FILE"
#echo "$EMU"     # debug check: verify that options have been passed to xsb

$EMU << EOF
[$FILE].
tell(temp).
$CMD
told.
EOF

