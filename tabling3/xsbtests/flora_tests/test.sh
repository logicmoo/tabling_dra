#! /bin/sh

#============================================================================
echo "-------------------------------------------------------"
echo "--- Running flora_tests/test.sh                       ---"
echo "-------------------------------------------------------"

XEMU=$1
XSBBINDIR=`dirname $XEMU`
FLORADIR=$XSBBINDIR/../../../packages/flora2

echo $XSBBINDIR
echo $FLORADIR
./testsuite.sh $FLORADIR
