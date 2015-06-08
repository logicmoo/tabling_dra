#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running cdf_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

# XEMU and options must be together in quotes
#------------------------------------
# Tests of theorem prover
#------------------------------------
../loadtest.sh "$XEMU $options" ce_test "testall."
../loadtest.sh "$XEMU $options" cdftp_test "oneclicktest."

#------------------------------------
# Tests of Type 0 interface
#------------------------------------
../gentest.sh "$XEMU $options" type0QueryExt "testreturns."
../gentest.sh "$XEMU $options" type0QueryInt "testreturns."
../gentest.sh "$XEMU $options" dlQuery "testreturns."

#------------------------------------
# Tests of Updates
#------------------------------------
../gentest.sh "$XEMU $options" updateExt "testreturns."
../gentest.sh "$XEMU $options" updateInt "testreturns."
../gentest.sh "$XEMU $options" updateDL "testreturns."

#------------------------------------
# Tests of checks
#------------------------------------
../gentest.sh "$XEMU $options" checks "testreturns."

#------------------------------------
# Tests of I/O
#------------------------------------
./ioExt.sh "$XEMU $options" ioExt "testreturns."
# inttest doesn't sort 
./ioInt.sh "$XEMU $options" ioInt "testreturns."

#------------------------------------
# Component Tests
#------------------------------------
# Test creating a new component.
./compUpd1.sh "$XEMU $options"
# Load newly created component and update in place.
./compUpd2.sh "$XEMU $options"
# Test merge
./compUpd3.sh "$XEMU $options"