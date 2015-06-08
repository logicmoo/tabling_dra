#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running basic_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

    # XEMU and options must be together in quotes
../gentest.sh "$XEMU $options" tsstr13 "testcombo."
../gentest.sh "$XEMU $options" tsstr23 "testcombo."
../gentest.sh "$XEMU $options" tsstr33 "testcombo."
#------------------------------------
# simplest series
#------------------------------------
../gentest.sh "$XEMU $options" tstr11 "tw,fail."
../gentest.sh "$XEMU $options" tstr21 "tw,fail."
../gentest.sh "$XEMU $options" tstr31 "tw,fail."
../gentest.sh "$XEMU $options" tstr51 "tw,fail."
../gentest.sh "$XEMU $options" tstr61 "tw,fail."
#------------------------------------
# simple series
#------------------------------------
../gentest.sh "$XEMU $options" tstr12 "testcombo."
../gentest.sh "$XEMU $options" tstr22 "testcombo."
../gentest.sh "$XEMU $options" tstr32 "testcombo."
../gentest.sh "$XEMU $options" tstr52 "testcombo."
../gentest.sh "$XEMU $options" tstr62 "testcombo."
#------------------------------------
# transitive closure series
#------------------------------------
../gentest.sh "$XEMU $options" tstr13 "tw."
../gentest.sh "$XEMU $options" tstr23 "tw."
../gentest.sh "$XEMU $options" tstr33 "tw."
../gentest.sh "$XEMU $options" tstr53 "tw."
../gentest.sh "$XEMU $options" tstr63 "tw."
#------------------------------------
# loader/compiler series
#------------------------------------
../gentest.sh "$XEMU $options" thstr13 "tw."
../gentest.sh "$XEMU $options" thstr43 "tw."
#------------------------------------
# simple levels series
#------------------------------------
../gentest.sh "$XEMU $options" thstr23 "tw1."
#------------------------------------
# cylinder series
#------------------------------------
../gentest.sh "$XEMU $options" tcyl11 "['tcyl-24-24-2'],tw(1)."
../gentest.sh "$XEMU $options" tcyl12 "['tcyl-24-24-2'],tw(1)."
../gentest.sh "$XEMU $options" testsg "[acyl,'tcyl-24-24-2'],tw."
#------------------------------------
# interpreter series.
#------------------------------------
../gentest.sh "$XEMU $options" interp "test."
#------------------------------------
# tabletrysingle tests.
#------------------------------------
../gentest.sh "$XEMU $options" tsing1 "a(X,Y),write(X),write(' '),write(Y),nl,fail."
#--------------------------------
# h series is hilog/tabling tests
#------------------------------------
../gentest.sh "$XEMU $options" hirc "tc(manage)(X,Y),write(X),write(' '),write(Y),nl,fail."
#------------------------------------
../gentest.sh "$XEMU $options" findall "test."
#------------------------------------
../gentest.sh "$XEMU $options" altindex "test."
#------------------------------------
../gentest.sh "$XEMU $options" longname "test."
#------------------------------------
../gentest.sh "$XEMU $options" gctest "test."
