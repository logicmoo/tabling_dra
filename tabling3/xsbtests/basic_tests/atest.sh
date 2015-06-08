#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running basic_tests/test.sh using call abstraction        ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

    # XEMU and options must be together in quotes
../agentest.sh "$XEMU $options" tsstr13 "testcombo."
../agentest.sh "$XEMU $options" tsstr23 "testcombo."
../agentest.sh "$XEMU $options" tsstr33 "testcombo."
#------------------------------------
# simplest series
#------------------------------------
../agentest.sh "$XEMU $options" tstr11 "tw,fail."
../agentest.sh "$XEMU $options" tstr21 "tw,fail."
../agentest.sh "$XEMU $options" tstr31 "tw,fail."
../agentest.sh "$XEMU $options" tstr51 "tw,fail."
../agentest.sh "$XEMU $options" tstr61 "tw,fail."
#------------------------------------
# simple series
#------------------------------------
../agentest.sh "$XEMU $options" tstr12 "testcombo."
../agentest.sh "$XEMU $options" tstr22 "testcombo."
../agentest.sh "$XEMU $options" tstr32 "testcombo."
../agentest.sh "$XEMU $options" tstr52 "testcombo."
../agentest.sh "$XEMU $options" tstr62 "testcombo."
#------------------------------------
# transitive closure series
#------------------------------------
../agentest.sh "$XEMU $options" tstr13 "tw."
../agentest.sh "$XEMU $options" tstr23 "tw."
../agentest.sh "$XEMU $options" tstr33 "tw."
../agentest.sh "$XEMU $options" tstr53 "tw."
../agentest.sh "$XEMU $options" tstr63 "tw."
#------------------------------------
# loader/compiler series
#------------------------------------
../agentest.sh "$XEMU $options" thstr13 "tw."
../agentest.sh "$XEMU $options" thstr43 "tw."
#------------------------------------
# simple levels series
#------------------------------------
../agentest.sh "$XEMU $options" thstr23 "tw1."
#------------------------------------
# cylinder series
#------------------------------------
../agentest.sh "$XEMU $options" tcyl11 "['tcyl-24-24-2'],tw(1)."
../agentest.sh "$XEMU $options" tcyl12 "['tcyl-24-24-2'],tw(1)."
../agentest.sh "$XEMU $options" testsg "[acyl,'tcyl-24-24-2'],tw."
#------------------------------------
# interpreter series.
#------------------------------------
../agentest.sh "$XEMU $options" interp "test."
#------------------------------------
# tabletrysingle tests.
#------------------------------------
../agentest.sh "$XEMU $options" tsing1 "a(X,Y),write(X),write(' '),write(Y),nl,fail."
#--------------------------------
# h series is hilog/tabling tests
#------------------------------------
../agentest.sh "$XEMU $options" hirc "tc(manage)(X,Y),write(X),write(' '),write(Y),nl,fail."
#------------------------------------
../agentest.sh "$XEMU $options" findall "test."
#------------------------------------
../agentest.sh "$XEMU $options" altindex "test."
#------------------------------------
../agentest.sh "$XEMU $options" longname "test."
#------------------------------------
../agentest.sh "$XEMU $options" gctest "test."

../agentest.sh "$XEMU $options" xpp_on_test "test."
