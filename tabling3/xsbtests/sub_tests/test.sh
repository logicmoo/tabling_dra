#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running Subsumption Tests                       ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2

## !! and to SORT the output !!

# Left-Recursive Transitive Closure
# ---------------------------------
../gentest.sh "$XEMU $options -S" lrtc1 "go."
../gentest.sh "$XEMU $options -S" lrtc2 "go."
../gentest.sh "$XEMU $options -S" lrtc3 "go."
../gentest.sh "$XEMU $options -S" lrtc4 "go."
../gentest.sh "$XEMU $options -S" lrtc5 "go."
../gentest.sh "$XEMU $options -S" lrtc6 "go."
../gentest.sh "$XEMU $options -S" lrtc7 "go."
../gentest.sh "$XEMU $options -S" lrtc8 "go."

# Right-Recursive Transitive Closure
# ----------------------------------
../gentest.sh "$XEMU $options -S" rrtc1 "go."
../gentest.sh "$XEMU $options -S" rrtc2 "go."
../gentest.sh "$XEMU $options -S" rrtc3 "go."
../gentest.sh "$XEMU $options -S" rrtc4 "go."
../gentest.sh "$XEMU $options -S" rrtc5 "go."
../gentest.sh "$XEMU $options -S" rrtc6 "go."
../gentest.sh "$XEMU $options -S" rrtc7 "go."
../gentest.sh "$XEMU $options -S" rrtc8 "go."

# Double-Recursive Transitive Closure
# -----------------------------------
../gentest.sh "$XEMU $options -S" drtc1 "go."
../gentest.sh "$XEMU $options -S" drtc2 "go."
../gentest.sh "$XEMU $options -S" drtc3 "go."
../gentest.sh "$XEMU $options -S" drtc4 "go."
../gentest.sh "$XEMU $options -S" drtc5 "go."
../gentest.sh "$XEMU $options -S" drtc6 "go."
../gentest.sh "$XEMU $options -S" drtc7 "go."
../gentest.sh "$XEMU $options -S" drtc8 "go."

# Same Generation
# ---------------
../gentest.sh "$XEMU $options -S" sg1 "go."
../gentest.sh "$XEMU $options -S" sg2 "go."
../gentest.sh "$XEMU $options -S" sg3 "go."
../gentest.sh "$XEMU $options -S" sg4 "go."
../gentest.sh "$XEMU $options -S" sg5 "go."
../gentest.sh "$XEMU $options -S" sg6 "go."
../gentest.sh "$XEMU $options -S" sg7 "go."
../gentest.sh "$XEMU $options -S" sg8 "go."

# Genome
# ------
../gentest.sh "$XEMU $options -S" genome1 "go."
../gentest.sh "$XEMU $options -S" genome2 "go."
../gentest.sh "$XEMU $options -S" genome3 "go."

# Complex
# -------
../gentest.sh "$XEMU -m  1500 -c  5000 -o 2000 $options -S" floratest "go."
../gentest.sh "$XEMU -m 10000 -c 60000 -o 4000 $options -S" decker    "go."
../gentest.sh "$XEMU $options -S" pilegaard "test."
