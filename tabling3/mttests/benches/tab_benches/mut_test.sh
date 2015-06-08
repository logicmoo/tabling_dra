#! /bin/sh

XEMU=$1
FILE=$2

echo "-------------------------------------------------------"
echo "--- Running benches/tab_benches/mut_test.sh         ---"
echo "-------------------------------------------------------"

# XEMU and options must be together in quotes
 ../mut_genbench.sh "$XEMU"  "[cs_o]." "bench([cs_o],200)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[cs_r]." "bench([cs_r],100)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[disj]." "bench([disj],400)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[gabriel]." "bench([gabriel],280)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[kalah]." "bench([kalah],300)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[peep]." "bench([peep],100)." "$FILE"
 ../mut_genbench.sh "$XEMU"  "[pg]." "bench([pg],400)." "$FILE"



