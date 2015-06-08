#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running sem_tests/test.sh                       ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2


# XEMU and options must be together in quotes
./CSH/all_sem.sh "$XEMU $options"
