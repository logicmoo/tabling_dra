PREF=$1
DIR=$2

rm -rf $DIR
mkdir $DIR

./mut_test.sh $PREF/bin/xsb-mt $DIR/mut-prolog.txt
