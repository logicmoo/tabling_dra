
XSBDIR=$1

 echo "--------------- mt-dbg test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*dbg-mt/saved.o/*.o ; 
 ./configure --enable-mt --enable-debug > /tmp/config ; 
 ./makexsb --config-tag=dbg-mt ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag dbg-mt -exclude "regmatch_tests incremental_tests"  $XSBDIR
 
