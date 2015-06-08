
XSBDIR=$1

 echo "--------------- dbg test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*dbg/saved.o/*.o ; 
# configure --enable-debug > /tmp/config ; 
 ./configure --enable-debug ; 
 ./makexsb --config-tag=dbg ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag dbg  $XSBDIR
 
echo "--------------- opt test -------------------------"

cd ../XSB/build

rm ../config/*-dflt/saved.o/*.o ; 
./configure --with-config-tag=dflt > /tmp/config ; 
./makexsb --config-tag=dflt ;

cd ../../xsbtests

sh testsuite.sh -tag "dflt"  $XSBDIR

 echo "--------------- mt-opt test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*mt/saved.o/*.o ; 
 ./configure --enable-mt  > /tmp/config ; 
 ./makexsb --config-tag=mt ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag mt -exclude "regmatch_tests incremental_tests" $XSBDIR
 
echo "--------------- 32 test -------------------------"

cd ../XSB/build

rm ../config/*-bits32/saved.o/*.o ; 
./configure --with-bits32 > /tmp/config ; 
./makexsb --config-tag=bits32 ;

cd ../../xsbtests

sh testsuite.sh -tag "bits32"  $XSBDIR

 echo "--------------- mt-dbg test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*dbg-mt/saved.o/*.o ; 
 ./configure --enable-mt --enable-debug > /tmp/config ; 
 ./makexsb --config-tag=dbg-mt ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag dbg-mt -exclude "regmatch_tests incremental_tests"  $XSBDIR
 
 echo "--------------- batched test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*btc/saved.o/*.o ; 
 ./configure --enable-batched  > /tmp/config ; 
 ./makexsb --config-tag=btc ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag btc -exclude "wfs_tests_subsumption delay_tests_subsumption" $XSBDIR
 
