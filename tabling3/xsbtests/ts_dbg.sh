
XSBDIR=$1

 echo "--------------- dbg test -------------------------"
 
 cd ../XSB/build
 
 rm ../config/*dbg/saved.o/*.o ; 
# configure --enable-debug > /tmp/config ; 
 ./configure --enable-debug ; 
 ./makexsb --config-tag=dbg ;
 
 cd ../../xsbtests
 
 sh testsuite.sh -tag dbg  $XSBDIR
