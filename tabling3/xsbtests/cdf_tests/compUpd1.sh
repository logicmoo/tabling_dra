#! /bin/sh

EMU=$1

# print out differences.
if test -f ${FILE}_new; then
	rm -f ${FILE}_new
fi
    
rm -rf comp_outdir*

echo "--------------------------------------------------------------------"
echo "First Comp Update Test"

$EMU << EOF

[compUpd1].

testreturns.

EOF

#-----------------------
# print out differences.  Compare 0 versions for test and testo1 in
#    _testdir to support update in second stage. 
#-----------------------

# first do the component: test.
status=0
diff -w comp_testdir/test_0/cdf_extensional.P comp_outdir/test/cdf_extensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/test/cdf_extensional.P tested"
else
	echo "compUpd1: comp_testdir/test/cdf_extensional.P differs!!!"
	diff -w comp_testdir/test_0/cdf_extensional.P comp_outdir/test/cdf_extensional.P  
fi

status=0
diff -w comp_testdir/test_0/cdf_intensional.P comp_outdir/test/cdf_intensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/test/cdf_intensional.P tested"
else
	echo "compUpd1: comp_testdir/test/cdf_intensional.P differ!!!"
	diff -w comp_testdir/test_0/cdf_intensional.P comp_outdir/test/cdf_intensional.P  
fi

#----------------------------------------------------------------------
# next do the component test_int
status=0
diff -w comp_testdir/test_int/cdf_extensional.P comp_outdir/test_int/cdf_extensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/test_int/cdf_extensional.P tested"
else
	echo "compUpd1: comp_testdir/test_int/cdf_extensional.P differ!!!"
	diff -w comp_testdir/test_int/cdf_extensional.P comp_outdir/test_int/cdf_extensional.P  
fi

status=0
diff -w comp_testdir/test_int/cdf_intensional.P comp_outdir/test_int/cdf_intensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/test_int/cdf_intensional.P tested"
else
	echo "compUpd1: comp_testdir/test_int/cdf_intensional.P differ!!!"
	diff -w comp_testdir/test_int/cdf_intensional.P comp_outdir/test_int/cdf_intensional.P  
fi

#----------------------------------------------------------------------
# next do the component testo1
status=0
diff -w comp_testdir/testo1_0/cdf_extensional.P comp_outdir/testo1/cdf_extensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/testo1/cdf_extensional.P tested"
else
	echo "compUpd1: comp_testdir/testo1/cdf_extensional.P differ!!!"
	diff -w comp_testdir/testo1_0/cdf_extensional.P comp_outdir/testo1/cdf_extensional.P  
fi

status=0
diff -w comp_testdir/testo1_0/cdf_intensional.P comp_outdir/testo1/cdf_intensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd1: comp_testdir/testo1/cdf_intensional.P tested"
else
	echo "compUpd1: comp_testdir/testo1/cdf_intensional.P differ!!!"
	diff -w comp_testdir/testo1_0/cdf_intensional.P comp_outdir/testo1/cdf_intensional.P  
fi

