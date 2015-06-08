#! /bin/sh

EMU=$1

# print out differences.
if test -f ${FILE}_new; then
	rm -f ${FILE}_new
fi
    
echo "--------------------------------------------------------------------"
echo "Create Second Copy"

$EMU << EOF

[compSecCpy].

create_second_copy.

EOF

echo "--------------------------------------------------------------------"
echo "Third Comp Update Test (unioning)"

$EMU << EOF

[compUpd3].

testreturns.

EOF

#-----------------------
# print out differences after reloading and updating in place.  test_int isn't 
# updated so no need to compare.
#-----------------------

# first do the component: test.
status=0
diff -w comp_testdir2/test/cdf_extensional.P comp_outdir2/test/cdf_extensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd3: comp_testdir2/test/cdf_extensional.P tested"
else
	echo "compUpd3: comp_testdir2/test/cdf_extensional.P differ!!!"
#	diff -w comp_testdir2/test_0/cdf_extensional.P comp_outdir2/test/cdf_extensional.P  
	diff -w comp_testdir2/test/cdf_extensional.P comp_outdir2/test/cdf_extensional.P  
fi

status=0
diff -w comp_testdir2/test/cdf_intensional.P comp_outdir2/test/cdf_intensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd3: comp_testdir2/test/cdf_intensional.P tested"
else
	echo "compUpd3: comp_testdir2/test/cdf_intensional.P differ!!!"
	diff -w comp_testdir2/test_0/cdf_intensional.P comp_outdir2/test/cdf_intensional.P  
fi

#----------------------------------------------------------------------
# next do the component testo1
status=0
diff -w comp_testdir2/testo1/cdf_extensional.P comp_outdir2/testo1/cdf_extensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd3: comp_testdir2/testo1/cdf_extensional.P tested"
else
	echo "compUpd3: comp_testdir2/testo1/cdf_extensional.P differ!!!"
	diff -w comp_testdir2/testo1/cdf_extensional.P comp_outdir2/testo1/cdf_extensional.P  
fi

status=0
diff -w comp_testdir2/testo1/cdf_intensional.P comp_outdir2/testo1/cdf_intensional.P  || status=1
if test "$status" = 0 ; then 
	echo "compUpd3: comp_testdir2/testo1/cdf_intensional.P tested"
else
	echo "compUpd3: comp_testdir2/testo1/cdf_intensional.P differ!!!"
	diff -w comp_testdir2/testo1/cdf_intensional.P comp_outdir2/testo1/cdf_intensional.P  
fi

