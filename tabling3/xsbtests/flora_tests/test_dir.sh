#! /bin/sh

# Test files in one directory
#============================================================================
dir=`pwd`
dir=`basename $dir`
echo "-------------------------------------------------------"
echo "--- Running $dir/test_dir.sh                       ---"
echo "-------------------------------------------------------"

FLORADIR=$1
options=$2

file_list=*.flr

# abp.flr does not work. Perhaps the program is wrong.
# btupdates.flr is loaded using btupdates_load.flr
exclude_list="abp.flr btupdates.flr compile_control.flr"

flora_command="chatterbox(off). test. flEnd."

# Test if element is a member of exclude list
# $1 - element
# $2 - exclude list
member ()
{
    for elt in $2 ; do
	if test "$1" = "$elt" ; then
	    return 0
	fi
    done
    return 1
}

OBJEXT=.xwam

# Remove the Prolog and object files to make sure we are using the 
# latest compiled sources
rm -f *.P *${OBJEXT} *.fld *.fdb
rm -f programs/*.P programs/*${OBJEXT} programs/*.fld programs/*.fdb
rm -f ../datafiles/*.P    ../datafiles/*${OBJEXT} \
          ../datafiles/*.fld  ../datafiles/*.fdb


# run the tests
for file in $file_list ; do
    if member $file "$exclude_list"; then
	continue
    fi
    prog=`basename $file .flr`
    touch $file
    ../test_one_file.sh $FLORADIR/runflora $prog "$flora_command"
done

