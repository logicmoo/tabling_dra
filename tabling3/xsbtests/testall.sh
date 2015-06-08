#! /bin/sh 

## File:      testall.sh
## Author(s): Probably Juliana Freire (rewritten by kifer)
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1996-1999
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id: testall.sh,v 1.24 2012-08-12 14:58:22 tswift Exp $
## 
##


echo "-------------------------------------------------------"
echo "--- Running testall.sh                              ---"
echo "-------------------------------------------------------"

while test 1=1
do
    case "$1" in
     *-opt*)
	    shift
	    options=$1
	    shift
	    ;;
     *-exclud*)
	    shift
	    excluded_tests=$1
	    shift
	    ;;
     *-add*)
	    shift
	    added_tests=$1
	    shift
	    ;;
     *-only*)
	    shift
	    only_tests=$1
	    shift
	    ;;
     *-valgrind*)
	    valgrind="true"
	    shift
	    ;;
      *)
	    break
	    ;;
    esac
done

if test -z "$1" -o $# -gt 1; then
  echo "Usage: testall.sh [-opts opts] [-exclude \"excl_list\"] [-add \"added_tests\"] [-only \"test-list\"] executable"
  echo "where: opts       -- options to pass to XSB executable"
  echo "       excl_list  -- quoted, space-separated list of tests to NOT run"
  echo "       add_list   -- list of additional tests to run"
  echo "       only_list  -- run only this list of tests"
  echo "       executable -- full path name of the XSB executable"
  exit
fi

XEMU=$1

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


# float_tests: don't pass. --mk
# regmatch_tests: don't pass on solaris
# what about compiler_tests?
default_testlist="basic_tests prolog_tests retract_tests \
	  table_tests ptq neg_tests sem_tests delay_tests \
	  wfs_tests ai_tests attv_tests constraint_tests \
          sub_tests io_tests regmatch_tests incremental_tests"

if test -z "$only_tests"; then
    testlist="$default_testlist $added_tests"
else
    testlist=$only_tests
fi

echo $testlist

# Run each test in $testlist except for the tests in $excluded_tests
for tst in $testlist ; do
  if member "$tst" "$excluded_tests" ; then
    continue
  else
    cd $tst
    if test -f core ; then
	rm -f core
    fi
    ./test.sh "$XEMU" "-e segfault_handler(warn). $options" $valgrind
    cd ..
  fi
done

# this screws up the parameter -only...
default_subsumptive_testlist="neg_tests wfs_tests delay_tests constraint_tests"
subtestlist=$default_subsumptive_testlist

for tst in $subtestlist ; do
  if member "${tst}_subsumption" "$excluded_tests" ; then
    continue
  else if member "$tst" "$testlist" ; then
    cd $tst
    if test -f core ; then
	rm -f core
    fi
    ./stest.sh "$XEMU" "-e segfault_handler(warn). -S $options"
    cd ..
    fi
  fi
done

# now test call abstraction
default_abstraction_testlist="basic_tests table_tests attv_tests constraint_tests"
abstestlist=$default_abstraction_testlist

for tst in $abstestlist ; do
    cd $tst
    if test -f core ; then
	rm -f core
    fi
    ./atest.sh "$XEMU"  "-e segfault_handler(warn). --max_subgoal_action a --max_subgoal_depth 4  $options" $valgrind
    cd ..
done
