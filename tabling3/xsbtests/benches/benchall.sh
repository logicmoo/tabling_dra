#! /bin/sh 

## File:      testall.sh
## Author(s): Freire, Kifer, Swift
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
## $Id: benchall.sh,v 1.2 2000-02-10 17:49:05 unova Exp $
## 
##


echo "-------------------------------------------------------"
echo "--- Running benchall.sh                             ---"
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
     *-only*)
	    shift
	    only_tests=$1
	    shift
	    ;;
      *)
	    break
	    ;;
    esac
done

if test -z "$1" -o $# -gt 1; then
  echo "Usage: benchall.sh [-opts opts] [-exclude \"test_list\"] executable"
  echo "where: opts       -- options to pass to XSB executable"
  echo "       bench_list  -- quoted, space-separated list of benchess to NOT run"
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
default_benchlist="synth_benches xmc"

if test -z "$only_tests"; then
    benchlist=$default_benchlist
else
    benchlist=$only_tests
fi

LOG_FILE=/tmp/xsb_bench_log.$USER
NEW_LOG=$LOG_FILE-`date +"%y.%m.%d-%H:%M:%S"`

if [ -f $LOG_FILE ]
then mv $LOG_FILE $NEW_LOG
fi


# Run each test in $testlist except for the tests in $excluded_tests
for bnch in $benchlist ; do
  if member "$bnch" "$excluded_benchess" ; then
    continue
  else
    cd $bnch
    if test -f core ; then
	rm -f core
    fi
    ./test.sh "$XEMU -e segfault_handler(warn). $options" $LOG_FILE
    cd ..
  fi
done
