#! /bin/sh

## File:      gctest.sh
## Author(s): Kostis Sagonas
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) K.U. Leuven, 1999
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
## $Id: gctest.sh,v 1.6 2000-02-15 07:45:09 bartkul Exp $
## 
##

# THIS SCRIPT EXPECTS TO BE RUN FROM THE TESTSUITE DIRECTORY

# $1 must be the path to the XSB installation directory

xsbdir=$1
testdir=`pwd`
logfile=/tmp/xsb_gc_log.`whoami`
lockfile=lock.gc

if test -z "$xsbdir" ; then
   echo "Arg 1 must be a path to the XSB installation directory"
   exit 1
elif test ! -d "$xsbdir" ; then
   echo "$xsbdir: XSB root directory doesn't exist or has wrong permissions"
   exit 1
fi

# Make sure to delete locks, if the user decides to abort the test
trap 'rm -fr $testdir/$lockfile $testdir/lock.test; exit 1' 1 2 15

if test -f "$testdir/$lockfile" ; then
 echo "./$lockfile exists. Remove it first"
 exit
else
 echo $$ > $lockfile
fi

#------------------------------------------------------------------

cd $xsbdir/build
echo "Configuring XSB with --enable-gc-test"
./configure --config-tag=chatgc --enable-gc-test >> $logfile
echo "Making XSB with --enable-gc-test"
./makexsb --config-tag=chatgc fast >> $logfile
./makexsb --config-tag=chatgc module >> $logfile
cd $testdir
./testsuite.sh -tag chatgc -exclude "sem_tests attv_tests sub_tests"  $xsbdir
./testsuite.sh -tag chatgc -opts "-g copying" \
		   -exclude "sub_tests sem_tests attv_tests ai_tests"  $xsbdir

cd $xsbdir/build
echo "Configuring XSB with --enable-local-scheduling --enable-gc-test"
./configure --config-tag=chatlocalgc --enable-local-scheduling --enable-gc-test >> $logfile
echo "Making XSB with --enable-local-scheduling --enable-gc-test"
./makexsb --config-tag=chatlocalgc fast >> $logfile
./makexsb --config-tag=chatlocalgc module >> $logfile
cd $testdir
./testsuite.sh -tag chatlocalgc -exclude "sem_tests attv_tests sub_tests"  $xsbdir
./testsuite.sh -tag chatlocalgc -opts "-g copying" \
		   -exclude "sem_tests ai_tests attv_tests sub_tests"  $xsbdir

#------------------------------------------------------------------

rm $testdir/$lockfile
