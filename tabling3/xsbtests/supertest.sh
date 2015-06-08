#! /bin/sh

## File:      supertest.sh
## Author(s): Michael Kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999
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
## $Id: supertest.sh,v 1.16 2000-04-29 20:16:22 kifer Exp $
## 
##

# THIS SCRIPT EXPECTS TO BE RUN FROM THE TESTSUITE DIRECTORY

# $1 must be the path to the XSB installation directory

xsbdir=$1
testdir=`pwd`
logfile=/tmp/xsb_super_log.`whoami`
msg_file=/tmp/xsb_supertest_msg.$USER
res_file=/tmp/xsb_supertest_res.$USER
lockfile=lock.super
GREP="grep -i"

if test -z "$xsbdir" ; then
   echo "***XSB root directory hasn't been specified."
   xsbdir=$testdir/../XSB
   echo "***Assuming $xsbdir"
fi
if test ! -d "$xsbdir" ; then
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

echo ""
echo "***NOTE: XSB must be fully pre-built with default options"
echo ""
cd $xsbdir/build
echo "Configuring XSB with default options"
./configure > $logfile
echo "Making XSB with default options"
# Using the fast mode saves time by not building packages.
# Packages must already be pre-built for default options, 
# or else this first run would fail on regmatch_tests
./makexsb fast >> $logfile
./makexsb module >> $logfile
cd $testdir
./testsuite.sh $xsbdir

cd $xsbdir/build
echo "Configuring XSB with --enable-local-scheduling"
./configure --config-tag=localsched --enable-local-scheduling >> $logfile
echo "Making XSB with --enable-local-scheduling"
./makexsb --config-tag=localsched fast >> $logfile
./makexsb --config-tag=localsched module >> $logfile
cd $testdir
./testsuite.sh -tag localsched $xsbdir

cd $xsbdir/build
echo "Configuring XSB with --enable-slg-wam"
./configure --config-tag=slg-wam --enable-slg-wam >> $logfile
echo "Making XSB with --enable-slg-wam"
./makexsb --config-tag=slg-wam fast >> $logfile
./makexsb --config-tag=slg-wam module >> $logfile
cd $testdir
./testsuite.sh -tag slg-wam $xsbdir


cd $xsbdir/build
echo "Configuring XSB with --enable-slg-wam --enable-local-scheduling"
./configure --config-tag=localschedNslg-wam --enable-slg-wam --enable-local-scheduling >> $logfile
echo "Making XSB with --enable-slg-wam --enable-local-scheduling"
./makexsb --config-tag=localschedNslg-wam fast >> $logfile
./makexsb --config-tag=localschedNslg-wam module >> $logfile
cd $testdir
./testsuite.sh -tag localschedNslg-wam $xsbdir

rm $testdir/$lockfile

$GREP "Error:" $logfile >> $res_file

# -s tests if size > 0
if test -s $res_file; then
	cat $res_file
	echo "-----------------------------------------"
	echo "***FAILED supertest for $XEMU on $HOSTNAME"
        echo "***FAILED supertest for $XEMU on $HOSTNAME" > $msg_file
	echo "" >> $msg_file
	echo "    Summary of the problems:" >> $msg_file
	echo "" >> $msg_file
	cat $res_file >> $msg_file
	mail $USER < $msg_file
	rm -f $msg_file
fi

rm -f $res_file
